;;; wasp-soundboard --- On-stream soundboard -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)

(require 'f)

(defcustom w/sfx-play-process "wasp-sfx-play"
  "Name of process for playing audio with ffplay."
  :type '(string)
  :group 'wasp)

(defun w/sfx-glob (pat)
  "Find soundboard paths matching PAT."
  (let ((base (w/asset "soundboard")))
    (cond
      ((f-dir? (f-join base pat)) (f-entries (f-join base pat) #'f-file? t))
      (t (f-glob (s-concat pat "*") base)))))

(defun w/sfx-play (cands &optional filter)
  "Select and play a sound randomly from CANDS.
Optionally apply FILTER."
  (let ((path (w/pick-random cands)))
    (unless path (error "No matching clips"))
    (make-process
      :name w/sfx-play-process
      :buffer nil
      :command
      (print
      `( "ffplay" "-autoexit" "-nodisp"
         "-f" "lavfi"
         "-graph" ,(s-concat "amovie=" path (if (s-present? filter) (s-concat "," filter) ""))
         "dummy")))))

(cl-defstruct (w/sfx-state (:constructor w/make-sfx-state))
  cands
  stack
  filters)

(defun w/sfx-pop (st)
  "Pop an element from the stack of ST."
  (or (pop (w/sfx-state-stack st))
    (error "Stack underflow")))

(defun w/sfx-filter (st fil)
  "Add an FFMPEG audio filter given FIL in ST."
  (cond
    ((s-equals? fil "reverse") (push (cons "areverse" nil) (w/sfx-state-filters st)))
    ((s-equals? fil "tempo")
      (push (cons "atempo" (number-to-string (w/sfx-pop st)))
        (w/sfx-state-filters st)))
    (t
      (message "Unknown audio filter: %s" fil))))

(defun w/sfx-command (st cmd)
  "Evaluate CMD in the context of ST."
  (cond
    ((s-prefix? "!" cmd)
      (w/sfx-filter st (s-chop-prefix "!" cmd)))
    ((or (s-equals? cmd "0") (not (= 0 (string-to-number cmd))))
      (push (string-to-number cmd) (w/sfx-state-stack st)))
    (t
      (setf (w/sfx-state-cands st)
        (-concat (w/sfx-glob cmd) (w/sfx-state-cands st))))))

(defun w/sfx (cmds)
  "Evaluate the sound effect CMDS."
  (condition-case err
    (let ((st (w/make-sfx-state)))
      (--each (s-split " " cmds t)
        (message "Command: %s" it)
        (w/sfx-command st it))
      (w/sfx-play (w/sfx-state-cands st)
        (s-join ","
          (--map
            (s-concat (car it) (if (cdr it) (s-concat "=" (cdr it)) ""))
            (reverse (w/sfx-state-filters st)))))
      nil)
    (error err)))

(provide 'wasp-soundboard)
;;; wasp-soundboard.el ends here
