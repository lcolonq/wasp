;;; wasp-bless --- The Blessing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'json)
(require 'wasp-chat)
(require 'wasp-model)

(defcustom w/bless-buffer " *wasp-bless*"
  "Name of buffer used to store Bless output."
  :type '(string)
  :group 'wasp)

(defun w/bless-error (e)
  "Report an error E."
  (message (alist-get 'message e)))

(defun w/bless-parse-value (j)
  "Construct an Emacs Lisp value representation of the value J."
  (let ((tag (alist-get 'tag j)))
    (cond
     ((s-equals? tag "ValueInteger") (alist-get 'contents j))
     ((s-equals? tag "ValueArray")
      (-map #'w/bless-parse-value (alist-get 'contents j)))
     (t (message "Unknown Bless tag: %s" tag)))))

(defun w/bless-parse-effect (j)
  "Construct an Emacs Lisp value representation of the effect J."
  (let ((tag (alist-get 'tag j))
        (c (alist-get 'contents j)))
  (cond
   ((s-equals? tag "EffectPrint") `(print ,(w/bless-parse-value c)))
   ((s-equals? tag "EffectPrintBackwards") `(print-backwards ,(w/bless-parse-value c)))
   ((s-equals? tag "EffectSoundboard") `(soundboard ,(w/bless-parse-value c)))
   ((s-equals? tag "EffectModelToggle") `(model-toggle ,(w/bless-parse-value c)))
   )))

(defun w/bless-parse-stack (j)
  "Construct an Emacs Lisp value representation of the stack J."
  (-map #'w/bless-parse-value j))

(defun w/bless-parse-effects (j)
  "Construct an Emacs Lisp value representation of the effects J."
  (-map #'w/bless-parse-effect j))

(defun w/bless-apply-effect (e)
  "Apply the list of side effects E."
  (cl-case (car e)
    (print (w/write-chat-event (format "%s" (cadr e))))
    (print-backwards (w/write-chat-event (reverse (format "%s" (cadr e)))))
    (soundboard (soundboard//play-clip (cadr e)))
    (model-toggle (w/model-toggle (cadr e)))
    (t (message "Unknown effect tag: %s" (car e)))))

(defun w/bless-eval (str k &optional fuel)
  "Bless STR according to the nature of the blessing.
Pass the result to K.
Optionally limit evaluation to FUEL steps."
  (let ((buf (generate-new-buffer w/bless-buffer)))
    (with-current-buffer buf
      (erase-buffer))
    (make-process
     :name "wasp-bless-eval"
     :buffer buf
     :command `("bless" "-j" "eval" ,@(if fuel (list "--fuel" (number-to-string fuel)) nil) ,str)
     :sentinel
     (lambda (_ _)
       (let* ((s (with-current-buffer buf (buffer-string)))
              (j (json-read-from-string s))
              (status (alist-get 'status j)))
         (kill-buffer buf)
         (if (s-equals? status "success")
             (funcall
              k
              (cons
               (w/bless-parse-stack (alist-get 'stack (alist-get 'data j)))
               (w/bless-parse-effects (alist-get 'effects (alist-get 'data j)))))
           (w/bless-error (alist-get 'data j))))))))

(defun w/bless (str &optional fuel)
  "Run the Bless program STR and apply its side effects.
Optionally limit evaluation to FUEL steps."
  (w/bless-eval
   str
   (lambda (res)
     (--each (cdr res)
       (w/bless-apply-effect it)))
   fuel))

(provide 'wasp-bless)
;;; wasp-bless.el ends here
