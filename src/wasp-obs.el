;;; wasp-obs --- OBS controls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'wasp-utils)
(require 'wasp-bus)
(require 'wasp-model)

(defun w/obs-toggle-modclonk ()
  "Toggle the MODCLONK panel."
  (w/pub '(monitor obs toggle) (list "MODCLONK" "MODCLONK Chibi")))

(defun w/obs-toggle-live-reaction ()
  "Toggle the Live LCOLONQ Reaction panel."
  (w/pub '(monitor obs toggle) (list "Live LCOLONQ Reaction" "Live Reaction")))

(defun w/obs-toggle-live-friend-reaction ()
  "Toggle the Live Friend Reaction panel."
  (w/pub '(monitor obs toggle) (list "Live Friend Reaction" "Live Friend Reaction Group")))

(defun w/obs-toggle-thug-life ()
  "Toggle the Thug Life overlay."
  (w/pub '(monitor obs toggle) (list "Thug Life" "Thug Life Video")))

(defun w/obs-toggle-intj-stare ()
  "Toggle the INTJ Stare overlay."
  (w/pub '(monitor obs toggle) (list "INTJ" "INTJ Image")))

(defun w/obs-toggle-critical-hit ()
  "Toggle the Critical Hit overlay."
  (w/pub '(monitor obs toggle) (list "Critical Hit Wrapper" "Critical Hit")))

(defun w/obs-toggle-vhs ()
  "Toggle the VHS overlay."
  (w/pub '(monitor obs toggle) (list "VHS" "VHS Group")))

(defun w/obs-toggle-saiyan ()
  "Toggle the Super Saiyan overlay."
  (w/pub '(monitor obs toggle) (list "Saiyan" "Saiyan Video")))

(defun w/obs-toggle-persona4 ()
  "Toggle the Persona 4 dialogue box."
  (w/pub '(monitor obs toggle) (list "Persona 4" "Persona 4 Background")))

(defun w/obs-toggle-explosion ()
  "Toggle the explosion effect."
  (w/pub '(monitor obs toggle) (list "Explosion" "Explosion Video")))

(defun w/obs-toggle-total-clarity ()
  "Toggle the total clarity effect."
  (w/pub '(monitor obs toggle) (list "Total Clarity" "Total Clarity Video"))
  (w/pub '(monitor obs toggle) (list "Main" "Mic")))

(defun w/obs-toggle-activate-nixos ()
  "Toggle the explosion effect."
  (w/pub '(monitor obs toggle) (list "Main" "Activate NixOS")))

(defun w/obs-set-clickbait-text (msg)
  "Change the clickbait text to MSG."
  (w/pub '(monitor obs set-text) (list "Red Arrow Text" (w/encode-string (s-trim msg)))))

(defun w/obs-toggle-clickbait (&optional msg)
  "Toggle the clickbait arrow.
Optionally, change text to MSG."
  (when msg
    (w/obs-set-clickbait-text msg))
  (w/pub '(monitor obs toggle) (list "Red Arrow" "Red Arrow Group")))

(defun w/obs-toggle-chase-dreams ()
  "Toggle the Chasing Dreams effect."
  (w/pub '(monitor obs toggle) (list "Chasing Dreams" "Dreams")))

(defun w/obs-toggle-brazil ()
  "Toggle the Brazilian flag."
  (w/pub '(monitor obs toggle) (list "Main" "Brazil")))

(defun w/obs-toggle-japan ()
  "Toggle the Japanese flag."
  (w/pub '(monitor obs toggle) (list "Main" "Japan")))

(defun w/obs-set-debate-topic-text (msg)
  "Change the debate topic text to MSG."
  (w/pub '(monitor obs set-text) (list "Debate Topic" (w/encode-string (s-trim msg)))))

(defun w/obs-toggle-debate-topic ()
  "Toggle the debate topic."
  (w/pub '(monitor obs toggle) (list "Main" "Debate Topic")))

(defun w/obs-toggle-spatiotemporal-clarity ()
  "Toggle the shader clarity effect."
  (w/model-toggle "shaderclarity")
  (w/pub '(monitor obs toggle) (list "Main" "Mic")))

(w/defstruct
 w/obs-toggle
 toggle
 reset
 timer)

(defun w/obs-activate-toggle-helper (toggle &rest args)
  "Pass ARGS to the callback for TOGGLE and start its timer."
  (unless (w/obs-toggle-timer toggle)
    (apply (w/obs-toggle-toggle toggle) args))
  (setf (w/obs-toggle-timer toggle) (w/obs-toggle-reset toggle)))

(defvar w/obs-toggles
  (list
    (cons 'modclonk (w/make-obs-toggle :toggle #'w/obs-toggle-modclonk :reset 11))
    (cons 'live-reaction (w/make-obs-toggle :toggle #'w/obs-toggle-live-reaction :reset 17))
    (cons 'live-friend-reaction (w/make-obs-toggle :toggle #'w/obs-toggle-live-friend-reaction :reset 17))
    (cons 'thug-life (w/make-obs-toggle :toggle #'w/obs-toggle-thug-life :reset 17))
    (cons 'intj-stare (w/make-obs-toggle :toggle #'w/obs-toggle-intj-stare :reset 17))
    (cons 'critical-hit (w/make-obs-toggle :toggle #'w/obs-toggle-critical-hit :reset 3))
    (cons 'clickbait (w/make-obs-toggle :toggle #'w/obs-toggle-clickbait :reset 31))
    (cons 'chase-dreams (w/make-obs-toggle :toggle #'w/obs-toggle-chase-dreams :reset 31))
    (cons 'total-clarity (w/make-obs-toggle :toggle #'w/obs-toggle-total-clarity :reset 10))
    (cons 'activate-nixos (w/make-obs-toggle :toggle #'w/obs-toggle-activate-nixos :reset 31))
    (cons 'pharaohs-curse (w/make-obs-toggle :toggle (lambda () (w/model-toggle "sand")) :reset 20))
    (cons 'spatiotemporal-clarity (w/make-obs-toggle :toggle #'w/obs-toggle-spatiotemporal-clarity :reset 7))
    ))

(defun w/obs-activate-toggle (tnm &rest args)
  "Pass ARGS to the callback for toggle symbol TNM and start its timer."
  (when-let ((toggle (alist-get tnm w/obs-toggles)))
    (apply #'w/obs-activate-toggle-helper toggle args)))

(defun w/obs-handle-toggles ()
  "Process all OBS toggle timers."
  (--each w/obs-toggles
    (when (w/obs-toggle-timer (cdr it))
      (cl-decf (w/obs-toggle-timer (cdr it)))
      (when (<= (w/obs-toggle-timer (cdr it)) 0)
        (setf (w/obs-toggle-timer (cdr it)) nil)
        (funcall (w/obs-toggle-toggle (cdr it)))))
    ))

(defvar w/obs-timer nil)
(defun w/run-obs-timer ()
  "Run the obs timer."
  (when w/obs-timer
    (cancel-timer w/obs-timer))
  (w/obs-handle-toggles)
  (setq
   w/obs-timer
   (run-with-timer 1 nil #'w/run-obs-timer)))

(provide 'wasp-obs)
;;; wasp-obs.el ends here
