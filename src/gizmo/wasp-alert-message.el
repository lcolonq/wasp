;;; wasp-alert-message --- Advertising message -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-chat)

(defconst
  w/alert-message-phrases
  (list
    "hi :3"
    "Chat seems active. Considerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
    "Witscord The Game 2025"
    ))

(defcustom w/alert-message-buffer "*wasp-alert-message*"
  "Name of buffer used to display alert message."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/alert-message-mode special-mode "ALERT ALERT ALERT"
  "Major mode for displaying alert message."
  :group 'w
  (setq-local cursor-type nil))

(defun w/alert-message-get-buffer ()
  "Return the alert message buffer."
  (unless (get-buffer w/alert-message-buffer)
    (with-current-buffer (get-buffer-create w/alert-message-buffer)
      (w/alert-message-mode)))
  (get-buffer w/alert-message-buffer))

(defun w/alert-message-update ()
  "Render the heartrate buffer."
  (with-current-buffer (w/alert-message-get-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (w/write (w/pick-random w/alert-message-phrases)))))
(add-hook 'w/gizmo-update-hook #'w/alert-message-update)

(provide 'wasp-alert-message)
;;; wasp-alert-message.el ends here
