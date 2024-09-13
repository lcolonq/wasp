;;; wasp-chatsummary --- Stealing a YouTube feature -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-ai)
(require 'wasp-friend)

(defcustom w/chatsummary-buffer "*wasp-chatsummary*"
  "Name of buffer used to display chat summary."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/chatsummary-mode special-mode "Chat Summary"
  "Major mode for displaying chat summary."
  :group 'w
  (setq-local cursor-type nil)
  (visual-line-mode))

(defun w/chatsummary-get-buffer ()
  "Return the chatsummary buffer."
  (unless (get-buffer w/chatsummary-buffer)
    (with-current-buffer (get-buffer-create w/chatsummary-buffer)
      (w/chatsummary-mode)))
  (get-buffer w/chatsummary-buffer))

(defun w/update-chatsummary ()
  "Update the chat summary."
  (w/ai
   (w/friend-journalism-input)
   (lambda (d)
     (when-let* ((d)
                 (resp (s-trim d)))
       (with-current-buffer (w/chatsummary-get-buffer)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (w/write-line "Chat summary" 'bold)
           (w/write-line resp)))))
   "Given a list of recent YouTube chatter activity, produce a summary of the topics discussed. The summary should be very short, maximum two sentences total. Do not introduce yourself. Simply provide a short summary of the chat. Do not mention specific names of chatters. Keep it succinct. Do not mention that you are summarizing YouTube activity. Be laconic."))

(defvar w/chatsummary-timer nil)
(defun w/run-chatsummary-timer ()
  "Run the chat summary timer."
  (when w/chatsummary-timer
    (cancel-timer w/chatsummary-timer))
  (w/update-chatsummary)
  (setq
   w/chatsummary-timer
   (run-with-timer 120 nil #'w/run-chatsummary-timer)))

(defun w/start-chatsummary ()
  "Enable fake chatters."
  (interactive)
  (w/run-chatsummary-timer))

(defun w/stop-chatsummary ()
  "Disable fake chatters."
  (interactive)
  (cancel-timer w/chatsummary-timer)
  (setq w/chatsummary-timer nil))

(provide 'wasp-chatsummary)
;;; wasp-chatsummary.el ends here
