;;; wasp-heartrate --- Heartbeat heartbreak it keeps on pounding -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-chat)

(defun w/get-load ()
  "Get the current CPU load."
  (let ((res (shell-command-to-string "uptime")))
    (string-to-number (s-trim (car (s-split "," (cadr (s-split "load average:" res))))))))

(defun w/get-disk-usage (disk)
  "Get the current usage percent for DISK"
  (let ((res (shell-command-to-string (format "df %s" disk))))
    (string-to-number (s-chop-suffix "%" (nth 4 (s-split " " (cadr (s-lines res)) t))))))

(defun w/get-heartrate ()
  "Get the streamer's heart rate."
  (* 100 (w/get-load)))

(defun w/get-blood-pressure ()
  "Get the streamer's blood pressure."
  (format "%s/%s" (w/get-disk-usage "/") (w/get-disk-usage "/home")))

(defface w/heartrate-big
  '((t
     :foreground "white"
     :height 700
     ))
  "Face for big heartrate."
  :group 'wasp)

(defface w/heartrate-small
  '((t
     :foreground "white"
     ))
  "Face for small heartrate disclaimer."
  :group 'wasp)

(defcustom w/heartrate-buffer "*wasp-heartrate*"
  "Name of buffer used to display heartrate."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/heartrate-mode special-mode "heartrate"
  "Major mode for displaying heartrate."
  :group 'w
  (setq-local cursor-type nil))

(defun w/get-heartrate-buffer ()
  "Return the heartrate buffer."
  (unless (get-buffer w/heartrate-buffer)
    (with-current-buffer (get-buffer-create w/heartrate-buffer)
      (w/heartrate-mode)))
  (get-buffer w/heartrate-buffer))

(defun w/render-heartrate ()
  "Render the heartrate buffer."
  (with-current-buffer (w/get-heartrate-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (w/write-line (format "%3d bpm" (w/get-heartrate)) 'w/heartrate-big)
      (w/write-line (format "blood pressure: %s" (w/get-blood-pressure)) 'w/heartrate-small)
      (w/write (format "arbitrary counter: %s times" w/chat-bpm-count) 'w/heartrate-small))))

(defvar w/heartrate-timer nil)
(defun w/run-heartrate-timer ()
  "Run the heartrate timer."
  (when w/heartrate-timer
    (cancel-timer w/heartrate-timer))
  (w/render-heartrate)
  (setq
   w/heartrate-timer
   (run-with-timer 1 nil #'w/run-heartrate-timer)))

(provide 'wasp-heartrate)
;;; wasp-heartrate.el ends here
