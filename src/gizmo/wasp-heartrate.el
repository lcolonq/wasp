;;; wasp-heartrate --- Heartbeat heartbreak it keeps on pounding -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-chat)

(defun w/heartrate ()
  "Get the streamer's heart rate."
  (* 100 (w/cpu-load)))

(defun w/heartrate-blood-pressure ()
  "Get the streamer's blood pressure."
  (format "%s/%s" (w/disk-usage "/") (w/disk-usage "/home")))

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

(defun w/heartrate-get-buffer ()
  "Return the heartrate buffer."
  (unless (get-buffer w/heartrate-buffer)
    (with-current-buffer (get-buffer-create w/heartrate-buffer)
      (w/heartrate-mode)))
  (get-buffer w/heartrate-buffer))

(defun w/heartrate-update ()
  "Render the heartrate buffer."
  (with-current-buffer (w/heartrate-get-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (w/write-line (format "%3d bpm" (w/heartrate)) 'w/heartrate-big)
      (w/write-line (format "blood pressure: %s" (w/heartrate-blood-pressure)) 'w/heartrate-small)
      (w/write (format "arbitrary counter: %s times" w/chat-bpm-count) 'w/heartrate-small))))
(add-hook 'w/gizmo-update-hook #'w/heartrate-update)

(provide 'wasp-heartrate)
;;; wasp-heartrate.el ends here
