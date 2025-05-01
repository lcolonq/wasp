;;; wasp-overlay --- Fullscreen overlay -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-bus)

(defun w/overlay-shader (user shader)
  "Set the overlay shader to SHADER by USER."
  (w/pub '(avatar overlay shader)
    (list (w/encode-string user) (w/encode-string shader))))

(defun w/overlay-chat (msg)
  "Update the overlay about chat MSG."
  (w/pub '(avatar overlay chat)
    (list
      (w/encode-string (w/. user msg))
      (w/encode-string (w/. text msg))
      (format "%s" (w/unix-time))
      (format "%s" (or (w/. biblicality msg) 0.0)))))

(defun w/overlay-muzak (user song)
  "Update the overlay about Muzak SONG played by USER."
  (ignore song)
  (w/pub '(avatar overlay muzak) (list (w/encode-string user))))

(defun w/overlay-muzak-clear ()
  "Tell the overlay that there is no Muzak song playing."
  (w/pub '(avatar overlay muzak clear) (list)))

(defvar w/overlay-last-cursor nil)
(defun w/overlay-update-cursor ()
  "Inform the overlay about the current cursor position."
  (when (and (process-live-p (get-process w/bus-process)))
    (when-let* ((pos (window-absolute-pixel-position)))
      (when (not (equal pos w/overlay-last-cursor))
        (setf w/overlay-last-cursor pos)
        (w/pub '(avatar overlay cursor) (list (car pos) (cdr pos)))))))
(add-hook 'post-command-hook #'w/overlay-update-cursor)

(defun w/overlay-emacs ()
  "Update the overlay with miscellaneous data from Emacs."
  (w/pub '(avatar overlay emacs)
    (list
      (w/get-heartrate)
      )))

(provide 'wasp-overlay)
;;; wasp-overlay.el ends here
