;;; wasp-bannerads --- Advertising and merchandising for elite finance -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-chat)

(defcustom w/banner-ad-buffer "*wasp-banner-ad*"
  "Name of buffer used to display banner ad."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/banner-ad-mode special-mode "banner ad"
  "Major mode for displaying banner ads."
  :group 'w
  (setq-local cursor-type nil))

(defun w/get-banner-ad-buffer ()
  "Return the banner ad buffer."
  (unless (get-buffer w/banner-ad-buffer)
    (with-current-buffer (get-buffer-create w/banner-ad-buffer)
      (w/banner-ad-mode)))
  (get-buffer w/banner-ad-buffer))

(defun w/render-banner-ad ()
  "Render the banner ad buffer."
  (with-current-buffer (w/get-banner-ad-buffer)
    (setq-local cursor-type nil)
    (let* ((inhibit-read-only t))
      (erase-buffer)
      (let* ((paths (f-files (w/asset "bannerads")))
              (path (w/pick-random paths)))
        (w/write (propertize "bannerad" 'display (create-image path nil nil :max-width 555 :max-height 175)))))))

(defvar w/banner-ad-timer nil)
(defun w/run-banner-ad-timer ()
  "Run the banner ad timer."
  (when w/banner-ad-timer
    (cancel-timer w/banner-ad-timer))
  (w/render-banner-ad)
  (setq
   w/banner-ad-timer
   (run-with-timer 60 nil #'w/run-banner-ad-timer)))
(w/run-banner-ad-timer)

(provide 'wasp-bannerads)
;;; wasp-bannerads.el ends here
