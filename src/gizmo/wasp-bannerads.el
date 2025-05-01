;;; wasp-bannerads --- Advertising and merchandising for elite finance -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-chat)

(defvar w/banner-ad-block nil)

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
      (if w/banner-ad-block
        (w/write "This advertisement was blocked by your adblocker")
        (let* ((paths (f-files (w/asset "bannerads")))
                (path (w/pick-random paths))
                (img (create-image path nil nil :max-width 555 :max-height 175)))
          (image-animate img nil t)
          (w/write (propertize "bannerad" 'display img)))))))

(defvar w/banner-ad-timer nil)
(defun w/run-banner-ad-timer ()
  "Run the banner ad timer."
  (when w/banner-ad-timer
    (cancel-timer w/banner-ad-timer))
  (w/render-banner-ad)
  (setq
    w/banner-ad-timer
    (run-with-timer 60 nil #'w/run-banner-ad-timer)))

(defun w/banner-ad-block ()
  "Toggle adblock."
  (setq w/banner-ad-block t)
  (w/render-banner-ad)
  (w/model-toggle-set "adblock")
  (run-with-timer 10 nil
    (lambda ()
      (setq w/banner-ad-block nil)
      (w/model-toggle-unset "adblock")
      (w/render-banner-ad)))
  nil)

(provide 'wasp-bannerads)
;;; wasp-bannerads.el ends here
