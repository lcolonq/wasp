;;; wasp-model --- Model controls -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)
(require 'wasp-utils)
(require 'wasp-bus)
(require 'wasp-twitch)
(require 'wasp-user)

(defun w/color-value-to-html-code (cval)
  "Convert color value CVAL to an HTML color code."
  (and
   cval
   (format
    "#%02x%02x%02x"
    (truncate (* 255 (/ (car cval) 65535.0)))
    (truncate (* 255 (/ (cadr cval) 65535.0)))
    (truncate (* 255 (/ (caddr cval) 65535.0)))
    )))

(defun w/color-to-html-code (cname)
  "Convert color name CNAME to an HTML color code."
  (w/color-value-to-html-code (color-values cname)))

(defvar w/model-palette-counter nil "Time to display model changes.")

(defun w/model-record-change ()
  "Record a change to the model in the counter."
  (setf w/model-palette-counter 300))

(defun w/model-reset ()
  "Reset the model palette."
  (interactive)
  (w/pub '(avatar reset)))

(defun w/model-toggle (toggle)
  "Toggle TOGGLE on model."
  (w/model-record-change)
  (w/pub '(avatar toggle) (list toggle)))

(defun w/model-background-text (msg)
  "Change the background text of the model to MSG."
  (let* ((cleanmsg (s-trim (w/clean-string msg)))
         (encoded (w/encode-string cleanmsg)))
    (unless (s-blank? cleanmsg)
      (w/model-record-change)
      (w/pub '(avatar text) (list encoded)))))

(w/defstruct
 w/color-source
 type ;; 'color or 'twitch-emote or '7tv-emote or 'video-url
 value)

(defun w/string-to-color-source (s k)
  "Convert S to a color source and pass it to K."
  (w/twitch-get-emote
   s
   (lambda (emote)
     (let ((7tv-emote (w/twitch-get-7tv-emote s))
           (color (color-values s))
           (url
            (-contains?
             '("www.youtube.com" "youtube.com" "youtu.be" "www.twitch.tv" "twitch.tv" "clips.twitch.tv")
             (url-host (url-generic-parse-url s)))))
       (funcall
        k
        (cond
         (url (w/make-color-source :type 'video-url :value s))
         (emote (w/make-color-source :type 'twitch-emote :value emote))
         (7tv-emote (w/make-color-source :type '7tv-emote :value 7tv-emote))
         (color (w/make-color-source :type 'color :value color))
         (t nil)))))))

(defun w/model-region-word (type msg)
  "Change the model region TYPE to MSG."
  (let* ((cleanmsg (s-trim (w/clean-string msg)))
         (encodedmsg (w/encode-string cleanmsg)))
    (unless (s-blank? cleanmsg)
      (w/model-record-change)
      (w/pub '(avatar palette word) (list type encodedmsg)))))

(defun w/model-region-color (type color)
  "Change the model region TYPE to COLOR."
  (let* ((encodedcol (w/encode-string (w/color-value-to-html-code color))))
    (w/model-record-change)
    (w/pub '(avatar palette color) (list type encodedcol))))

(defun w/model-region-image (type path)
  "Change the model region TYPE to an image at PATH."
  (interactive)
  (let* ((cleanpath (s-trim (w/clean-string path)))
         (encodedpath (w/encode-string cleanpath)))
    (unless (s-blank? cleanpath)
      (w/model-record-change)
      (w/pub '(avatar palette image) (list type encodedpath)))))

(defun w/model-region-video (type url)
  "Change the model region TYPE to a video at URL."
  (interactive)
  (let* ((cleanurl (s-trim (w/clean-string url)))
         (encodedurl (w/encode-string cleanurl)))
    (unless (s-blank? cleanurl)
      (w/model-record-change)
      (w/pub '(avatar palette video) (list type encodedurl)))))

(defun w/model-region-user-avatar (type user)
  "Change the model region TYPE to USER's avatar."
  (w/twitch-get-user-avatar
   user
   (lambda ()
     (when (f-exists? (w/twitch-user-avatar-path user))
       (w/model-region-image type (w/twitch-user-avatar-path user))))))

(defun w/model-region-color-source (type cs)
  "Change the model region TYPE to CS."
  (cl-case (w/color-source-type cs)
    (color
     (w/model-region-color
      type
      (w/color-source-value cs)))
    (twitch-emote
     (w/model-region-image
      type
      (w/twitch-emote-path (w/color-source-value cs))))
    (7tv-emote
     (w/model-region-image
      type
      (w/twitch-7tv-emote-path (w/color-source-value cs))))
    (video-url
     (w/model-region-video
      type
      (w/color-source-value cs)))
    (t nil)))

(defun w/handle-redeem-region-swap (type)
  "Return a redeem callback for region swap of TYPE.
If the color is unspecified, use DEFCOLOR."
  (lambda (user inp)
    (let ((splinp (s-split-up-to " " (s-trim inp) 1))
          (auth (w/user-authorized)))
      (w/string-to-color-source
       (car splinp)
       (lambda (cs)
         (let ((text (if cs (cadr splinp) (s-join " " splinp))))
           (w/write-chat-event (format "%s changes my %s to %s" user type inp))
           (when cs
             (if (or auth
                     (not (eq 'video-url (w/color-source-type cs))))
                 (w/model-region-color-source type cs)
               (w/write-chat-event (format "%s is not authorized to play video, boost harder" user))))
           (when text
             (w/model-region-word type text))))))))

(defvar w/model-timer nil)
(defun w/run-model-timer ()
  "Run the model timer."
  (when w/model-timer
    (cancel-timer w/model-timer))

  (when w/model-palette-counter
    (cl-decf w/model-palette-counter)
    (when (<= w/model-palette-counter 0)
      (setf w/model-palette-counter nil)
      (w/model-reset)
      ))

  (setq
   w/model-timer
   (run-with-timer 1 nil #'w/run-model-timer)))

(provide 'wasp-model)
;;; wasp-model.el ends here
