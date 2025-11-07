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

(defun w/model-get-default-backgrounds (k)
  "Retrieve the background playlist and pass it to K."
  (w/db-get "modelbackgrounds"
    (lambda (res)
      (funcall k (if (s-present? res) (w/read-sexp res) nil)))))

(defun w/model-add-default-background (url)
  "Add URL to the background playlist."
  (w/model-get-default-backgrounds
    (lambda (cur)
      (w/db-set "modelbackgrounds" (format "%S" (cons url cur))))))

(defun w/model-frame-test ()
  "Submit a test frame for the new model."
  (let ((data
          (seq-mapcat #'byte-to-string (apply #'seq-concatenate 'list (seq-into (caddr (u/load-image-png "/home/llll/mrgreenbig.png")) 'list)) 'string)
          ;; (seq-mapcat #'byte-to-string (--mapcat (list 0 255 0) (-iota (* 64 64))) 'string)
          ))
    (w/pub '(avatar frame) (list (base64-encode-string data t)))))

(defvar w/model-palette-counter nil "Time to display model changes.")

(defun w/model-record-change ()
  "Record a change to the model in the counter."
  (setf w/model-palette-counter 300))

(defun w/model-reset ()
  "Reset the model palette."
  (interactive)
  (w/pub '(avatar reset))
  ;; (w/model-region-video "hair" "https://www.youtube.com/watch?v=PruiY9BJi84")
  (w/model-region-word "eyes" "EYES")
  (w/model-region-word "hair" "hair")
  (w/model-region-word "highlight" "highlight")
  ;; (w/model-region-color "hat" (color-values "#FF7518"))
  (w/model-region-word "hat" "ISPUMPKIN")
  (w/irish-reset)
  (w/model-get-default-backgrounds
    (lambda (bgs)
      (when bgs
        (w/model-region-video "hair" (w/pick-random bgs))))))

(defun w/model-toggle (toggle)
  "Toggle TOGGLE on model."
  (w/model-record-change)
  (w/pub '(avatar toggle) (list toggle)))

(defun w/model-toggle-set (toggle)
  "Set TOGGLE on model."
  (w/model-record-change)
  (w/pub '(avatar toggle set) (list toggle)))

(defun w/model-toggle-unset (toggle)
  "Unset TOGGLE on model."
  (w/model-record-change)
  (w/pub '(avatar toggle unset) (list toggle)))

(defun w/model-background-text (msg)
  "Change the background text of the model to MSG."
  (let* ((cleanmsg (s-trim (w/clean-string msg)))
         (encoded (w/encode-string cleanmsg)))
    (unless (s-blank? cleanmsg)
      (w/model-record-change)
      (w/pub '(avatar text) (list encoded)))))

(w/defstruct
 w/model-color-source
 type ;; 'color or 'twitch-emote or '7tv-emote or 'video-url
 value)

(defun w/model-string-to-color-source (s k)
  "Convert S to a color source and pass it to K."
  (w/twitch-get-emote
   s
   (lambda (emote)
     (let ((7tv-emote (w/twitch-get-7tv-emote s))
           (color (color-values s))
           (url (w/allowed-video-url s)))
       (funcall
        k
        (cond
         (url (w/make-model-color-source :type 'video-url :value s))
         (emote (w/make-model-color-source :type 'twitch-emote :value emote))
         (7tv-emote (w/make-model-color-source :type '7tv-emote :value 7tv-emote))
         (color (w/make-model-color-source :type 'color :value color))
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
  (cl-case (w/model-color-source-type cs)
    (color
     (w/model-region-color
      type
      (w/model-color-source-value cs)))
    (twitch-emote
     (w/model-region-image
      type
      (w/twitch-emote-path (w/model-color-source-value cs))))
    (7tv-emote
     (w/model-region-image
      type
      (w/twitch-7tv-emote-path (w/model-color-source-value cs))))
    (video-url
     (w/model-region-video
      type
      (w/model-color-source-value cs)))
    (t nil)))

(defun w/model-handle-redeem-region-swap (type)
  "Return a redeem callback for region swap of TYPE.
If the color is unspecified, use DEFCOLOR."
  (lambda (user inp)
    (let ((splinp (s-split-up-to " " (s-trim inp) 1))
          (auth (w/user-authorized)))
      (w/model-string-to-color-source
       (car splinp)
       (lambda (cs)
         (let ((text (if cs (cadr splinp) (s-join " " splinp))))
           (w/chat-write-event (format "%s changes my %s to %s" user type inp))
           (when cs
             (if (or auth
                     (not (eq 'video-url (w/model-color-source-type cs))))
                 (w/model-region-color-source type cs)
               (w/chat-write-event (format "%s is not authorized to play video, boost harder" user))))
           (when text
             (w/model-region-word type text))))))))

(defvar w/model-timer nil)
(defun w/model-run-timer ()
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
   (run-with-timer 1 nil #'w/model-run-timer)))

(defun w/test-length-prefixed (s)
  (let ((bytes (seq-into s 'list)))
    (-concat
      (seq-into (w/bus-binary-build-int32le (length bytes)) 'list)
      bytes)))
(defun w/test-background-drawing ()
  (-let [(w h pixels) (w/load-image-png "/home/llll/irish.png")]
    (w/binary-pub "background frame"
      (apply #'unibyte-string
        (-concat
          (w/test-length-prefixed "foobar") ;; tag
          (seq-into (w/bus-binary-build-int32le w) 'list)
          (seq-into (w/bus-binary-build-int32le h) 'list)
          (--mapcat
            (-concat it '(255))
            (seq-into pixels 'list)))))))

(provide 'wasp-model)
;;; wasp-model.el ends here
