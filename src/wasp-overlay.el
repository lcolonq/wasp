;;; wasp-overlay --- Superterranean Animism Overlay -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-bus)
(require 'wasp-prod)

(add-to-list 'load-path (f-canonical "~/src/animism/"))
(require 'bulletml)

;; src,Xway,fire^쾠㘽쾷ㅗ껋㾨먝乲뛏屚w㮇㸩멽ꝼ쓋♫릫m떫쓏䓳⩺䮻1㨧퉝ƭ쓐ǥᓌ㒵ᒄ먽乳俴㢈쿗峫ฐꙢ왽욍투ㅖ㨧슝呑㟪䁓Ɂy䏧呠3먫슎呒3⧘ɍy욋ᕈ00먫⇳
;; this one is broken fix it -ellg, probably

(defconst w/overlay-spellcard-names
  '("Joel" "Pemis" "JoelTeachingHisSonJolHowToSpinWhileWideBorisPassesBy" "bugSegz"
    "widepeepoMASTURBATION77769420GANGSHITNOMOREFORTNITE19DOLLERFORTNITECARD"
    "Machine Made Of Fire, Heart Made Of Doves"
    "Dream Seal" "Evil-Sealing Circle" "Dream Seal -Spread-" "Dream Seal -Concentrate-"
    "Duplex Barrier" "Dream Orb" "Omnidirectional Oni-Binding Circle" "Yin-Yang Treasured Orb"
    "Yin-Yang Kishin Orb" "Dream Orb String" "Yin-Yang Scattering" "Exorcising Border"
    "Yin-Yang King Piece" "Illusionary Moon" "Flying Mysterious Shrine Maiden"
    "Dream Seal -Blink-" "Great Duplex Barrier" "Dream Seal -Worn-" "Dream Seal -Marred-"
    "Dream Seal -The Point Of The Mask-"
    ))

(defvar w/overlay-barrage-active nil)
(defvar w/overlay-last-cursor nil)
(defun w/overlay-update-cursor ()
  "Inform the overlay about the current cursor position."
  (when (and w/overlay-barrage-active (process-live-p (get-process w/bus-process)))
    (when-let ((pos (window-absolute-pixel-position)))
      (when (not (equal pos w/overlay-last-cursor))
        (setf w/overlay-last-cursor pos)
        (w/pub '(overlay cursor) (list (- (car pos) 1920) (cdr pos)))))))
(add-hook 'post-command-hook #'w/overlay-update-cursor)

(defun w/overlay-start-barrage (bml)
  "Start a barrage on the overlay using the BulletML source string BML."
  (w/pub '(overlay barrage start) (list (w/encode-string bml))))

(defun w/overlay-decode-shorthand-bml (s k)
  "Decode the shorthand BulletML string S.
Pass the resulting BulletML XML string to K."
  (w/write-log s)
  (w/prod-get-raw
   (format "/api/yamame?input=%s" (url-encode-url s))
   (lambda (data)
     (if-let* ((bml (bml/parse-string data))
               (b (bml/initialize bml))
               ((bml/barrage-toplevel b)))
         (progn
           (funcall k data))
       (w/write-chat-event "That spell card is too powerful... ")))))

(provide 'wasp-overlay)
;;; wasp-overlay.el ends here
