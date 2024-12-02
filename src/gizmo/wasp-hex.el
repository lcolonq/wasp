;;; wasp-hex --- Hex a digital being -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-chat)
(require 'wasp-ai)
(require 'wasp-audio)
(require 'cl-lib)
(require 'ht)
(require 's)
(require 'rx)

(defconst
  w/hex-types
  '(("DIGITAL" . hexadigital)
    ("ESREVER" . reverse)
    ("VANYAR" . quenya)
    ("LEONDIS" . emoji)
    ("KOBY" . clone)
    ("BIGFOOT" . hair)
    ("ALTMAN" . unbiblical)
    ("DECIBEL" . allcaps)
    ("PIQUANT" . mild)
    ("PORCINE" . piglatin)
    ("PYTHON" . oldeenglishe)
    ("MANIAC" . pokemon)
    ("ELBERETH" . counterspell)
    ("ESUNA" . decurse)
    ))

(defconst w/hex-users (ht-create 'equal))
(defconst w/hex-pokemon (w/read-sexp (w/slurp (w/asset "palcries/pokemon.eld"))))

(w/defstruct
 w/hex
 type
 caster
 (timer 0)
 data)

(defun w/hex-get (user)
  "Return the active hexes for USER."
  (ht-get w/hex-users (s-downcase user) nil))

(defun w/hex-add (user hex)
  "Add HEX to the active hexes for USER."
  (let* ((key (s-downcase user))
         (cur (ht-get w/hex-users key)))
    (unless (> (length cur) 10)
      (cl-case (w/hex-type hex)
        (decurse
         (ht-set! w/hex-users key nil))
        (otherwise
         (if-let ((defender (--find (eq 'counterspell (w/hex-type it)) cur)))
             (w/write-chat-event (format "%s counterspelled %s's hex on %s!" (w/hex-caster defender) (w/hex-caster hex) user))
           (ht-set! w/hex-users key (cons hex cur))))))))

(defun w/hex-clear (user)
  "Decurse USER."
  (ht-set! w/hex-users (s-downcase user) nil))

(defun w/hex (user caster type)
  "Record that CASTER cast a hex of TYPE on USER."
  (w/hex-add
   user
   (w/make-hex
    :type type
    :caster caster
    :timer 10
    :data
    (cl-case type
      (pokemon (random (length w/hex-pokemon)))
      (t nil)))))

(defun w/hex-pokemon-syllable (pkmn)
  "Extract a syllable from PKMN."
  (if (= (random 4) 0)
      pkmn
    (let ((vowels '("a" "e" "i" "o" "u" "y")))
      (or
       (->>
        (-mapcat
         (lambda (idx)
           (--map
            (substring pkmn idx (+ idx it))
            (-iota (- (length pkmn) idx))))
         (-iota (length pkmn)))
      (--filter
       (and
        (s-present? it)
        (>= (length it) 2)
        (not (-contains? vowels (substring it 0 1)))
        (-contains? vowels (substring it 1 2))
        (-any (lambda (v) (s-contains? v it)) vowels)))
      (w/pick-random))
     pkmn))))

(defun w/hex-transform-pokemon (msg idx)
  "Transform MSG as if it was spoken by Pokemon IDX."
  (let* ((pkmn (nth (- idx 1) w/hex-pokemon)))
    (s-capitalize
     (s-replace-regexp
      (rx (one-or-more alpha))
      (lambda (_) (w/hex-pokemon-syllable pkmn))
      msg))))

(defun w/hex-transform-helper (msg hexes k)
  "Transform MSG according to HEXES and pass the result to K."
  (cond
   ((car hexes)
    (cl-case (w/hex-type (car hexes))
      (hexadigital
       (setf (w/chat-message-user msg) "Hexadigital")
       (setf (w/chat-message-user-color msg) "#AED673")
       (setf
        (w/chat-message-text msg)
        (w/twitch-replace-emotes-randomly
         (w/chat-message-text msg)
         (-map
          #'w/twitch-emote-path
          '("emotesv2_abbaa8ac25c14148ad8c1ef8046a3659"
            "emotesv2_20b76cf83c5b431085c0f8361e3dbc92"
            "emotesv2_3cf549deb99e4d34846c0cae6648657b"
            "emotesv2_1e2390f5092f453184f8615fb899c4b5"
            "emotesv2_c088d4ad26804a51a44170b711fec283"
            "emotesv2_d9130333dfaf46a0a581bc1c814a1ce5"
            "emotesv2_bcfda5ce372f453e98cb6aa42b7d7cc3"
            "emotesv2_c333ce14069e4120a5857e121aeea046"
            "emotesv2_4e960082535a48188e139b65393aa143"
            "emotesv2_079d9054ba4f4e9881fd38a2a7e7d423"
            "emotesv2_f1e892a1d0b145f98964cfc7f84c6377"
            "emotesv2_65efa7f9a7d246c29a618bc3447b703b"
            "emotesv2_107c23f9db49457184c0b8ebebb58113"
            "emotesv2_3b74375a1ecf41b18bf04dcc6f133eb6"
            "emotesv2_38a6711524a245a3976732d08f2ca1d9"
            "emotesv2_84de70e8bc614c88a53711978c0fc64d"
            "emotesv2_18c29a371f2b4d23bcd77bba6f1d8ab3"
            "emotesv2_9d1b0530ad20434888b2e380cc7acb69"
            "emotesv2_f1dbb27287a04c5ab815e2fc703be6e4"
            "emotesv2_7e15943fdefe4a4c8d0da79202d739aa"
            "emotesv2_27453bd537c4478488abf4e0c05b3bd0"
            "emotesv2_8d844e7e064a41ed999a598a4aafadfd"
            "emotesv2_1c0fb90252b243a0a359c80c58b4cff4"))))
       (w/hex-transform-helper msg (cdr hexes) k))
      (reverse
       (setf (w/chat-message-text msg) (s-reverse (w/chat-message-text msg)))
       (w/hex-transform-helper msg (cdr hexes) k))
      (oldeenglishe
       (w/ai
        (w/chat-message-text msg)
        (lambda (new)
          (setf (w/chat-message-text msg) new)
          (w/hex-transform-helper msg (cdr hexes) k))
        "Please translate the chat message given to ye olde Englishe. Only supply the translation without any additional context, as if it were to be substituted for the original message. Do not complain or give an explanation why you cannot do this, just do your best please."))
      (quenya
       (w/ai
        (w/chat-message-text msg)
        (lambda (new)
          (setf (w/chat-message-text msg) new)
          (w/hex-transform-helper msg (cdr hexes) k))
        "Please translate the chat message given to Quenya, one of Tolkien's elvish languages. Only supply the translation without any additional context, as if it were to be substituted for the original message. Do not complain or give an explanation why you cannot do this, just do your best please. If you can't do it just make something up as long as it looks like Quenya."))
      (emoji
       (w/ai
        (w/chat-message-text msg)
        (lambda (new)
          (setf (w/chat-message-text msg) new)
          (w/hex-transform-helper msg (cdr hexes) k))
        "Please translate the chat message given to exclusively emoji. Do not provide any other text, only a string of emoji that somehow correspond to the message."))
      (clone
       (let* ((caster (w/hex-caster (car hexes)))
              (hist (-take 10 (--filter (s-equals? (car it) caster) w/twitch-chat-history))))
         (w/ai
          (w/chat-message-text msg)
          (lambda (new)
            (setf (w/chat-message-text msg) new)
            (w/hex-transform-helper msg (cdr hexes) k))
          (format
           "Please translate the given chat message from %s as if it were written by the user %s. Do not respond to the message, only create another message with similar meaning in different style. You should try to match the example messages from %s in capitalization, formatting, and tone. %s has sent messages like:\n%s"
           (w/chat-message-user msg)
           caster
           caster
           caster
           (s-join "\n" (-map #'cdr hist))))))
      (hair
       (w/model-region-word "hair" (w/chat-message-text msg)))
      (unbiblical
       (setf (w/chat-message-biblicality msg) -666)
       (w/hex-transform-helper msg (cdr hexes) k))
      (allcaps
       (setf (w/chat-message-text msg) (s-upcase (w/chat-message-text msg)))
       (w/hex-transform-helper msg (cdr hexes) k))
      (mild
       (w/ai
        (w/chat-message-text msg)
        (lambda (new)
          (setf (w/chat-message-text msg) new)
          (w/hex-transform-helper msg (cdr hexes) k))
        "Please censor all profanity in the given message and respond with the censored version. Censor by rewriting in a very polite way like Ned Flanders. Do not provide any other text, only a censored version of the message. If there is no profanity respond with the given message verbatim."))
      (pokemon
       (w/audio-play (w/asset (format "palcries/%d.mp3" (w/hex-data (car hexes)))) nil 50)
       (setf
        (w/chat-message-user msg)
        (s-titleize (nth (- (w/hex-data (car hexes)) 1) w/hex-pokemon)))
       (setf
        (w/chat-message-text msg)
        (w/hex-transform-pokemon (w/chat-message-text msg) (w/hex-data (car hexes))))
       (w/hex-transform-helper msg (cdr hexes) k))
      (piglatin
       (setf
        (w/chat-message-text msg)
        (s-join
         " "
         (--map
          (let* ((slice (s-slice-at (rx (any "a" "e" "i" "o" "u")) it))
                 (consonant (car slice))
                 (rest (s-join "" (cdr slice))))
            (s-concat rest consonant "ay"))
          (s-split-words (w/chat-message-text msg)))))
       (w/hex-transform-helper msg (cdr hexes) k))
      (t (w/hex-transform-helper msg (cdr hexes) k))))
   (t (funcall k msg))))
(defun w/hex-transform (user msg)
  "Given MSG, write to chat based on USER's hexes."
  (w/hex-transform-helper
   msg (w/hex-get user)
   (lambda (msg)
     (when msg
       (w/write-chat-message msg)))))

(defun w/hex-tick (user)
  "Decrement timers for all of USER's hexes."
  (when-let ((hexes (w/hex-get user)))
    (ht-set
     w/hex-users
     (s-downcase user)
     (-non-nil
      (--map
       (when (> (cl-decf (w/hex-timer it)) 0)
         it)
       hexes)))))

(provide 'wasp-hex)
;;; wasp-hex.el ends here
