;;; wasp-friend-voice --- "friend" talks -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-friend)

(defconst w/friend-grapheme-phonemes
  '((("b" "bb") . "bug") (("d" "dd" "ed") . "dad")
    (("f" "ff" "ph" "gh" "lf" "ft") . "fat")
    (("g" "gg" "gh" "gu" "gue") . "gun") (("h" "wh") . "hop")
    (("j" "ge" "g" "dge" "di" "gg") . "jam")
    (("k" "c" "ch" "cc" "lk" "qu" "q" "ck" "x") . "kit")
    (("l" "ll") . "live") (("m" "mm" "mb" "mn" "lm") . "man")
    (("n" "nn" "kn" "gn" "pn" "mn") . "net") (("p" "pp") . "pin")
    (("r" "rr" "wr" "rh") . "run")
    (("s" "ss" "c" "sc" "ps" "st" "ce" "se") . "sit")
    (("t" "tt" "th" "ed") . "tip") (("v" "f" "ph" "ve") . "vine")
    (("w" "wh" "u" "o") . "wit")
    (("z" "zz" "s" "ss" "x" "ze" "se") . "zed")
    (("s" "si" "z") . "treasure") (("ch" "tch" "tu" "te") . "chip")
    (("sh" "ce" "s" "ci" "si" "ch" "sci" "ti") . "sham")
    (("th ") . "thongs") (("th") . "leather")
    (("ng" "n" "ngue") . "ring") (("y" "i" "j") . "you")
    (("a" "ai" "au") . "cat")
    (("a" "ai" "eigh" "aigh" "ay" "er" "et" "ei" "au" "ea" "ey") . "bay")
    (("e" "ea" "u" "ie" "ai" "a" "eo" "ei" "ae") . "end")
    (("e" "ee" "ea" "y" "ey" "oe" "ie" "i" "ei" "eo" "ay") . "be")
    (("i" "e" "o" "u" "ui" "y" "ie") . "it")
    (("i" "y" "igh" "ie" "uy" "ye" "ai" "is" "eigh") . "spider")
    (("a" "ho" "au" "aw" "ough") . "swan")
    (("o" "oa" "oe" "ow" "ough" "eau" "oo" "ew") . "open")
    (("o" "oo" "u" "ou") . "wolf") (("u" "o" "oo" "ou") . "lug")
    (("o" "oo" "ew" "ue" "oe" "ough" "ui" "oew" "ou") . "who")
    (("oi" "oy" "uoy") . "join") (("ow" "ou" "ough") . "now")
    (("a" "er" "i" "ar" "our" "ur") . "about")
    (("air" "are" "ear" "ere" "eir" "ayer") . "chair") (("a") . "arm ")
    (("ir" "er" "ur" "ear" "or" "our" "yr") . "bird")
    (("aw" "a" "or" "oor" "ore" "oar" "our" "augh" "ar" "ough" "au") . "paw")
    (("ear" "eer" "ere" "ier") . "ear") (("ure" "our") . "cure")))

(defconst w/friend-phonemes
  (-sort
   (-on #'> (lambda (x) (length (car x))))
   (--mapcat
    (-map (lambda (g) (cons g (cdr it))) (car it))
    w/friend-grapheme-phonemes)))

(defun w/friend-replace-graphemes (str)
  "Replace all graphemes with phoneme words in STR."
  (let* ((phoneme-codes (--map-indexed (cons (cdr it) (format "%s," it-index)) w/friend-grapheme-phonemes))
         (grapheme-codes (--map (cons (car it) (alist-get (cdr it) phoneme-codes nil nil #'s-equals?)) w/friend-phonemes))
         (cleaned (s-downcase (replace-regexp-in-string "[^[:alpha:]]" "" str))))
    (--map (car (nth (string-to-number it) phoneme-codes)) (-filter #'s-present? (s-split "," (s-replace-all grapheme-codes cleaned))))))

(defun w/friend-phoneme-path (ph)
  "Return a randomly chosen path to the given PH."
  (let ((samples (f--entries (w/asset "friendvoice/") (s-contains? ph it) t)))
    (nth (random (length samples)) samples)))
(defun w/friend-pronounce-phonemes (ph)
  "Say PH."
  (let ((files (-map #'w/friend-phoneme-path ph)))
    (apply
     #'start-process
     "phoneme-say" nil "playphonemes"
     files)))

(provide 'wasp-friend-voice)
;;; wasp-friend-voice.el ends here
