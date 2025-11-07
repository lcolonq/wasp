;;; wasp-tcg --- trading card game -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'f)

(require 'wasp-twitch)
(require 'wasp-user)

(defconst w/tcg-bases
  (-map #'f-base
    (f-entries "/home/llll/src/newton/crates/renderer/src/assets/textures/tcg/bases")))

(defvar w/tcg-request-sequence 0)
(defvar w/tcg-request-handlers (ht-create))

(cl-defstruct (w/tcg-card (:constructor w/make-tcg-card))
  name
  type
  depicted-subject
  element
  color
  faction
  equity
  boost-level
  rarity
  rarity-level
  body-text
  base-image-name
  flags ;; comma-separated strings: inverse, etc.
  )

(defun w/tcg-determine-rarity ()
  "Return a rarity level."
  (let ((ret 0))
    (while (= (random 2) 0)
      (cl-incf ret))
    ret))

(defun w/tcg-render-rarity (r)
  "Convert rarity R to a string."
  (cond
    ((= 0 r) "C")
    ((= 1 r) "R")
    (t (s-concat (s-repeat (- r 1) "S") "R"))))

(defun w/tcg-pick-flags (u)
  "Return flags for the user U."
  (ignore u)
  (-concat
    (when (= (random 20) 0) (list "inverse"))))

(defun w/tcg-random-user-card (k)
  "Generate a random `w/tcg-card' and pass it to K."
  (let* ( (users (-map #'car w/twitch-chat-history))
          (name (w/pick-random users))
          (ud (w/user-cache-get name))
          (element (alist-get :element ud))
          (colornm (or (cadr (alist-get element w/user-elements nil nil #'s-equals?)) "grey"))
          (color (w/color-value-to-html-code (color-values colornm)))
          (faction (format "%s" (or (alist-get :faction ud) 'none)))
          (equity (format "%s" (or (alist-get :equity ud) 0)))
          (boost-level (format "%s" (or (alist-get :boost ud) "ABSTAINER")))
          (rarity-level (w/tcg-determine-rarity))
          (rarity (w/tcg-render-rarity rarity-level)))
    (funcall k
      (w/make-tcg-card
        :name name
        :type "user"
        :depicted-subject name
        :element (or element "neutral")
        :color color
        :faction faction
        :equity equity
        :boost-level boost-level
        :rarity rarity
        :rarity-level (format "%s" rarity-level)
        :body-text ""
        :base-image-name (w/pick-random w/tcg-bases)
        :flags (s-join "," (w/tcg-pick-flags ud))))
    nil))

(defun w/tcg-encode-card (c)
  "Encode C to a string."
  (s-join "\t"
    (list
      (w/. name c)
      (w/. type c)
      (w/. depicted-subject c)
      (w/. element c)
      (w/. color c)
      (w/. faction c)
      (w/. equity c)
      (w/. boost-level c)
      (w/. rarity c)
      (w/. rarity-level c)
      (w/. body-text c)
      (w/. base-image-name c)
      (w/. flags c))))

(defun w/tcg-generate-card (c k)
  "Generate the card C.
Pass the resulting PNG to K."
  (let* ( (seq (cl-incf w/tcg-request-sequence))
          (msg (format "%s\t%s" seq (w/tcg-encode-card c))))
    (ht-set w/tcg-request-handlers seq
      (lambda (resp)
        (ht-remove w/tcg-request-handlers seq)
        (funcall k resp)))
    (w/binary-pub "overlay tcg generate" msg)))

(provide 'wasp-tcg)
;;; wasp-tcg.el ends here
