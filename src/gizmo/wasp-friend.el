;;; wasp-friend --- "friend" -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'flycheck)
(require 'wasp-utils)
(require 'wasp-audio)
(require 'wasp-ai)
(require 'wasp-chat)
(require 'wasp-twitch)
(require 'wasp-newspaper)
(require 'wasp-gcp)
(require 'wasp-aoc)
(require 'wasp-uwoomfie)
(require 'wasp-wikipedia)
(require 'muzak)
(require 'muzak-wasp)

(defcustom w/friend-buffer "*wasp-friend*"
  "Name of buffer used to display \"friend\"."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/friend-mode special-mode "\"friend\"'s lair"
  "Major mode for displaying \"friend\"'s lair."
  :group 'wasp
  (message "hi i'm \"friend\"")
  (setq-local cursor-type nil))

(defun w/get-friend-buffer ()
  "Return the \"friend\" buffer."
  (unless (get-buffer w/friend-buffer)
    (with-current-buffer (get-buffer-create w/friend-buffer)
      (w/friend-mode)))
  (get-buffer w/friend-buffer))

(defun w/friend-journalism-input ()
  "Collect an input for \"friend\"'s journalism based on recent activities."
  (s-join
   "\n"
   (cons
    (format "LCOLONQ: %s" (s-trim w/last-stream-transcription))
    (--map
     (format "%s: %s" (car it) (cdr it))
     (reverse (-take 20 w/twitch-chat-history))))))

(defun w/friend-journalism (author headline)
  "Retrieve \"friend\"'s opinion on current events related to HEADLINE.
AUTHOR was a contributing author btw."
  (w/ai
   (s-concat
    "Headline: " headline "\n\n"
    (w/friend-journalism-input))
   (lambda (resp)
     (when resp
       (w/write-chat-event (format "\"friend\" finished writing about: %s" headline))
       (funcall
        (if (= (random 5) 0) #'w/newspaper-screenshot (lambda (k) (funcall k nil)))
        (lambda (img)
          (when img
            (w/write-chat-event "...and the article included some photojournalism"))
          (push
           (w/make-newspaper-article
            :headline headline
            :author (format "\"friend\" and %s" author)
            :content (s-trim resp)
            :image img)
           w/newspaper-todays-articles)))))
       "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. You like people, video games, emojis, learning, and food. Given a headline of a newspaper article and a summary of recent user activity, please do your best journalist impression and produce a one paragraph article about the situation that fits the headline."))

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

;; (defun w/get-friend-expensive-tastes (k)
;;   "Pass non-nil to K if \"friend\" has expensive tastes this stream.
;; Also update the cached Amazon stock price for next stream."
;;   (fig//load-db2-entry
;;    "LCOLONQ" :amzn-price
;;    (lambda (price)
;;      (let ((prev (or price 0))
;;            (cur (fig//stock-price "AMZN")))
;;        (fig//update-db-number "LCOLONQ" :amzn-price (lambda (_) cur))
;;        (funcall k (> cur prev))))))

(defvar w/friend-tastes " You love eating cranberries and lemons.")
;; (fig//get-friend-expensive-tastes
;;  (lambda (expensive)
;;    (let ((moon (car (lunar-phase-for-date (calendar-current-date)))))
;;      (setf
;;       fig//friend-tastes
;;       (s-concat
;;        (cond
;;         ((-contains? '("New" "Waxing Crescent") moon) " You prefer warm foods like soups.")
;;         ((-contains? '("First Quarter" "Waxing Gibbous") moon) " You prefer to eat leafy greens and fruits.")
;;         ((-contains? '("Full" "Waning Gibbous") moon) " You prefer to eat barbeque and grilled meats.")
;;         ((-contains? '("Last Quarter" "Waning Crescent") moon) " You prefer to eat corn beans and squash.")
;;         (t "")
;;         )
;;        (if expensive " You have expensive taste in food and dislike any food that can be obtained cheaply." ""))))))

;; states:
;; default
;; jumping
;; eating, eating0, eating1, eating2
;; chatting, chatting0
(defvar w/friend-state 'default)
(defvar w/friend-emotion "neutral")
(defvar w/friend-message-cache nil)
(defvar w/friend-state-timer 0)

(defvar w/friend-animation 1)
(defvar w/friend-speech "")
(defvar w/friend-speech-timer 0)

(defconst w/friend-composition-examples
  '(("My Life Is Like A Video Game" . "A/A/c/c/c/dcc/c///a/a/a/f/g/f/f///a/a/a/a/g/g/ga//f//")
    ("Super Idol" . "gg[g#]gfg[CD#cG#][D#][CG#f][Cd#][Cc]C[Cd#]/[DFfd][FA#][DA#f]D[Dg][A#f][Dd#a#]f[GBgd]B[Gd#][GDc][Gd#]G[Gd#]/[D#Gc]G[D#cg][D#g][D#g#][dg][D#f][d#d#][D#Ggc]f[D#][D#Gg][D#c][D#][D#c][d#][DFdA#]F[DA#d][Dd][Dg]/[Da#g]/[D#d#][D#][D#][D#][D#][FD#][GA#][fd#][gA#]")
    ("Reindeer" . "FG/FD/B/A/G/////GAGAG/c/B///////FG/FD/B/A/G/////GAGAG/d/c/////|C4~~~G3~~~C4~~~G3~~~C~~~E3~D#3~D3~~~~~~~G3~~~D3~~~G3~~~D3~~~G3~~D3G3~B3/C4")))

(defun w/friend-compose-song (theme)
  "Compose a song about THEME to play on the bells."
  (w/ai
   theme
   (lambda (res)
     (let* ((sp (s-split ":" (s-trim res)))
            (name (s-trim (car sp)))
            (song (s-trim (cadr sp))))
       (when (and (stringp name) (stringp song))
         (w/friend-respond
          (format "You just composed a song about %s called %s! Say something about it!" theme name)
          (lambda ()
            (w/write-chat-event (format "The song is called %s: %s" name song))
            (w/add-song (s-concat "friend's " name) song)
            (muzak/play-tracks song))))))
   "Please compose a song about the provided theme. The format for the song is a sequence of characters with meanings as follows: / represents a rest, uppercase letters A through G indicate semitones, octaves are specified with a number following a semitone, ~ extends the duration of a note, square brackets like [] group notes together into a chord. The pipe character | separates tracks. Respond only with the song's name followed by a colon folowed by the song notes. Do not explain yourself. The song should ideally be 20 to 30 notes long."
   (-map #'car w/friend-composition-examples)
   (--map (format "%s: %s" (car it) (cdr it)) w/friend-composition-examples)))

(defun w/friend-personality (msg k)
  "Given MSG, pass a string with more personality to K."
  (let ((call (s-concat w/friend-emotion " | " msg)))
    (w/ai
     call
     (lambda (new)
       (let ((sp (s-split "|" (s-trim new))))
         (if (= 2 (length sp))
             (progn
               (when (stringp (car sp))
                 (setf w/friend-emotion (s-trim (car sp))))
               (when (stringp (cadr sp))
                 (let ((resp (s-trim (cadr sp))))
                   (push (cons call (s-trim new)) w/friend-message-cache)
                   (funcall k resp))))
           (let ((resp (s-trim new)))
             (push (cons call (s-trim new)) w/friend-message-cache)
             (funcall k resp)))))
     (s-concat
      "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause. You like people, video games, emojis, learning, and food."
      "The theme of LCOLONQ's stream today is " (s-trim (w/slurp "~/today.txt")) " "
      "The title of LCOLONQ's stream today is " w/twitch-current-stream-title " "
      w/friend-tastes
      )
     (cons "neutral | Mimeyu fed you an apple." (reverse (-take 5 (-map #'car w/friend-message-cache))))
     (cons "happy | yum apple so good" (reverse (-take 5 (-map #'cdr w/friend-message-cache))))
     )))

(defun w/enemy-personality (msg k)
  "Given MSG, pass a string with more personality (enemy mode) to K."
  (w/ai
   (s-concat w/friend-emotion " | " msg)
   (lambda (new)
     (let ((sp (s-split "|" (s-trim new))))
       (when (= 2 (length sp))
         (when (stringp (car sp))
           (setf w/friend-emotion (s-trim (car sp))))
         (when (stringp (cadr sp))
           (funcall k (s-trim (cadr sp)))))))
   (s-concat
    "You are the personality of a desktop buddy named \"enemy\". \"enemy\" is irreverant and rude. You are very intelligent in a cute way and mean like a snake. You live in the corner of LCOLONQ's stream and provide commentary on events. Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause."
    w/friend-tastes
    )
   "neutral | notgeiser fed you bone hurting juice."
   "disdainful | I really dislike you strongly, notgeiser."
   ))

(defun w/friend-set-state (st &optional time)
  "Set \"friend\"'s state to ST for TIME seconds."
  (setf w/friend-state st)
  (setf w/friend-state-timer (or time 5)))

(defun w/friend-set-speech (msg &optional time)
  "Have \"friend\" say MSG for TIME."
  (w/write-chat-event (s-concat "Friend says: " msg))
  (setf w/friend-speech msg)
  (setf w/friend-speech-timer (or time 5)))

(defun w/friend-say (msg)
  "Have \"friend\" say MSG."
  (w/daily-log (format "[FRIEND]: %s" msg))
  (w/friend-pronounce-phonemes (w/friend-replace-graphemes msg))
  (w/friend-set-speech msg 10)
  (w/friend-set-state 'chatting 10))

(defun w/friend-feed (user food)
  "Call when USER fed FOOD to \"friend\"."
  (if (s-equals? "imgeiser" user)
      (w/enemy-personality
       (format "You dislike %s and they are your enemy. %s fed you %s" user user food)
       (lambda (msg)
         (w/friend-set-speech msg 6)
         (w/friend-set-state 'eating 6)))
    (w/friend-personality
     (format "%s fed you %s" user food)
     (lambda (msg)
       (w/friend-set-speech msg 6)
       (w/friend-set-state 'eating 6)))))

(defun w/friend-respond (ev &optional k)
  "Call when an event EV happens to \"friend\".
If K is specified, call it after the response."
  (w/friend-personality
   ev
   (lambda (msg)
     (w/friend-say msg)
     (when k
       (funcall k)))))

(defun w/friend-chat (user msg)
  "Call when USER sends MSG to \"friend\"."
  (if (s-equals? user "imgeiser")
      (w/enemy-personality
       (format "You dislike %s and they are your enemy. %s says: %s" user user msg)
       (lambda (msg)
         (w/friend-set-speech msg 10)
         (w/friend-set-state 'chatting 10)))
    (w/friend-respond (format "%s says: %s" user msg))))

(defun w/friend-gift (user gift)
  "Call when USER gave GIFT to \"friend\"."
  (if (s-equals? user "imgeiser")
      (w/enemy-personality
       (format "You dislike %s and they are your enemy. %s gave you %s as a Christmas present." user user gift)
       (lambda (msg)
         (w/friend-set-speech msg 6)))
    (w/friend-personality
     (format "%s gave you %s as a Christmas present." user gift)
     (lambda (msg)
       (w/friend-set-speech msg 6)))))

(defun w/friend-tfig (user tfig)
  "Call when USER took TFIG from \"friend\"."
  (if (not (s-equals? "imgeiser" user))
      (w/enemy-personality
       (format "You dislike %s and they are your enemy. %s took away %s from you and stole your Christmas present." user user tfig)
       (lambda (msg)
         (w/friend-set-speech msg 6)))
    (w/friend-personality
     (format "%s took away %s from you and stole your Christmas present." user tfig)
     (lambda (msg)
       (w/friend-set-speech msg 6)))))

(defun w/friend-react-wikipedia (user page)
  "Call when USER asks \"friend\" to react to PAGE on Wikipedia."
  (w/fetch-wikipedia
   page
   (lambda (sum)
     (w/friend-respond (format "%s asks you to react to the Wikipedia page for %s. The page summary is: %s" user page sum)))))

(defun w/friend-callout-flycheck-error ()
  "Call to respond to a random Flycheck error in the current buffer."
  (when-let* ((errs (--filter (eq (flycheck-error-level it) 'error) flycheck-current-errors))
              (err (nth (random (length errs)) errs)))
    (w/friend-respond
     (s-concat
      "LCOLONQ made an error while programming: "
      (flycheck-error-message err)))))

(defun w/friend-callout-holiday ()
  "Call to respond to the current holiday."
  (w/friend-respond "It's a beautiful day today! Say something about it!"))

(defun w/friend-callout-hexamedia ()
  "Call to respond to a random recent chatter's Hexamedia card collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :hexamedia-cards (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (cards (cdr user))
         (coll (and cards (nth (random (length cards)) cards))))
    (when coll
      (w/friend-respond
       (format
        "%s has collected %s out of 20 cards in the %s collection. Please mention the collection name and the person collecting."
        (car user)
        (cdr coll)
        (car coll))))))

(defun w/friend-callout-copfish ()
  "Call to respond to a random recent chatter's Copfish ratio."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :copfish-ratio (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (when user
      (w/friend-respond
       (format
        "%s has collected %s out of %s fish in the Copfish fish catching collection. Please mention the collection name and the person collecting."
        (car user)
        (cadr user)
        (cddr user))))))

(defun w/friend-callout-uwoomfie ()
  "Call to respond to a random recent chatter's Uwoomfie status."
  (let* ((users
          (-filter
           #'cdr
           (--map
            (cons (car it) (w/uwoomfie-get-status (car it)))
            (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (cl-case (cdr user)
      (cool (w/friend-respond (format "According to UWOSLAB, %s is a very cool person. Make sure to mention their username." (car user))))
      (honored (w/friend-respond (format "According to UWOSLAB, %s is an honorary viewer. Make sure to mention their username." (car user))))
      (t nil))))

(defun w/friend-callout-shindaggers ()
  "Call to respond to a random recent chatter's Shindaggers knife collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :shindaggers-knives (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (knives (cdr user))
         (knife (and knives (nth (random (length knives)) knives))))
    (when knife
      (w/friend-respond
       (format
        "%s has collected the %s from shindig's Shindaggers knife collection. Please mention the collection name and the person collecting and the knife."
        (car user)
        knife)))))

(defun w/friend-callout-aoc ()
  "Call to respond to a random recent chatter's Advent of Code completion."
  (let* ((users (-filter #'cdr (--map (cons (car it) (w/aoc-lookup-stars (car it))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (w/friend-respond
     (format
      "%s has been doing Advent of Code this year, and they've completed %d out of %d problems so far."
      (car user)
      (cdr user)
      (w/aoc-max-stars)))))

(defun w/friend-callout-gcp ()
  "Call to respond to the current GCP dot."
  (w/gcp-dot
   (lambda (d)
     (w/friend-respond
      (format
       "The Global Consciousness Project indicator is currently as follows: %s"
       (w/gcp-describe d))))))

(defun w/friend-callout-resolution ()
  "Call to respond to a random recent chatter's resolve."
  (when-let*
      ((users (-filter #'cdr (--map (cons (car it) (alist-get :resolution (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
       (user (and users (nth (random (length users)) users))))
    (if (s-match (rx (one-or-more digit) (zero-or-more space) "x" (zero-or-more space) (one-or-more digit)) (cdr user))
        (w/friend-respond
         (format
          "%s snarkily said that their New Year's resolution was a screen resolution. What do you think about this?" (car user)))
      (w/friend-respond
       (format
        "%s made a New Year's resolution to %s. Ask them how it's going!"
        (car user)
        (cdr user))))))

(defun w/get-friend-offset ()
  "Return the number of newlines to print before \"friend\"."
  (if (-contains? '(jumping) w/friend-state)
      w/friend-animation
    1))

(defun w/get-friend-face ()
  "Return the eyes and mouth for \"friend\" as a list of strings."
  (cl-case w/friend-state
    (jumping (list "^" "^" "ww"))

    (eating (list "v" "v" "<>"))
    (eating0 (list "v" "v" "<>"))
    (eating1 (list "-" "-" "mw"))
    (eating2 (list "-" "-" "wm"))

    (chatting (list ">" ">" "oo"))
    (chatting0 (list ">" ">" "~~"))

    (t (list "-" "-" "ww"))))

(defun w/get-friend-bubble ()
  "Return the text bubble for \"friend\"."
  (if (> w/friend-speech-timer 0)
      w/friend-speech
    nil))

(defun w/friend-random-event ()
  "Activate a random \"friend\" event."
  (cl-case (random 10)
    (0 (w/friend-callout-flycheck-error))
    (1 (w/friend-callout-gcp))
    (2 (w/friend-callout-hexamedia))
    (3 (w/friend-callout-uwoomfie))
    (4 (w/friend-callout-shindaggers))
    (5 (w/friend-callout-copfish))
    (6 (w/friend-callout-resolution))
    (9 (w/friend-callout-holiday))
    (t (w/friend-set-state 'jumping))))

(defun w/update-friend ()
  "Update \"friend\"'s state per tick."
  (setf w/friend-animation (% (+ w/friend-animation 1) 2))
  (if (> w/friend-state-timer 0)
      (cl-decf w/friend-state-timer)
    (setf w/friend-state 'default))
  (if (> w/friend-speech-timer 0)
      (cl-decf w/friend-speech-timer))
  (when (= (random 120) 0)
    (w/friend-random-event))
  (cl-case w/friend-state
    (eating (setf w/friend-state 'eating0))
    (eating0 (setf w/friend-state 'eating1))
    (eating1 (setf w/friend-state 'eating2))
    (eating2 (setf w/friend-state 'eating1))

    (chatting (setf w/friend-state 'chatting0))
    (chatting0 (setf w/friend-state 'chatting))
    ))

(defun w/render-friend ()
  "Render the \"friend\" buffer."
  (save-excursion
    (with-current-buffer (w/get-friend-buffer)
      (setq-local cursor-type nil)
      (let*
          ((inhibit-read-only t)
           (face (w/get-friend-face))
           (bubble (w/get-friend-bubble)))
        (erase-buffer)
        (w/write
         (format-spec
          "%a\
  /----\\  
 / %l  %r \\ 
 \\  %m  /
  +----+\
"
;;           "%a\
;;    ----    
;;   /    \\
;; ----------
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;   oooooo      
;;  oooooooo     
;; oo/----\\oo   
;; o/ %l  %r \\o 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;     /\\      
;;    /\\/\\     
;;   / :3 \\    
;;  /santa!\\   
;; ~~~~~~~~~~
;; ~~~~~~~~~~   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;     /\\      
;;    / *\\     
;;   / *  \\    
;;  / *  * \\   
;; ----------   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
;;           "%a\
;;     ---       
;;    /   \\     
;;   / [=] \\    
;; -----------   
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
          `((?a . ,(s-repeat (w/get-friend-offset) "          \n"))
            (?l . ,(car face))
            (?r . ,(cadr face))
            (?m . ,(caddr face)))))
        (goto-char (point-min))
        (end-of-line)
        (w/write (or bubble ""))
        (forward-line)
        (end-of-line)
        (w/write (if bubble "/" ""))
        ))))

(defvar w/friend-timer nil)
(defun w/run-friend-timer ()
  "Run the \"friend\" timer."
  (when w/friend-timer
    (cancel-timer w/friend-timer))
  (condition-case e
      (progn
        (w/update-friend)
        (w/render-friend))
    ((debug error)
     (message "friend error: %s" e)
     (cancel-timer w/friend-timer)
     (setq w/friend-timer nil)))
  (setq
   w/friend-timer
   (run-with-timer 1 nil #'w/run-friend-timer)))

(defun w/start-friend ()
  "Launch \"friend\"."
  (interactive)
  (w/run-friend-timer))

(defun w/stop-friend ()
  "Stop \"friend\"."
  (interactive)
  (cancel-timer w/friend-timer)
  (message "\"friend\" is going to sleep!"))

(provide 'wasp-friend)
;;; wasp-friend.el ends here
