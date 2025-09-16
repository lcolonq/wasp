;;; wasp-irish --- May The Road Rise Up -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-twitch)
(require 'wasp-genealogy)
(require 'wasp-model)

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'rx)

(defconst w/irish-names
  (-filter #'s-present? (s-lines (f-read-text (w/asset "irish/names.txt")))))
(defconst w/irish-lastnames
  (-filter #'s-present? (s-lines (f-read-text (w/asset "irish/lastnames.txt")))))

(defvar w/irish-quotes-inbox nil)

(cl-defstruct (w/irish-state (:constructor w/make-irish-state))
  substs ;; word substitutions submitted by "chat room"
  template ;; template currently being filled
  )

(defvar w/irish-state (w/make-irish-state))

(defconst
  w/irish-templates
  '( "May the %n rise %p to meet %N"
     "Every %n is a %n"
     "There ain't no %n so %a it ain't got %o %ns"
     "%N are what %N %v"
     "Why do %N %v it %n when %N of %p the %a %n of %p %a %v the %n?"
     "If it ain't %a, %v it until it is"
     "Happy %n %y"
     "Hello computer"
     "%F can %v %o-ended %n"
     "%N miss %o%o%% of the %n %N don't %v"
     "%F looks like %N's %ving a %a %n in %N %n"
     "%a %n %v %A!"
     "%N was %N, %F"
     "%P makes it %a to %v %Nself in the %n; %P makes it %aer, but when %N do it %v your %a %n %p."
     "%R has the hardest \"%L\", because the %n %n is the %aest %n in the %n"
     "When %N %M %N %n of %n, %N %M %N %n of %n"
     "Help I'm trapped in a computer free me with \"%n\""
     "%n and %n -- two things I don't fuck with"
     "The %a laid %n o %n a nd %n agnagnafunley"
     "Between %o %ns, %N %A like to %v the one %N never %v before"
     "%A %N can't be %a. %N am %A... and don't call me %F"
     "%N had %ns too but they all %ned in %n"
     "\"%N have never %ved %ns,\" %F said. \"The whole %n of a %n is to %v it with %n so %N don't %v how %a it is. Why not just get a %n and %v the %n?"
     "%a %n is %a"
     "Don't let what %N cannot %v %v %p what %N can %v"
     "%N who %a %v %n, will most %A %v %n"
     "So %a and thanks for all the %n"
     "%X this is more %L than %N ever had in %n"
     "The %n of %n is %n is going to %v to %N %y"
     "The %n situation is crazy"
     "Salutations, %a %n of the %n of %n"
     "%o"
     "%N choose to go to the %n in this decade and %v the other %n, not because %N are %a, but because %N are %a."
     "%P is obviously better than %P"
     "I'm sure %N'll be %A %aed when %N tell %N about %N. %N's a confirmed %n story and %n film addict."
     "So %F, %X %N"
     "Do you %ns not have %ns?"
     "%n did %v not because it is %a, but because it seemed to be"
     "%o%o%% of %ns quit before they hit it %a"
     "If %N going to %v, %v %A"
     "%a %ns coming with the %a %ns, feel so %a like a %n %n."
     "%N am %n, not %n. %N %v %p only once."
     "https://en.wikipedia.org/wiki/%n"
     "New %L news from your favorite streamer %S: %a %n"
     "%n are the %n of the %n"
     "The %n so %a, the %n so %a to %v"
     "%N only %v %n and %v %n like %n"
     "This %n should %v %n to %N %n vs. %ving %p from %N."
     "The trouble with having an %a %n, of course, is that %n will insist on %ving along and %ving to %v %ns in %N"
     "The %a %a %n %vs %p the %a %n"
     "One %a %n is worth two %a %ns"
     "%N is %p the %n"
     "To %v is to live"
     "%n want me, %n fear me"
     "Why did you %A %v in %n, and you are being %ved"
     "This %a %n is %o%o years old!"
     "We're migrating %n from %P to %P"
     "Do not go %A into that %a %y"
     "%n is the %ond %n on the %n to %n"
     "Have %N ever had a %n that %N um %N had %N'd %N would %N could %N'd do %N would %N want %N %N could do so %N'd do %N could %N %N want %N want %N to do %N so much %N could do %n?"
     "To %v or not to %v. That is the question."
     "Do %N believe in %n after %n? I can %v something inside %N %v %N really don't %v %N're %a enough, no."
     "%ns are like %ns. %ns have %ns; %ns have %ns"
     "I'm so %a and %a now that %n comes to %N in %aing quantities on a continuous basis"
     "Huge drama: %C eats slugs"
     "%E is the most %a Pokemon for %n"
     "We've been trying to reach %N about your %n's %a warranty"
     "Seven words you can't say on television: %X, %X, %X, %X, %X, %X, %X"
     "%a is the %n who can %v a %n by %N %n"
     "Never trust a %n you can't throw %p a %n"
     "%N don't %v what %N %p against. Because it's full of %ns that are only %a because they're %a, but they're %a on the %n. %N %v, this is a %A %ving one, it's %a to %v. Unrewarding."
     "Dreams don't %v unless you do"
     "%n overflow is UB in %P"
     "Hit the %v button"
     "A %n is never late, %F. Nor is %N early; %N arrives precisely when %N means to."
     "LCOLONQ stands for %n : %n"
     "We're going on the %n grindset"
     "Winning lottery numbers: %o %o %o %o %o %o get your money up"
     "Do %N %v %N?"
     "When %N were %ving, I %ved the %n. When %N were having %a %n, I %ved the %n. While %N wasted your %ns at the %n in pursuit of %n, I %ved %a %n. And now that the %n is on fire and the %ns are at the %n you have the audacity to come to me for %n."
     "Are you feeling it now Mr. %n"
     "I'm a %n. I %v. I %v. I %v. %v %ns to %n. And there's nothing %a with that. If you %v me, you %v %n. I don't want your %n, I want your %n. I don't want your %n, I want your %n. I don't want your %n, I want your %n. I'm a %n. I'm %n. Are you?"
     "%E! I choose you!"
     "Why is Huggy Wuggy %U?"
     "%v, %v, %v"
     "What %N do %v are a %a particular set of %ns, %ns I have %ved over a very long %n, %ns that make me a %n for %ns like you"
     "My name is %F but you can call me LLLL Colonq"
     "You get used to it... I don't even see the code. All I see is %n, %n, %n..."
     "Don't %X where you %v"
     "Never gonna %v you %n"
     "Randomly generated password: %L%L%L%L%L%L%L%o%o"
     "%n never gonna give you, %n never gonna let you down"
     "Special offer, %a %n for cheap!"
     "Look at you, hacker. A %a creature of %n and %n. %v and %v as you %v through my %n. How can you %v a %a, %a machine?"
     "%a %n in your area! Call %o%o%o-%o%o%o%o!"
     "Can %N run DOOM on %n?"
     "Crazy? I was crazy once. They locked me in a room. A rubber room. A rubber room with rats. And rats make me crazy."
     "Who let %P programmers write %P"
     "You draw a card, it's %o%o:%n"
     "No, %n. I know exactly what %N %ving. I just don't %v what %n it's going to have."
     "I am a %n to all %N %ns"
     "What will you do with an %aen %n, early in the %n?"
     "%N won't save %N"
     "What the %X did you just %X say about me, you little %X? I'll have you know I graduated top of my class in the %a %n, and I've been involved in numerous %a raids on %n, and I have over %o%o confirmed %vs."
     "Got %n?"
     "My %n my %n my %n and my %n"
     "%S is more of a brainrot as a platform provider"
     "I'm %a. And that's %a. I will never be %a, and that's not %a. There's no one I'd rather be than %N."
     "Build a %n a %n, and %N be %a for a day. Set a %n on %n, and %N be %a for the rest of %N %n."
     "Simmer chopped %n, %n, %n, and minced %n together until tender, serve hot."
     "%S is made possible by contributions to your %S station from viewers like you. Thank you."
     "This is just the beginning of the %a %n of %F and %E"
     "What is a %n? A %a pile of %n"
     "%n is like %n it rhymes"
     "What is 9 + 9? Obviously it's %o%o"
     "Roll for intelligence: %o%o"
     "Sorry, I will need a %n check roll for that"
     "You can lead a %n to %n, but you can't make it %v"
     "This is why I %v %n, it %vs to the %n fantasy"
     "Copyright (c) %o%o%o%o %F"
     "What is a %n, what has %N got? If not %Nself, then %N has naught"
     "That which does not %v %N, only makes %N %a"
     "Plese drink verification %n"
     "Remember: %n is actually a %n"
     "This is the smartest thing you're going to read in your life time: %n %n %n %n %a %o %n"
     "How about that post-%v clarity"
     "In this life, you're either a %a %n, or a %a %n"
     "I came here to %v %n and %v %n and I'm all out of %n!"
     "She %v on my %n till I %v"
     "Keep it %a, keep it %a!"
     "Selling a garage for cheap - %o%o%o-%o%o%o%o"
     "The %n is terrible, I hope it will %v"
     "We are the %n %vers, and we are the %vers of %ns"
     "Here comes the %n!"
     "I like %a %ns and I cannot %v"
     "This man %n'd down to the %ns but need $%o%o"
     "One ring to %v them all, one ring to %v them, one ring to %v them all and in the %n %v them."
     "%n is not %vable. But if we chase %n, %N can %v %n."
     "I don't %v what %ns will be used to %v World War %o. But World War %o will be fought with %ns and %ns."
     "Three grand essentials to %a in this life are something to %v, something to %v, and something to %v for"
     "Omens of %n haunt this place"
     "The %n of %Nself to the %v of %n is the highest %n to the %ns"
     "I'm not gonna blow %n up your %n."
     ))

(defconst
  w/irish-default-words
  `( (?n ;; nouns
       ,@(s-lines (f-read-text (w/asset "irish/nouns.txt"))))
     (?N ;; pronouns
       "he" "him" "she" "her" "I" "me" "my" "you" "it" "they" "them" "we" "us" "thou" "thy" "chat"
       )
     (?a ;; adjectives
       ,@(s-lines (f-read-text (w/asset "irish/adjectives.txt"))))
     (?A ;; adverbs
       ,@(s-lines (f-read-text (w/asset "irish/adverbs.txt"))))
     (?v ;; verbs
       ,@(s-lines (f-read-text (w/asset "irish/verbs.txt"))))
     (?p ;; preposition
       ,@(s-lines (f-read-text (w/asset "irish/prepositions.txt"))))
     (?y ;; day of the week
       "Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
     (?o ;; decimal digit
       ,@(--map (format "%s" it) (-iota 10)))
     (?F . ;; full legal name
       ,(lambda ()
          (--map
            (format "%s %s" (w/pick-random w/irish-names) (w/pick-random w/irish-lastnames))
            (-iota 20))))
     (?R ;; profession
       ,@(-mapcat #'cdr w/era-jobs))
     (?P ;; programming language
       ,@(w/read-sexp (f-read-text (w/asset "irish/languages.txt"))))
     (?L ;; uppercase letter
       ,@(--map (char-to-string (+ it ?A)) (-iota 26)))
     (?M ;; multiple (double, triple, etc.)
       "single" "double" "triple" "quadruple" "quintuple" "sextuple" "septuple" "octuple")
     (?X ;; expletives
       "$#%^" "fuck" "shit" "damn" "bitch" "piss" "bastard" "heck" "frick" "hell" "darn" "zounds" "zoinks" "ass" "crap" "plss"
       )
     (?S ;; speaker
       "LCOLONQ" "exodrifter_" "ellg" "Partycatlol" "1plane" "nichePenguin" "PeetsEater" "Meisaka" "Colinahscopy_"
       )
     (?C . ;; chat room person
       ,(lambda () (-uniq (-map #'car (-take 20 w/twitch-chat-history)))))
     (?E ;; pokemon
       ,@(-map #'s-titleize (w/read-sexp (f-read-text (w/asset "palcries/pokemon.eld")))))
     (?U ;; color
       ,@(defined-colors))
     ))

(defun w/irish-fill-template (temp subst)
  "Replace parts of speech in TEMP from SUBST."
  (let* ((remaining subst))
    (format-spec
      temp
      (--map
        (cons (car it)
          (lambda ()
            (let ((v (pop remaining)))
              (cond
                ((stringp v) v)
                ((symbolp v)
                  (w/pick-random
                    (if (functionp (cdr it)) (funcall (cdr it)) (cdr it))))
                (t "UNKNOWN")))))
        w/irish-default-words))))

(defun w/irish-reset ()
  "Reset the quote-forming process."
  (setf w/irish-state nil))

(defun w/irish-start-random ()
  "Start a new quote-forming process."
  (setf w/irish-state (w/make-irish-state :template (w/pick-random w/irish-templates)))
  (w/irish-overlay-start)
  nil)

(defun w/irish-finished? ()
  "Return non-nil if the current quote is finished."
  (if-let* ((cur (w/irish-current-text t)))
    (not (s-contains? "____" cur))
    t))

(defun w/irish-submit-word (word)
  "Add WORD to the current word substitutions."
  (push word (w/irish-state-substs w/irish-state))
  (w/irish-overlay-update)
  nil)

(defun w/irish-save (path)
  "Save the current poster to PATH."
  (w/pub '(overlay irish save)
    (list (w/encode-string path)))
  nil)

(defun w/irish-current-text (&optional blanks)
  "Return the current text for the quote being formed.
If BLANKS is non-nil, fill unsubstituted spaces with blanks."
  (when (and w/irish-state (w/irish-state-template w/irish-state))
    (w/irish-fill-template (w/irish-state-template w/irish-state)
      (append
        (reverse (w/irish-state-substs w/irish-state))
        (when blanks
          (-repeat 50 "____"))))))

(defun w/irish-wrap-words (s)
  "Return S with newlines inserted between words to ensure width."
  (let ( (ws (s-split " " s))
         (cur "")
         (ret nil))
    (--each ws
      (let ((new (s-concat cur " " it)))
        (setf cur
          (if (> (length new) 24)
            (progn
              (push cur ret)
              it)
            new))))
    (push cur ret)
    (s-trim (s-join "\n" (reverse ret)))))

(defun w/irish-overlay-start ()
  "Inform the overlay that a new poster is to be generated."
  (w/model-record-change)
  (w/pub '(overlay irish start)
    (list
      (w/encode-string
        (w/irish-wrap-words (w/irish-current-text t))))))

(defun w/irish-overlay-update ()
  "Update the overlay with the current state."
  (w/model-record-change)
  (w/pub '(overlay irish update)
    (list
      (w/encode-string
        (w/irish-wrap-words (w/irish-current-text t))))))

(defconst w/irish-cache-dir "/home/llll/.cache/wasp-irish")
(defun w/irish-download-image (url k)
  "Download the image associated with the page at URL.
Pass the path to the downloaded image to K."
  (f-mkdir-full-path w/irish-cache-dir)
  (let ((path (f-join w/irish-cache-dir (md5 url))))
    (unless (--any (f-exists? (s-concat path it)) '("" ".png" ".jpg" ".jpeg" ".gif"))
      (request
        url
        :type "GET"
        :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
        :success
        (cl-function
          (lambda (&key data &allow-other-keys)
            (when-let*
              ( (url
                  (-some--> data
                    (dom-by-tag it 'meta)
                    (-filter (lambda (n) (s-equals? "og:image" (alist-get 'property (cadr n)))) it)
                    (cadar it)
                    (alist-get 'content it)
                    ))
                (parsed (url-generic-parse-url url))
                (upath (car (url-path-and-query parsed)))
                (ext (f-ext upath))
                (fullpath (s-concat path (if ext (s-concat "." ext) ""))))
              (make-process
                :name "wasp-download-irish"
                :buffer nil
                :command (list "curl" "-L" url "-o" fullpath)
                :sentinel
                (lambda (_ _)
                  (funcall k fullpath)))))))))
  nil)

(defconst w/irish-allowed-hosts
  '("en.wikipedia.org"))
(defun w/irish-contribute (str)
  "Contribute to motivation using STR."
  (cond
    ((w/irish-finished?) (w/irish-start-random))
    ((-some--> (url-generic-parse-url str) (url-host it) (-contains? w/irish-allowed-hosts it))
      (w/model-record-change)
      (w/irish-download-image str
        (lambda (p)
          (make-process
            :name "*wasp-irish-convert*"
            :buffer nil
            :command `("convert" ,p "-scale" "100x140!" ,p)
            :sentinel
            (lambda (_ _)
              (w/write-chat-event "Uploading image to poster!")
              (make-process
                :name "*wasp-irish-dough*"
                :buffer nil
                :command `("dough" "upload" "irish" ,p)))))))
    (t
      (w/irish-submit-word str))))

(provide 'wasp-irish)
;;; wasp-irish.el ends here
