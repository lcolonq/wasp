;;; wasp-debate --- "friend" is the baiter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-utils)
(require 'wasp-ai)
(require 'wasp-obs)
(require 'wasp-twitch)
(require 'wasp-dna)
(require 'wasp-fakechat)

(defconst
  w/debate-example-topics
  '(("is a hot dog a sandwich?" "not a sandwich" "is a sandwich")
    ("are our personalities defined by nature or nurture?" "nature" "nurture")
    ("is pineapple a good pizza topping?" "yes" "no")
    ("is metroidvania a coherent name for a video game genre?" "yes" "no")
    ("do humans have free will?" "yes" "no")
    ("does LCOLONQ have real background sounds?" "yes" "no")
    ("is LCOLONQ a househusband?" "yes" "no")
    ("how much wood would a woodchuck chuck?" "A woodchuck would chuck as much wood as a woodchuck could chuck if a woodchuck could chuck wood" "So much wood would a woodchuck chuck as a woodchuck would if a woodchuck could chuck wood!")
    ("are donut holes the right name for that confection?" "yes" "no")
    ("what exactly constitutes SotN?" "something of this nature" "Castlevania: Symphony of the Night (PSX, 1997)")
    ("what does the null pointer point to?" "nothing" "zero")
    ("which goes first, milk or cereal?" "milk" "cereal")
    ("is cereal a soup?" "yes" "no")
    ("wait can I add a question?" "yes" "no")
    ("why does Lisp have so many parentheses?" "a good reason" "a bad reason")
    ("what is even the point of this exercise?" "enlightenment and mutual satisfaction" "despair")
    ("are tabs or spaces better for indentation?" "tabs" "spaces")
    ("what is the airspeed velocity of an unladen swallow?" "what type of swallow?" "lol idk")
    ("vim or emacs?" "vim" "emacs")
    ("android or iphone?" "android" "iphone")
    ("object-oriented programming or functional programming?" "oop" "functional")
    ("is this stream live or prerecorded?" "live" "prerecorded")
    ("mac or pc?" "mac" "pc")
    ("is crack cocaine edible?" "maybe" "uncertain")
    ("are we alone in the universe?" "yes" "no")
    ("what's the stream idea?" "debate club" "what is happening")
    ("do I look fat in this dress?" "yes" "no")
    ("is this the year of the linux desktop?" "yes" "yes")
    ("do mmo enjoyers have rights?" "no" "no")
    ("would you still love me if I were a worm?" "yes" "no")
    ("chat is this real?" "yes" "no")
    ("is it over, or are we back?" "it's so over" "we are so back boys")
    ("what's for lunch?" "soup" "sandwich")
    ("can pancakes be dinner?" "yes" "no")
    ("are we there yet?" "no" "no, stop asking")
    ("where in the world is Carmen Sandiego?" "San Diego" "San Francisco")
    ("which came first, the chicken or the egg?" "chicken" "egg")
    ("magnets: how do they work?" "it's magic" "it's a miracle")))

(defvar w/debate-current-topic nil)
(defvar w/debate-current-opponent nil)
(defvar w/debate-current-friend-position nil)
(defvar w/debate-current-user-position nil)
(defvar w/debate-current-history nil)

(defun w/debate-topic (k)
  "Generate a new debate topic and pass the resulting list to K."
  (let* ((generate (= 0 (random 2)))
         (ass (--map (format "%s|%s|%s" (car it) (cadr it) (caddr it)) w/debate-example-topics))
         (user (-repeat (length ass) "topic")))
    (if generate
        (w/ai
         "topic"
         (lambda (data)
           (funcall k (-map #'s-trim (s-split "|" data))))
         "Respond to the phrase \"topic\" with a potential topic for internet debate alongside two possible positions to hold in that debate."
         user
         ass)
      (funcall k (w/pick-random w/debate-example-topics)))
    nil))

(defcustom w/debate-buffer "*wasp-debate*"
  "Name of buffer used to display debater."
  :type '(string)
  :group 'wasp)
(defcustom w/debate-podium-buffer "*wasp-debate-podium*"
  "Name of buffer used to display debater."
  :type '(string)
  :group 'wasp)
(define-derived-mode w/debate-mode special-mode "The Marketplace of Ideas"
  "Major mode for displaying the debate log."
  :group 'wasp)
(define-derived-mode w/debate-podium-mode special-mode "The Podium"
  "Major mode for displaying the debate podium."
  :group 'wasp)
(defun w/debate-get-buffer ()
  "Return the debate log buffer."
  (unless (get-buffer w/debate-buffer)
    (with-current-buffer (get-buffer-create w/debate-buffer)
      (w/debate-mode)))
  (get-buffer w/debate-buffer))
(defun w/debate-get-podium-buffer ()
  "Return the debate podium buffer."
  (unless (get-buffer w/debate-podium-buffer)
    (with-current-buffer (get-buffer-create w/debate-podium-buffer)
      (w/debate-podium-mode)))
  (get-buffer w/debate-podium-buffer))

(defun w/debate-display-user (user)
  "Place USER behind the debate podium."
  (w/twitch-get-user-avatar
   user
   (lambda ()
     (when (f-exists? (w/twitch-user-avatar-path user))
       (with-current-buffer (w/debate-get-podium-buffer)
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert
            (w/image-text
             (w/twitch-user-avatar-path user) user
             :width 200
             :height 200))))))))

(defun w/debate-system-message (nm msg)
  "Add a MSG from NM to the debate logs."
  (push (cons nm msg) w/debate-current-history)
  (with-current-buffer (w/debate-get-buffer)
    (let ((inhibit-read-only t))
      (insert (format "%s: %s\n" nm msg)))))

(defun w/debate-user-message (msg)
  "Add a MSG from the user to the debate logs."
  (push (cons 'user msg) w/debate-current-history)
  (with-current-buffer (w/debate-get-buffer)
    (let ((inhibit-read-only t))
      (insert (format "%s: %s\n" w/debate-current-opponent msg)))))

(defun w/debate-friend-respond ()
  "Have \"friend\" respond to the debate context and add the response to the log."
  (w/ai
   (s-join "\n" (--map (format "%s: %s" (car it) (cdr it)) (reverse w/debate-current-history)))
   (lambda (data)
     (w/debate-system-message 'friend data))
   (format
    "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. You are debating the topic \"%s\" with the user %s. Assuming you are taking the position \"%s\" in the debate, please respond with a short message arguing that position against your interlocutor. The message should only be one clause. YOur responses are short and sweet. You like people, video games, emojis, learning, and food."
    w/debate-current-topic
    w/debate-current-opponent
    w/debate-current-friend-position
    )))

(defun w/debate-ellg-respond ()
  "Have fake ellg respond to the debate context and add the response to the log."
  (let ((acidtube (w/dna-apply-hydrochloric-acid (w/dna-put-in-chemical-grade-tube w/dna-ellg))))
    (w/dna-g5-50-solution
     acidtube
     (s-join "\n" (--map (format "%s: %s" (car it) (cdr it)) (reverse w/debate-current-history)))
     (format
      "Respond to the message given as if you are the Twitch chat user ellg. ellg is a huge fan of JavaScript and Nightcore music. He is a bit snarky. He is always right and has the best takes. You are debating the topic \"%s\" with the user %s. Assuming you are taking the position \"%s\" in the debate, please respond with a short message arguing that position against your interlocutor. Your response should be short, no more than one sentence. You don't use capital letters and you don't tend to use punctuation. You are not a fan of Jonathan Blow."
      w/debate-current-topic
      w/debate-current-opponent
      w/debate-current-friend-position)
     (lambda (data)
       (w/debate-system-message 'ellg data)))))

(defun w/debate-begin (user)
  "Initiate a debate with USER."
  (w/debate-display-user user)
  (w/debate-topic
   (lambda (topic-resps)
     (let* ((topic (car topic-resps))
            (posns (cdr topic-resps))
            (fposidx (random 2))
            (oposidx (if (= fposidx 0) 1 0)))
       (message "%s" topic-resps)
       (w/obs-set-debate-topic-text (format "topic: %s" topic))
       (w/obs-toggle-debate-topic)
       (setq w/debate-current-topic topic)
       (setq w/debate-current-opponent user)
       (setq w/debate-current-friend-position (nth fposidx posns))
       (setq w/debate-current-user-position (nth oposidx posns))
       (w/debate-ellg-respond)))))

(defun w/debate-end ()
  "Finish the current debate."
  (w/obs-toggle-debate-topic)
  (with-current-buffer (w/debate-get-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (setq w/debate-current-history nil)
  (setq w/debate-current-topic nil)
  (setq w/debate-current-opponent nil)
  (setq w/debate-current-friend-position nil)
  (setq w/debate-current-user-position nil))

(provide 'wasp-debate)
;;; wasp-debate.el ends here
