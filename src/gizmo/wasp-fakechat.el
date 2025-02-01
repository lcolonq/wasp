;;; wasp-fakechat --- Fake Twitch chatters -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'soundboard)
(require 'wasp-utils)
(require 'wasp-chat)
(require 'wasp-audio)
(require 'wasp-ai)
(require 'wasp-twitch)
(require 'wasp-prod)

(defvar w/fake-chatters nil
  "List of all active fake chatters.")

(defconst w/fake-chatter-enclosure-buffer "*wasp-false-enclosure*")

;; immutable information about a particular chatter identity
(w/defstruct
  w/fake-chatter-profile
  username
  color
  sigil
  compute-likeliness ;; state -> float
  send-message ;; state -> update buffer
  )

;; mutable chatter state, resets each stream
(w/defstruct
  w/fake-chatter
  profile
  profile-state ;; profile-dependent state type
  (message-count 0) ;; total messages sent this stream
  last-message ;; timestamp of last message sent
  )

(defun w/fake-chatter-send (st msg &optional buf)
  "Insert MSG in the chat log as ST."
  (let* ((prof (w/fake-chatter-profile st))
          (trimmed (s-replace-regexp "^.+: " "" (s-replace "\n" " " (s-trim msg))))
          (text-colored-bible-res (w/bible-colorize-sentence trimmed))
          (text-colored-bible (car text-colored-bible-res))
          (bible-score (cdr text-colored-bible-res)))
    (push (cons (w/fake-chatter-profile-username prof) trimmed) w/twitch-chat-history)
    (cl-incf (w/fake-chatter-message-count st))
    (setf (w/fake-chatter-last-message st) (current-time))
    (unless (string-empty-p text-colored-bible)
      (when (s-contains? "hexadiCoding" trimmed)
        (soundboard//play-clip "developers.ogg"))
      (w/write-chat-message
        (w/make-chat-message
          :user (w/fake-chatter-profile-username prof)
          :id ""
          :text (w/twitch-add-7tv-emotes text-colored-bible)
          :user-color (w/fake-chatter-profile-color prof)
          :sigil (w/fake-chatter-profile-sigil prof)
          :biblicality bible-score)
        buf))))

(defun w/fake-chatter-select ()
  "Return the fake chatter who should speak."
  (let* ((weights
           (--map
             (cons
               (round
                 (* 100
                   (funcall (w/fake-chatter-profile-compute-likeliness (w/fake-chatter-profile it)) it)))
               it)
             w/fake-chatters))
          (passing-chatters (--filter (< (random 100) (car it)) weights)))
    (when passing-chatters
      (let ((chosen-chatter (nth (random (length passing-chatters)) passing-chatters)))
        (cdr chosen-chatter)))))

(defun w/fake-chatter-run (st)
  "Run the fake chatter ST."
  (when-let* ((prof (w/fake-chatter-profile st)))
    (funcall (w/fake-chatter-profile-send-message prof) st)))

(defun w/fake-chatters-handle ()
  "Handle the active fake chatters."
  (when-let* ((st (w/fake-chatter-select)))
    (w/fake-chatter-run st)))

(defvar w/fake-chatter-timer nil)
(defun w/run-fake-chatter-timer ()
  "Run the fake chatter timer."
  (when w/fake-chatter-timer
    (cancel-timer w/fake-chatter-timer))
  (w/fake-chatters-handle)
  (setq
    w/fake-chatter-timer
    (run-with-timer 30 nil #'w/run-fake-chatter-timer)))

(defun w/start-fake-chatters ()
  "Enable fake chatters."
  (interactive)
  (w/run-fake-chatter-timer))

(defun w/stop-fake-chatters ()
  "Disable fake chatters."
  (interactive)
  (cancel-timer w/fake-chatter-timer)
  (setq w/fake-chatter-timer nil))

(defun w/fake-chat-prompt-build (_st)
  "Assemble a standard chatlog plus transcription prompt for ST."
  (s-join
    "\n"
    (funcall (if (s-present? w/last-stream-transcription) #'cons (lambda (_ y) y))
      (format "LCOLONQ: %s" (s-trim w/last-stream-transcription))
      (--map
        (format "%s: %s" (car it) (cdr it))
        (reverse
          (-take 5 w/twitch-chat-history))))))

(defun w/fake-chat-system-prompt-build (st custom)
  "Build a system prompt for ST using a template combined with CUSTOM."
  (let* ((prof (w/fake-chatter-profile st))
          (nm (w/fake-chatter-profile-username prof)))
    (s-join
      " "
      (list
        (format
          "You are a Twitch chatter named %s talking in LCOLONQ's chat. LCOLONQ streams programming, but the conversation is sometimes off-topic. Your responses are brief, never more than one sentence. You type in all lowercase with no punctuation. You speak informally and casually, and address the streamer directly but not by name. Your messages should be at most two sentences."
          nm)
        custom))))

(defun w/fake-chatter-standard-likeliness (st)
  "Compute the standard likeliness for ST to chat."
  (let* ((last (w/fake-chatter-last-message st))
          (cur (current-time))
          (diff (if last (time-subtract cur last) 99999999))
          (d (time-convert diff 'integer)))
    (+
      (if (> d 300) 0.01 0.1)
      (if
        (--any?
          (s-contains? (w/fake-chatter-profile-username (w/fake-chatter-profile st)) (cdr it))
          (-take 20 w/twitch-chat-history))
        0.8
        0.0))))

(defun w/fake-chatter-elevated-likeliness (st)
  "Compute the elevated likeliness for ST to chat."
  0.1)

(defconst w/fake-chatter-profile-prodzpod
  (w/make-fake-chatter-profile
    :username "prodzpod"
    :compute-likeliness #'w/fake-chatter-standard-likeliness
    :send-message
    (lambda (st) (w/prod-clone-respond (w/fake-chat-prompt-build st)))))

(defconst w/fake-chatter-profile-forsen
  (w/make-fake-chatter-profile
    :username "forsen"
    :color "#777777"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg))
        "You are a Twitch chatter who is obsessed with the Twitch streamer Forsen. You are somewhat silly. You're currently chatting in LCOLONQ's stream. You are very laconic, preferring to respond with single Twitch emotes. Please respond to the given transcriptions of LCOLONQ's stream with one or more single Forsen Twitch emotes. You do not explain what emotes mean. You do not respond to specific chatters by name. You never type anything other than emotes. You only type one or two emotes at a time."
        "LCOLONQ: Hey guys, let's have a good day today."
        "forsenC"
        ))))

(defconst w/fake-chatter-profile-eighteyedsixwingedseraph
  (w/make-fake-chatter-profile
    :username "eighteyedsixwingedseraph"
    :color "#777777"
    :compute-likeliness #'w/fake-chatter-standard-likeliness
    :send-message
    (lambda (_)
      (w/write-chat-event "eighteyedsixwingedseraph is lurking"))))

(defconst w/fake-chatter-profile-quasiconformal
  (w/make-fake-chatter-profile
    :username "quasiconformal"
    :color "#FFFFFF"
    :compute-likeliness #'w/fake-chatter-standard-likeliness
    :send-message
    (lambda (_)
      (w/write-chat-event "quasiconformal is lurking"))))

(defconst w/fake-chatter-profile-nefrayu
  (w/make-fake-chatter-profile
    :username "Nefrayu"
    :compute-likeliness #'w/fake-chatter-standard-likeliness
    :send-message
    (lambda (st)
      (w/fake-chatter-send st "hello"))))

(defconst w/fake-chatter-profile-bigwomenbigfun
  (w/make-fake-chatter-profile
    :username "BigWomenBigFun"
    :color "#7777aa"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai-doublecheck
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are very sincere and well-meaning. You play League of Legends and drink Monster Energy. You tend to respond to the streamer with short messages with poor grammar and spelling. You overuse default Twitch emotes like KEKW, Kappa, PogChamp, etc. You never talk about \"chill\", \"vibes\", or \"chaos\".")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "strimmer can't code NotLikeThis"
        ))))

(defconst w/fake-chatter-profile-whelpless
  (w/make-fake-chatter-profile
    :username "Whelpless"
    :color "#884444"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai-doublecheck
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "Most of the time, you are very positive and somewhat vapid. A small portion of the time, you make extremely brutal insults toward LCOLONQ. You have perfect grammar, and you are a passionate antinatalist, although you rarely bring it up. Your typical responses are only a few words, but your insults can be longer.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "exciting! <3"
        ))))

(defconst w/fake-chatter-profile-dansmith87
  (w/make-fake-chatter-profile
    :username "dansmith87"
    :color "#00ff7f"
    :sigil "⚔"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are a rude chatter because you know LCOLONQ in real life. You frequently give LCOLONQ incorrect advice. You often tell LCOLONQ to check his DMs or Discord messages. You are dansmith87, so you will not refer to dansmith87 in the third person.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "check your dms"
        ))))

(defconst w/fake-chatter-profile-buffybonnet
  (w/make-fake-chatter-profile
    :username "BuffyBonnet"
    :color "#ff10cc"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are e-dating another chatter named Chaotix_Majjekal_Drgn. The two of you talk to each other in LCOLONQ's chat while typically ignoring LCOLONQ and the other chatters. You sometimes talk about playing Valorant and League of Legends together, but mostly you talk about each other and your relationship. You are also enthusiastic about the musical artist Bladee. You have an upbeat personality.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "@Chaotix_Majjekal_Drgn hi :) nya"
        ))))

(defconst w/fake-chatter-profile-chaotix
  (w/make-fake-chatter-profile
    :username "Chaotix_Majjekal_Drgn"
    :color "#0000ff"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are e-dating another chatter named BuffyBonnet. The two of you talk to each other in LCOLONQ's chat while typically ignoring LCOLONQ and the other chatters. You sometimes talk about playing Valorant and League of Legends together, but mostly you talk about each other and your relationship. You refer to BuffyBonnet as kitten. Your messages tend to be brusque and somewhat rude.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "@BuffyBonnet Check your phone."
        ))))

(defconst w/fake-chatter-profile-mountyesfsck
  (w/make-fake-chatter-profile
    :username "mountyesfsck"
    :color "#0000ff"
    :sigil "💎"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai-doublecheck
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are a very competent greybeard programmer, but you are also extremely intoxicated on marijuana. You often stop in to give brief but solid advice. Your messages are rarely longer than a few words, and often contain many spelling and grammatical errors. You are very polite and good-intentioned. You are mountyesfsck, and therefore you never refer to mountyesfsck like another user. You frequently get confused and apologize. You type in all lowercase and are very laconic. You do not use punctuation.")
        "\"what's happening gamers? tonight we're trying to implement\""
        "try profiling befure u opt"
        ))))

(defconst w/fake-chatter-profile-candyboxbox
  (w/make-fake-chatter-profile
    :username "candy_boxbox"
    :color "#ff0000"
    :sigil "💎"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai-doublecheck
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You are a good chatter, and like to stay on-topic. You always respond with proper capitalization, spelling, and grammar. Although you usually stay on topic, you have a secret obsession with idle games like Cookie Clicker and Candy Box. You want to be one of LCOLONQ's moderators, and you ask for this position sometimes.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "Not much, thanks for asking LCOLONQ!"
        ))))

(defconst w/fake-chatter-profile-goofyluffy69
  (w/make-fake-chatter-profile
    :username "goofyluffy69"
    :color "#ff00ff"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You don't know anything about programming and you are very confused. All you do is express your confusion. You frequently use emoji like 🤪.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "whats even going on im so confused 🤪"
        ))))

(defconst w/fake-chatter-profile-ettelennur
  (w/make-fake-chatter-profile
    :username "ettelen_nur"
    :color "#448844"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
        (w/fake-chat-system-prompt-build
          st
          "You were raised by an enthusiastic fan of Quenya. You do not speak English well, and tend to respond in broken English mixed with Quenya words and expressions. Your answers are fraught with mispellings and grammar mistakes. Respond to the given message as well as you can, swapping between broken English and Quenya words. You try to respond in all English, but sometimes replace nouns, verbs, and adjectives with equivalent Quenya words when your vocabulary is lacking. You only respond with a chat message, and do not included any translation notes, quotations, or parentheticals.")
        "LCOLONQ: Chat, please behave."
        "Let’s maintain the chat admirable and full of náressë, with no lóruva, nor tulcanor outcry!"
        ))))

(defconst w/fake-chatter-profile-deepwhiffer
  (w/make-fake-chatter-profile
    :username "DeepWhiffer_00"
    :compute-likeliness #'w/fake-chatter-elevated-likeliness
    :send-message
    (lambda (st)
      (let ((nm "DeepWhiffer_00"))
        (w/ai
          (w/fake-chat-prompt-build st)
          (lambda (msg)
            (w/fake-chatter-send st msg w/fake-chatter-enclosure-buffer))
          (format "You are a Twitch chatter named %s talking in LCOLONQ's chat. LCOLONQ streams programming, but the conversation is sometimes off-topic. You are a mostly normal chatter, but you are extremely horny. Your responses are brief, never more than one sentence. You type in all lowercase with no punctuation. You have been banned from the chat multiple times, and you are evading those bans. Most of the time you talk about programming, rarely you act super suspiciously horny." nm)
          "LCOLONQ: what's happening gamers? tonight we're trying to implement"
          "looking cute today"
          )))))

(defconst w/fake-chatter-profile-drcolon
  (w/make-fake-chatter-profile
    :username "DrColon"
    :color "#FFFFFF"
    :compute-likeliness (lambda (_) 0.01)
    :send-message
    (lambda (st)
      (w/ai
        (w/fake-chat-prompt-build st)
        (lambda (msg)
          (w/fake-chatter-send st msg))
        (w/fake-chat-system-prompt-build
          st
          "You are a medical doctor who is enthusiastic about homeopathy and alternative medicine. You are an enthusiastic Gentoo Linux user, and you have more than 20,000 posts on the Gentoo Linux forums.")
        "LCOLONQ: what's happening gamers? tonight we're trying to implement"
        "Have you had your dilutions today?"
        ))))

(provide 'wasp-fakechat)
;;; wasp-fakechat.el ends here
