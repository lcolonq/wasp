;;; wasp-twitch-chat-commands --- Twitch chat commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'soundboard)
(require 'wasp-twitch)
(require 'wasp-ai)
(require 'wasp-8ball)

;; gizmos
(require 'wasp-pronunciation)

(setq
 w/twitch-chat-commands
 (list
  (cons
   "!commands"
   (lambda (_ _)
     (w/twitch-say
      (s-concat
       "Available commands: "
       (s-join " " (--filter (s-contains? "!" it) (-map #'car w/twitch-chat-commands)))))))
  (cons "MRBEAST" (lambda (_ _) (soundboard//play-clip "mrbeast.mp3")))
  (cons "NICECOCK" (lambda (_ _) (soundboard//play-clip "pantsintoashes.mp3")))
  (cons "hexadiCoding" (lambda (_ _) (soundboard//play-clip "developers.ogg")))
  (cons "ProgrammingTime" (lambda (_ _) (soundboard//play-clip "emacslisp.ogg")))
  (cons
   "roguelike"
   (lambda (user _)
     (w/twitch-say
      (if (= 0 (random 20))
          (format "@%s that is a roguelike :3" user)
        (format "@%s that's not a roguelike" user)))))
  (cons "arch btw" (lambda (_ _) (w/twitch-say "I use nix btw")))
  (cons "!discord" (lambda (_ _) (w/twitch-say "https://discord.gg/f4JTbgN7St")))
  (cons "discord IRC" (lambda (_ _) (w/twitch-say "https://discord.gg/f4JTbgN7St")))
  (cons "discord irc" (lambda (_ _) (w/twitch-say "https://discord.gg/f4JTbgN7St")))
  (cons "Discord IRC" (lambda (_ _) (w/twitch-say "https://discord.gg/f4JTbgN7St")))
  (cons "Discord irc" (lambda (_ _) (w/twitch-say "https://discord.gg/f4JTbgN7St")))
  (cons "Joel" (lambda (_ _) (cl-incf w/chat-joel-count) (w/chat-update-header-line)))
  (cons "+2" (lambda (_ _) (cl-incf w/chat-plus2-count) (w/chat-update-header-line)))
  (cons "-2" (lambda (_ _) (cl-incf w/chat-minus2-count) (w/chat-update-header-line)))
  (cons "ICANT" (lambda (_ _) (cl-incf w/chat-icant-count) (w/chat-update-header-line)))
  (cons "bpm" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "BPM" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "heartrate" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "Heartrate" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "heart" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "Heart" (lambda (_ _) (cl-incf w/chat-bpm-count)))
  (cons "!irc" (lambda (_ _) (w/twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))
  (cons "IRC" (lambda (_ _) (w/twitch-say "#cyberspace on IRC at colonq.computer:26697 (over TLS)")))

  (cons "!today" (lambda (_ _) (w/twitch-say (s-trim (w/slurp "~/today.txt")))))
  (cons "!schedule" (lambda (_ _) (w/twitch-say "https://twitch.tv/LCOLONQ/schedule")))
  (cons "!bingo" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer/~prod/toy/bingo/")))
  (cons
   "!music"
   (lambda (_ _) (w/twitch-say "https://www.youtube.com/playlist?list=PLQ_Vw7ACol3CN58_osDkbeKa14Hk-N-TZ")))
  (cons
   "!fish"
   (lambda (_ _)
     (w/twitch-say (shell-command-to-string "fishing"))))
  (cons "!nc" (lambda (_ _) (w/twitch-say "try: \"nc colonq.computer 31340\", if nc doesn't work try ncat or telnet")))
  (cons
   "!oomfie"
   (lambda (_ _)
      (soundboard//play-clip "oomfie.ogg")
      (w/twitch-say "hi!!!!!!!")))
  (cons "!helloiloveyou" (lambda (_ _) (w/twitch-say "hello i love you")))
  (cons "!pronunciation" (lambda (_ _) (w/twitch-say (w/pronuciation))))
  ;; (cons "!jetsWave" (lambda (_ _) (fig//twitch-say (fig/slurp "jetsWave.txt"))))
  ;; (cons "!forth" (lambda (_ _) (fig//twitch-say "https://github.com/lcolonq/giving")))
  (cons "!oub" (lambda (_ _) (w/twitch-say "https://oub.colonq.computer")))
  (cons "!cellar" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer/~llll/cellar/index.html")))
  (cons "!game" (lambda (_ _) (w/twitch-say "https://oub.colonq.computer")))
  (cons "!voidstranger" (lambda (_ _) (w/twitch-say "https://store.steampowered.com/app/2121980/Void_Stranger/")))
  (cons "!pubnix" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer")))
  (cons "!ring" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer")))
  (cons "!webring" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer")))
  (cons "!animeguide" (lambda (_ _) (w/twitch-say "https://nixos-and-flakes.thiscute.world/introduction")))
  (cons "!tsuki" (lambda (_ _) (w/twitch-say "https://forum.tsuki.games")))
  (cons "!sponsor" (lambda (_ _) (w/twitch-say "Like what you see? Don't forget to download GNU Emacs at https://www.gnu.org/software/emacs/?code=LCOLONQ")))
  (cons "!specs" (lambda (_ _) (w/twitch-say "Editor: evil-mode, WM: EXWM, OS: NixOS, hardware: shit laptop")))
  (cons "!coverage" (lambda (_ _) (w/twitch-say (format "Test coverage: %s%%" (random 100)))))
  (cons "!learnprogramming" (lambda (_ _) (w/twitch-say "1) program")))
  (cons "!github" (lambda (_ _) (w/twitch-say "https://github.com/lcolonq")))
  (cons "!language" (lambda (_ _) (w/twitch-say "probably emacs lisp or maybe rust")))
  (cons "!onlyfans" (lambda (_ _) (soundboard//play-clip "pornhub.mp3")))
  (cons "!throne" (lambda (_ _) (w/twitch-say "xdding")))
  (cons "!vim" (lambda (_ _) (w/twitch-say "vi is the best text editor, emacs is the best operating system")))
  (cons "!emacs" (lambda (_ _) (w/twitch-say "i've tried everything else emacs is best girl")))
  (cons "!bells" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer/~bezelea/bells/ and https://pub.colonq.computer/~prod/toy/dbkai/")))
  (cons "!help" (lambda (_ _) (w/twitch-say "https://pub.colonq.computer/~prod/toy/glossary/")))
  (cons
   "!boost"
   (lambda (user _)
     (w/twitch-say (format "boost power for @%s: %s" user (alist-get :boost w/user-current)))))
  (cons
   "!faction"
   (lambda (user _)
     (w/twitch-say (format "faction for %s: %s" user (alist-get :faction w/user-current)))))
  (cons "!thanks" (lambda (user _) (w/twitch-say (format "thank you %s!" user))))
  (cons "!bible" (lambda (_ _) (w/twitch-say "https://www.youtube.com/watch?v=G5u23bh29hI")))
  (cons "!drink" (lambda (_ _) (w/twitch-say "its watah im drinkin it")))
  (cons
   "!lore"
   (lambda (_ _)
     (w/ai
      "ITEM"
      (lambda (msg) (w/twitch-say msg))
      "Please produce a Dark Souls style item name and description related to LCOLONQ. Please limit your response to one sentence maximum. The sentence should be vague and incorporate archaic words that are not commonly used. LCOLONQ is a spirit that lives inside the computer. LCOLONQ is associated with: the moon, snakes, the color grey, dolls and puppets, amber, the wind, and GNU Emacs. The description should mostly describe the item, but with vague insinuations about the true nature of LCOLONQ."
      "ITEM"
      "Ring of Favor and Protection - A ring symbolizing the favor and protection of the goddess Fina, known in legend to possess fateful beauty.")))
  ;; (cons "!geisercounter" (lambda (_ _) (fig//twitch-say (format "The Geiser counter beeps %s times" (fig//geiser-counter)))))
  (cons
   "!8ball"
   (lambda (user inp)
     (let ((trimmed (s-trim (s-replace "!8ball" "" inp))))
       (w/8ball
        trimmed
        (lambda (answer)
          (w/twitch-say (format "@%s 8ball says: %s" user answer)))))))
  (cons
   "!bookrec"
   (lambda (_ _)
     (w/user-get
      "__books__"
      (lambda (books)
        (let ((choice (w/pick-random books)))
          (w/twitch-say (format "%s (recommended by %s)" (car choice) (cdr choice))))))))
  (cons
   "!quote"
   (lambda (_ _)
     (w/user-get
      "__quotes__"
      (lambda (books)
        (let ((choice (w/pick-random books)))
          (w/twitch-say (format "%s: %s" (cdr choice) (car choice))))))))
  (cons
   "!leaderboard"
   (lambda (_ _)
     (let* ((user-scores (-filter #'cdr (--map (when (and (listp it) (listp (cdr it))) (cons (car it) (alist-get :boost (cdr it)))) (ht->alist w/user-cache))))
            (sorted (-sort (-on #'> #'cdr) user-scores))
            (leaders (-take 5 sorted)))
       (w/twitch-say (s-join ", " (--map (format "%s: %s" (car it) (cdr it)) leaders))))))
  (cons
   "draobredael!"
   (lambda (_ _)
     (let* ((user-scores (-filter #'cdr (--map (when (and (listp it) (listp (cdr it))) (cons (car it) (alist-get :boost (cdr it)))) (ht->alist w/user-cache))))
            (sorted (-sort (-on #'< #'cdr) user-scores))
            (leaders (-take 5 sorted)))
       (w/twitch-say (s-join ", " (--map (format "%s: %s" (reverse (car it)) (cdr it)) leaders))))))
  (cons
   "!resolution"
   (lambda (user inp)
     (let ((trimmed (s-trim (s-replace "!resolution" "" inp))))
       (if (string-empty-p trimmed)
           (w/write-chat-event "You gotta put what your resolution is.")
         (w/write-chat-event (format "%s RESOLVES: %s" (s-upcase user) trimmed))
         (setf (alist-get :resolution w/user-current) trimmed)))))
  ;; (cons "!addbookrec"
  ;;       (lambda (user inp)
  ;;         (let ((trimmed (s-trim (s-replace "!addbookrec" "" inp))))
  ;;           (fig//write-chat-event (format "%s recommends: %s" user trimmed))
  ;;           (fig//add-recommended-book user trimmed))))
  ;; (cons "!quote"
  ;;       (lambda (_ _)
  ;;         (let ((choice (nth (random (length fig/quotes)) fig/quotes)))
  ;;           (fig//twitch-say (format "%s: %s" (cdr choice) (car choice))))))
  ;; (cons "!addquote"
  ;;       (lambda (user inp)
  ;;         (let ((trimmed (s-trim (s-replace "!addquote" "" inp))))
  ;;           (fig//write-chat-event (format "%s saves quote: %s" user trimmed))
  ;;           (fig//add-quote user trimmed))))
  ;; (cons "!twitter"
  ;;       (lambda (_ _)
  ;;         (fig/ask "How do you feel about Twitter? Should viewers follow LCOLONQ on Twitter?" #'fig/say)
  ;;         (fig//twitch-say "https://twitter.com/LCOLONQ")))
  ;; ;; (cons "!aoc" (lambda (_ _) (fig//twitch-say "Join our leaderboard: 3307583-b61f237c")))
  ;; (cons "!roll" (lambda (user _) (fig//twitch-say (fig//character-to-string (fig//roll-character user)))))
  ;; (cons
  ;; (cons
  ;;  "!vippers"
  ;;  (lambda (_ _)
  ;;    (let ((vipperstring (s-join ", " (fig//shuffle-seq fig//twitch-vip-list))))
  ;;      (fig//twitch-say (seq-take vipperstring 450)))
  ;;    (fig//twitch-get-vip-list)))
  ;; (cons "!levelup"
  ;;       (lambda (user _)
  ;;         (fig//update-db-character
  ;;          user
  ;;          (lambda (c)
  ;;            (cl-incf (fig//rpg-character-level c))
  ;;            c))
  ;;         (fig//twitch-say (fig//character-to-string (fig//get-db-character user)))))
  ))

(provide 'wasp-twitch-chat-commands)
;;; wasp-twitch-chat-commands.el ends here
