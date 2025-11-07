;;; wasp-gdq --- Automatic GDQ Donation Messages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)
(require 'wasp-utils)
(require 'wasp-chat)
(require 'wasp-ai)
(require 'wasp-audio)

(defvar w/gdq-usernames
  (list
   "SpeedyGonzales42"
   "Bezelea"
   "Hat_Knight"
   "DanktownBunny"
   "DrawThatRedstone"
   "han_bun_"
   "neunmalelf"
   "vesdeg"
   "GenDude"
   "nem_dev"
   "LederHosenCowboy"
   "vasher_1025"
   "mickynoon"
   "TF_TOKYO"
   "Setolyx"))

(defvar w/gdq-templates
  (list
    "We have a $100 dollar donation from goofyluffy69"
    "Greetings from Germany"
    "First time donator"
    "Long time watcher"
    "I'm happy to donate to such a great cause"
    "I am donating because my son just died after a long battle with cancer."
    "WarioWare Smooth Moves has always been my favorite childhood game and I love seeing it get destroyed."
    "Keep up the good work."
    "This goes to naming the Starfield file cumboy"
    "HYYYYYYYYPE!"
    "Much love to the runner and the couch"
    "Save the frames!"
    "Save the animals!"
    "Kill the frames!"
    "Kill the animals!"))

(defun w/gdq-generate-username (k)
  "Generate a random GDQ username and pass it to K."
  (w/ai
   "go"
   (lambda (msg)
     (funcall k msg))
   "Generate a random username that might be used by a Games Done Quick donator."
   (list "go" "go" "go")
   (take 3 (w/shuffle w/gdq-usernames))))

(defun w/gdq-generate-message (user k)
  "Generate a random GDQ-style donation message for USER and pass it to K."
  (let ((exuser (nth (random (length w/gdq-usernames)) w/gdq-usernames))
        (extemplate (s-join " " (-take 3 (w/shuffle w/gdq-templates)))))
    (w/ai
     user
     (lambda (msg)
     (funcall k msg))
     "Produce a Games Done Quick style donation message from the given username. The format should be \"username|donation amount|message. message. message.\". The message should be between 2 and 5 sentences. The message should be longer than a single sentence."
     exuser
     (format "%s|$%s|%s" exuser (* 25 (random 41)) extemplate))))

(defun w/ldq ()
  "LCOLONQ Done Quickly."
  (w/gdq-generate-username
   (lambda (user)
     (w/gdq-generate-message
      user
      (lambda (s)
        (let ((sp (s-split "|" s)))
          (w/chat-write-event (format "%s donated %s!" (car sp) (cadr sp)))
          (w/tts
           (format
            "%s donated %s with the message: %s"
            (car sp)
            (cadr sp)
            (caddr sp)))))))))

(provide 'wasp-gdq)
;;; wasp-gdq.el ends here
