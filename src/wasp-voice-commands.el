;;; wasp-voice-commands --- Voice commands -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'soundboard)
(require 'wasp-audio)
(require 'wasp-twitch)
(require 'wasp-chat)
(require 'wasp-obs)

(setq
 w/stream-transcribe-voice-commands
 (list
  (cons "mr. beast" (lambda () (soundboard//play-clip "mrbeast.mp3")))
  (cons "joel" (lambda () (w/twitch-say (w/pick-random (list "Joel" "EvilJoel")))))
  (cons "i can't" (lambda () (cl-incf w/chat-icant-count) (w/chat-update-header-line)))
  (cons "lua"
        (lambda ()
          (progn (w/obs-toggle-brazil)
                 (run-with-timer 1 nil #'w/obs-toggle-brazil))))
  ))

(provide 'wasp-voice-commands)
;;; wasp-voice-commands.el ends here
