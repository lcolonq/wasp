;;; wasp-event-handlers-binary --- Event handlers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-utils)
(require 'wasp-twitch)
(require 'wasp-bus-binary)

(setf
  w/bus-binary-event-handlers
  (list
    (cons "monitor twitch chat incoming"
      (lambda (d)
        (-let [(user stags msg) (s-split-up-to " " d 2)]
          (w/twitch-handle-incoming-chat
            user
            (--map (s-split "\t" it) (s-split "\n" stags))
            msg)))))
    ))

(provide 'wasp-event-handlers-binary)
;;; wasp-event-handlers-binary.el ends here
