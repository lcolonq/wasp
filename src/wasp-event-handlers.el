;;; wasp-event-handlers --- Event handlers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-bus)
(require 'wasp-twitch)

(setf
 w/bus-event-handlers
 (list
  (cons '(monitor twitch chat incoming) #'w/twitch-handle-incoming-chat)
  (cons '(monitor twitch redeem incoming) #'w/twitch-handle-redeem)
  ))

(provide 'wasp-event-handlers)
;;; wasp-event-handlers.el ends here
