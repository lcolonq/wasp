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
        (-let [(user stags msg) (s-split-up-to " " (w/utf8 d) 2)]
          (w/twitch-handle-incoming-chat
            user
            (--map (s-split "\t" it) (s-split "\n" stags))
            msg))))
    (cons "frontend redeem incoming"
      (lambda (d)
        (-let [(user redeem input) (s-split-up-to "\t" (w/utf8 d) 2)]
          (w/twitch-handle-redeem-helper user redeem input 1000))))
    ;; (cons "test event 2"
    ;;   (lambda (d)
    ;;     (message "incoming: %s" d)))
    ))

(provide 'wasp-event-handlers-binary)
;;; wasp-event-handlers-binary.el ends here
