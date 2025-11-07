;;; wasp-event-handlers-binary --- Event handlers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-utils)
(require 'wasp-twitch)
(require 'wasp-friend)
(require 'wasp-model)
(require 'wasp-bus-binary)

(setf
  w/bus-binary-event-handlers
  (list
    (cons "fig monitor twitch chat incoming"
      (lambda (d)
        (-let [(user stags msg) (s-split-up-to " " (w/utf8 d) 2)]
          (w/twitch-handle-incoming-chat
            user
            (--map (s-split "\t" it) (s-split "\n" stags))
            msg))))
    (cons "fig monitor twitch redeem incoming"
      (lambda (d)
        (-let [(user redeem input) (s-split-up-to "\t" (w/utf8 d) 2)]
          (w/twitch-handle-redeem-helper user redeem input))))
    (cons "fig web redeem incoming"
      (lambda (d)
        (-let [(user redeem input) (s-split-up-to "\t" (w/utf8 d) 2)]
          (w/twitch-handle-redeem-helper user redeem input 1000))))
    (cons "fig monitor twitch raid"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (soundboard//play-clip "rampage.mp3")
          (w/chat-write-event (format "%s just raided!" user))
          (w/friend-respond (format "%s just came to visit" user))
          (run-with-timer
            15 nil
            (lambda ()
              (w/twitch-get-user-recent-clips
                user
                (lambda (clips)
                  (w/model-region-word "hair" (s-concat user "_"))
                  (w/model-region-word "eyes" "WELCOME")
                  (if clips
                    (w/model-region-video "hair" (car clips))
                    (w/model-region-user-avatar "hair" user)))))))))
    (cons "fig monitor twitch follow"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (soundboard//play-clip "firstblood.mp3")
          (w/model-region-word "skin" (format "welcome_%s_" user))
          (w/friend-respond (format "%s just followed the stream" user))
          (w/chat-write-event (format "New follower: %s" user)))))
    (cons "fig monitor twitch subscribe"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (w/thank-sub user)
          (w/model-region-word "skin" (format "thanks_%s_" user))
          (w/friend-respond (format "%s just subscribed to the stream" user))
          (w/chat-write-event (format "New subscriber: %s" user)))))
    (cons "fig monitor twitch gift"
      (lambda (d)
        (-let [(user subs) (s-split-up-to " " (w/utf8 d) 2)]
          (unless (s-equals? user "lcolonq")
            (w/model-region-word "skin" (format "thanks_%s_" user))
            (w/friend-respond (format "%s just gifted subscriptions" user))
            (w/chat-write-event (format "%s gifted %d subs" user subs))
            (soundboard//play-monsterkill subs)))))
    (cons "fig monitor twitch poll begin"
      (lambda (_)
        (w/chat-write-event "Poll started")
        (w/friend-respond "The chatters are doing a poll")))
    (cons "fig monitor twitch poll end"
      (lambda (d)
        (let*
          ( (sp (s-split "\n" (w/utf8 d)))
            (choices (--map (-let [(o v) (s-split "\t" it)] (cons o (string-to-number v))) (cdr sp)))
            (winner (car (-max-by (-on #'> #'cdr) choices))))
          (w/chat-write-event (format "Poll finished, winner is: %s" winner))
          (when w/twitch-current-poll-callback
            (funcall w/twitch-current-poll-callback winner))
          (setq w/twitch-current-poll-callback nil))))
    (cons "fig monitor twitch prediction begin"
      (lambda (d)
        (w/chat-write-event "Gamble started")
        (w/friend-respond "The chatters are gambling")
        (setq w/twitch-current-prediction-ids (w/utf8 d))))
    (cons "fig monitor twitch prediction end"
      (lambda (_)
        (w/chat-write-event "Gamble finished")
        (setq w/twitch-current-prediction-ids nil)))
    ))

(provide 'wasp-event-handlers-binary)
;;; wasp-event-handlers-binary.el ends here
