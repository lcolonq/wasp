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
    (cons "monitor twitch chat incoming"
      (lambda (d)
        (message "incoming: %s" d)
        (-let [(user stags msg) (s-split-up-to " " (w/utf8 d) 2)]
          (w/twitch-handle-incoming-chat
            user
            (--map (s-split "\t" it) (s-split "\n" stags))
            msg))))
    (cons "monitor twitch redeem incoming"
      (lambda (d)
        (-let [(user redeem input) (s-split-up-to " " (w/utf8 d) 2)]
          (w/twitch-handle-redeem-helper user redeem input))))
    (cons "frontend redeem incoming"
      (lambda (d)
        (-let [(user redeem input) (s-split-up-to "\t" (w/utf8 d) 2)]
          (w/twitch-handle-redeem-helper user redeem input 1000))))
    (cons "monitor twitch raid"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (soundboard//play-clip "rampage.mp3")
          (w/write-chat-event (format "%s just raided!" user))
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
    (cons "monitor twitch follow"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (soundboard//play-clip "firstblood.mp3")
          (w/model-region-word "skin" (format "welcome_%s_" user))
          (w/friend-respond (format "%s just followed the stream" user))
          (w/write-chat-event (format "New follower: %s" user)))))
    (cons "monitor twitch subscribe"
      (lambda (d)
        (let ((user (w/utf8 d)))
          (w/thank-sub user)
          (w/model-region-word "skin" (format "thanks_%s_" user))
          (w/friend-respond (format "%s just subscribed to the stream" user))
          (w/write-chat-event (format "New subscriber: %s" user)))))
    (cons "monitor twitch gift"
      (lambda (d)
        (-let [(user subs) (s-split-up-to " " (w/utf8 d) 2)]
          (unless (s-equals? user "lcolonq")
            (w/model-region-word "skin" (format "thanks_%s_" user))
            (w/friend-respond (format "%s just gifted subscriptions" user))
            (w/write-chat-event (format "%s gifted %d subs" user subs))
            (soundboard//play-monsterkill subs)))))
    (cons "monitor twitch poll begin"
      (lambda (_)
        (w/write-chat-event "Poll started")
        (w/friend-respond "The chatters are doing a poll")))
    (cons "monitor twitch poll end"
      (lambda (d)
        (let*
          ( (sp (s-split " " (w/utf8 d)))
            (choices (--map (s-split "," it) (cdr sp)))
            (winner (car (-max-by (-on #'> #'cadr) choices))))
          (w/write-chat-event (format "Poll finished, winner is: %s" winner))
          (when w/twitch-current-poll-callback
            (funcall w/twitch-current-poll-callback winner))
          (setq w/twitch-current-poll-callback nil))))
    (cons "monitor twitch prediction begin"
      (lambda (d)
        (w/write-chat-event "Gamble started")
        (w/friend-respond "The chatters are gambling")
        (setq w/twitch-current-prediction-ids (w/utf8 d))))
    (cons "monitor twitch prediction end"
      (lambda (_)
        (w/write-chat-event "Gamble finished")
        (setq w/twitch-current-prediction-ids nil)))
    ))

(provide 'wasp-event-handlers-binary)
;;; wasp-event-handlers-binary.el ends here
