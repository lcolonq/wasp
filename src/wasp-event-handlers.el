;;; wasp-event-handlers --- Event handlers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'soundboard)
(require 'wasp-bus)
(require 'wasp-twitch)
(require 'wasp-friend)
(require 'wasp-model)
(require 'wasp-overlay)

(setf
  w/bus-event-handlers
  (list
    (cons
      '(monitor lamulana test)
      (lambda (data)
        (w/write-chat-event (format "LA-MULANA says hi: %s" data))))
    (cons
      '(monitor nethack test)
      (lambda (data)
        (w/write-chat-event (format "Nethack says hi: %s" data))))
    (cons
      '(monitor nethack monster)
      (lambda (data)
        (when-let ((sp (s-split " " (car data))))
          (setf planet/last-monster (cons (s-join " " (cdr sp)) (string-to-number (car sp))))
          (planet/render-monster-summary))))
    (cons
      '(overlay barrage started)
      (lambda (_)
        (w/write-chat-event "It begins...")
        (setf w/overlay-barrage-active t)
        (w/overlay-update-cursor)))
    (cons
      '(overlay barrage ended)
      (lambda (msg)
        (cond
          ((s-equals? (car msg) "won") (w/write-chat-event "Fufufu... I win..."))
          (t (w/write-chat-event "that it's over")))
        (setf w/overlay-barrage-active nil)))
    (cons '(monitor twitch chat incoming) #'w/twitch-handle-incoming-chat)
    (cons '(monitor twitch redeem incoming) #'w/twitch-handle-redeem)
    (cons
      '(frontend redeem incoming)
      (lambda (msg)
        (w/twitch-handle-redeem-api msg)
        ))
    (cons
      '(monitor twitch raid)
      (lambda (msg)
        (let ((user (car msg)))
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
    (cons
      '(monitor twitch follow)
      (lambda (msg)
        (let ((user (car msg)))
          (soundboard//play-clip "firstblood.mp3")
          (w/model-region-word "skin" (format "welcome_%s_" user))
          (w/friend-respond (format "%s just followed the stream" user))
          (w/write-chat-event (format "New follower: %s" user)))))
    (cons
      '(monitor twitch subscribe)
      (lambda (msg)
        (let ((user (car msg)))
          (w/thank-sub user)
          (w/model-region-word "skin" (format "thanks_%s_" user))
          (w/friend-respond (format "%s just subscribed to the stream" user))
          (w/write-chat-event (format "New subscriber: %s" user)))))
    (cons
      '(monitor twitch gift)
      (lambda (msg)
        (let ((user (car msg))
               (subs (cadr msg)))
          (unless (s-equals? user "lcolonq")
            (w/model-region-word "skin" (format "thanks_%s_" user))
            (w/friend-respond (format "%s just gifted subscriptions" user))
            (w/write-chat-event (format "%s gifted %d subs" user subs))
            (soundboard//play-monsterkill subs)))))
    (cons
      '(monitor twitch poll begin)
      (lambda (_)
        (w/write-chat-event "Poll started")
        (w/friend-respond "The chatters are doing a poll")))
    (cons
      '(monitor twitch poll end)
      (lambda (msg)
        (let ((winner (car (-max-by (-on #'> #'cadr) (cadr msg)))))
          (w/write-chat-event (format "Poll finished, winner is: %s" winner))
          (when w/twitch-current-poll-callback
            (funcall w/twitch-current-poll-callback winner))
          (setq w/twitch-current-poll-callback nil))))
    (cons
      '(monitor twitch prediction begin)
      (lambda (msg)
        (w/write-chat-event "Gamble started")
        (w/friend-respond "The chatters are gambling")
        (setq w/twitch-current-prediction-ids msg)))
    (cons
      '(monitor twitch prediction end)
      (lambda (_)
        (w/write-chat-event "Gamble finished")
        (setq w/twitch-current-prediction-ids nil)))
    ;; (cons
    ;;  '(monitor discord chat incoming)
    ;;  (lambda (data)
    ;;    (let ((user (w/decode-string (cadr data)))
    ;;          (msg (w/decode-string (cadddr data))))
    ;;    (w/write-chat-event (format "discord from %s: %s" user msg)))))
    ))

(provide 'wasp-event-handlers)
;;; wasp-event-handlers.el ends here
