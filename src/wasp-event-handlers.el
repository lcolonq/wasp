;;; wasp-event-handlers --- Event handlers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'soundboard)
(require 'wasp-bus)
(require 'wasp-twitch)
(require 'wasp-friend)
(require 'wasp-model)

(setf
 w/bus-event-handlers
 (list
  (cons '(monitor twitch chat incoming) #'w/twitch-handle-incoming-chat)
  (cons '(monitor twitch redeem incoming) #'w/twitch-handle-redeem)
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
       ;; (soundboard//play-clip "firstblood.mp3")
       ;; (w/model-region-word "skin" (format "welcome_%s_" user))
       (w/friend-respond (format "%s just followed the stream" user))
       ;; (w/write-chat-event (format "New follower: %s" user))
       )
     ))
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
  ))

(provide 'wasp-event-handlers)
;;; wasp-event-handlers.el ends here
