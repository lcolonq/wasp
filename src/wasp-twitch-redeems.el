;;; wasp-twitch-redeems --- Twitch redeems -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'soundboard)
(require 'wasp-twitch)
(require 'wasp-model)
(require 'wasp-obs)
(require 'wasp-user)
(require 'wasp-friend)
(require 'bezelea-muzak)

(setf
 w/twitch-redeems
 (list
  (list
   "BOOST" 1
   (lambda (user _)
     (soundboard//play-clip "yougotboostpower.ogg")
     (w/write-chat-event (s-concat user " boosted their boost number"))
     (cl-incf (alist-get :boost w/user-current 0))))
  (list
   "TSOOB" 1
   (lambda (user _)
     (soundboard//play-clip "rewoptsoobtoguoy.ogg" 140)
     (w/write-chat-event (s-reverse (s-concat user " boosted their boost number")))
     (cl-decf (alist-get :boost w/user-current 0))))
  (list
   "submit headline" 1
   (lambda (user inp)
     (w/write-chat-event (format "%s submitted a headline: %s" user inp))
     (w/friend-journalism user inp)))
  (list
   "spinne" 3
   (lambda (user _)
     (w/write-chat-event (s-concat user " activates the spinne cyclle"))
     (w/model-toggle "spin")))
  (list
   "forsen" 3
   (lambda (_ _)
     (soundboard//play-clip "cave3.ogg" 75)
     (w/model-toggle "forsen")))
  (list "SEASICKNESS GENERATOR" 3 (lambda (_ _) (w/model-toggle "zoom_wave")))
  (list
   "pursue idol dream" 3
   (lambda (user _)
     (w/write-chat-event (format "Helping %s pursue their idol dream~" user))
     (w/obs-activate-toggle 'chase-dreams)
     (w/model-region-user-avatar "hair" user)))
  (list
   "bells of bezelea" 4
   (lambda (user msg)
     (muzak//get-song
      msg
      (lambda (song)
        (if song
            (progn
              (w/write-chat-event (format "%s played a song: %s (sponsored by Bezelea)" user msg))
              (muzak/play-song msg))
          (w/write-chat-event (format "%s played the bells (sponsored by Bezelea)" user))
          (muzak/play-tracks msg))))))
  (list "palette swap (hair)" 5 (w/handle-redeem-region-swap "hair"))
  (list "palette swap (highlight)" 5 (w/handle-redeem-region-swap "highlight"))
  (list "palette swap (eyes)" 5 (w/handle-redeem-region-swap "eyes"))
  (list "palette swap (hat)" 5 (w/handle-redeem-region-swap "hat"))
  (list
   "feed friend" 10
   (lambda (user inp)
     (w/write-chat-event (s-concat user " feeds \"friend\" " inp))
     (w/friend-feed user inp)))
  (list
   "talk to friend" 10
   (lambda (user inp)
     (w/write-chat-event (s-concat user " talks to \"friend\": " inp))
     (w/friend-chat user inp)))
  (list
   "gamer" 500
   (lambda (user _)
     (w/write-chat-event (s-concat user " quickscoped me"))
     (soundboard//play-clip "videogame.ogg")
     (w/obs-activate-toggle 'thug-life)))
  (list
   "arrow" 500
   (lambda (user msg)
     (w/write-chat-event (format "%s points and says %S" user msg))
     (w/obs-activate-toggle 'clickbait msg)))
  (list
   "super idol" 500
   (lambda (_ _)
     (w/twitch-say "SuperIdoldexiaorongdoumeinidetianbayuezhengwudeyangguangdoumeiniyaoyanreai105Cdenididiqingchundezhen")
     (soundboard//play-clip "superidol.mp3")))
  ))

(provide 'wasp-twitch-redeems)
;;; wasp-twitch-redeems.el ends here
