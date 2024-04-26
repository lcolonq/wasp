;;; wasp-twitch-redeems --- Twitch redeems -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'soundboard)
(require 'wasp-utils)
(require 'wasp-twitch)
(require 'wasp-model)
(require 'wasp-obs)
(require 'wasp-user)
(require 'wasp-friend)
(require 'wasp-dna)
(require 'wasp-fakechat)
(require 'bezelea-muzak)

(setf
 w/twitch-redeems
 (list
  (list
   "mental clarity" 1
   (lambda (user _)
     (w/write-chat-event (format "%s established mental clarity" user))
     (w/stop-all-audio)))
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
   "talk to clone" 2
   (lambda (user inp)
     (push (cons user inp) w/twitch-chat-history)
     (let ((w/twitch-chat-history (list (cons user inp)))
           (w/last-stream-transcription "")
           (ping (w/message-ping inp)))
       (w/write-chat-event (s-concat user " asks clone: " inp))
       (w/fake-chatter-run
        (w/pick-random
         (or
          (and ping (w/dna-user-clones ping))
          (w/dna-user-clones user)
          w/fake-chatters))))))
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
   "theme: maris-dark" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: maris-dark" user))
     (w/change-theme 'ef-maris-dark)))
  (list
   "theme: autumn" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: autumn" user))
     (w/change-theme 'ef-autumn)))
  (list
   "theme: tritanopia-dark" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: tritanopia-dark" user))
     (w/change-theme 'ef-tritanopia-dark)))
  (list
   "theme: duo-dark" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: duo-dark" user))
     (w/change-theme 'ef-duo-dark)))
  (list
   "theme: bio" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: bio" user))
     (w/change-theme 'ef-bio)))
  (list
   "theme: rosa" 50
   (lambda (user _)
     (w/write-chat-event (format "%s changed the theme: rosa" user))
     (w/change-theme 'ef-rosa)))
  (list
   "gamer" 500
   (lambda (user _)
     (cl-incf w/twitch-gamer-counter)
     (if (not (= 0 (% w/twitch-gamer-counter 5)))
         (w/write-chat-event (s-concat user " offered a sacrifice at the altar of Gaming"))
       (w/write-chat-event (s-concat user "'s Gamer Sacrifice summoned an entity"))
       (soundboard//play-clip "videogame.ogg")
       (w/obs-activate-toggle 'thug-life))))
  (list
   "arrow" 500
   (lambda (user msg)
     (w/write-chat-event (format "%s points and says %S" user msg))
     (w/obs-activate-toggle 'clickbait msg)))
  (list
   "antipiracy" 500
   (lambda (_ _)
     (w/obs-activate-toggle 'activate-nixos)))
  (list
   "super idol" 500
   (lambda (_ _)
     (w/twitch-say "SuperIdoldexiaorongdoumeinidetianbayuezhengwudeyangguangdoumeiniyaoyanreai105Cdenididiqingchundezhen")
     (soundboard//play-clip "superidololdshortstyle.ogg")))
  (list
   "VIPPER" 1000
   (lambda (user inp)
     (soundboard//play-clip "aeiou.ogg")
     (w/write-chat-event (s-concat user " gave VIP to " inp))
     (w/twitch-add-vip (s-chop-prefix "@" inp))))
  (list
   "deVIPPER" 1000
   (lambda (user inp)
     (soundboard//play-clip "uoiea.ogg")
     (w/write-chat-event (s-concat user " removed VIP from " inp))
     (w/twitch-remove-vip (s-chop-prefix "@" inp))))
  (list
   "total clarity" 5000
   (lambda (user _)
     (w/stop-all-audio)
     (run-with-timer
      1 nil
      (lambda () (soundboard//play-clip "gong.ogg")))
     (w/write-chat-event (s-concat user " established total clarity"))
     (w/obs-activate-toggle 'total-clarity)))
  ))

(provide 'wasp-twitch-redeems)
;;; wasp-twitch-redeems.el ends here
