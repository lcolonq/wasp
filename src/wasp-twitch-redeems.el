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
(require 'wasp-overlay)
(require 'wasp-cyclone)
(require 'wasp-bless)
(require 'wasp-flymake)
(require 'wasp-irish)
(require 'wasp-soundboard)

(defvar w/twitch-redeem-sound-last 0)

(setf
  w/twitch-redeems
  (list
    (list
      "submit quote" 1
      (lambda (user quote)
        (ignore user)
        (w/irish-contribute quote)))
    (list
      "throw shade" 1
      (lambda (user shader)
        (w/write-chat-event (format "%s threw shade" user))
        (w/db-set "shader" shader)
        (w/model-record-change)
        (w/overlay-shader user shader)))
    (list
      "spawn" 1
      (lambda (user pattern)
        (w/write-chat-event (format "%s created life" user))
        (w/model-record-change)
        (w/overlay-automata user pattern (alist-get :color w/user-current))))
    (list
      "sound board" 1
      (lambda (user cmd)
        (w/write-chat-event (format "%s played sound: %s" user cmd))
        (w/sfx cmd)))
    (list
      "lurker check in" 1
      (lambda (user _)
        (w/write-chat-event (format "%s is lurking" user))))
    (list
      "mental clarity" 1
      (lambda (user _)
        (w/write-chat-event (format "%s established mental clarity" user))
        (w/stop-all-audio)
        (w/model-reset)))
    (list
      "BOOST" 1
      (lambda (user _)
        (unless (-contains? w/twitch-boosters user)
          (add-to-list 'w/twitch-boosters user)
          (soundboard//play-clip "yougotboostpower.ogg")
          (w/write-chat-event (s-concat user " boosted their boost number"))
          (cl-incf (alist-get :boost w/user-current 0)))))
    (list
      "TSOOB" 1
      (lambda (user _)
        (unless (-contains? w/twitch-tsoobers user)
          (add-to-list 'w/twitch-tsoobers user)
          (soundboard//play-clip "rewoptsoobtoguoy.ogg" 140)
          (w/write-chat-event (s-reverse (s-concat user " boosted their boost number")))
          (cl-decf (alist-get :boost w/user-current 0)))))
    (list
      "submit headline" 1
      (lambda (user inp)
        (w/write-chat-event (format "%s submitted a headline: %s" user inp))
        (w/glossary-record inp)
        (w/friend-journalism user inp)))
    (list
      "cycle gizmos" 1
      (lambda (user _)
        (w/write-chat-event (format "%s cycled the gizmos" user))
        (w/gizmo-cycle)))
    (list
      "allow streamer to drink" 1
      (lambda (user _)
        (w/write-chat-event (format "%s allowed the streamer to \"drink\"" user))))
    (list
      "deslug" 1
      (lambda (user _)
        (w/write-chat-event (format "%s inverted slug" user))))
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
      (lambda (user _)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user " loudly exclaims forsenE"))
            (soundboard//play-clip "cave3.ogg" 75)
            (w/model-toggle "forsen")
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "SEASICKNESS GENERATOR" 3
      (lambda (user _)
        (w/write-chat-event (s-concat user " is a salty sea dog"))
        (w/model-toggle "zoom_wave")))
    (list
      "The Pharaoh's Curse" 3
      (lambda (user _)
        (w/write-chat-event (format "%s drew the ire of the Pharaoh upon me" user))
        (w/obs-activate-toggle 'pharaohs-curse)
        ))
    (list
      "pursue idol dream" 3
      (lambda (user _)
        (w/write-chat-event (format "Helping %s pursue their idol dream~" user))
        (w/obs-activate-toggle 'chase-dreams)
        (w/model-region-user-avatar "hair" user)))
    (list
      "INTJ stare" 3
      (lambda (user _)
        (w/write-chat-event (format "%s suggested a little more sodium chloride next time" user))
        (w/obs-activate-toggle 'intj-stare)))
    (list
      "Live LCOLONQ Reaction" 3
      (lambda (user _)
        (w/write-chat-event (format "%s demanded extremely \"hype\" reactions, &c." user))
        (w/obs-activate-toggle 'live-reaction)))
    (list
      "Live friend Reaction" 3
      (lambda (user _)
        (w/write-chat-event (format "%s demanded extremely \"hype\" reactions, &c. but from \"friend\"!?" user))
        (w/obs-activate-toggle 'live-friend-reaction)))
    (list
      "bells of bezelea" 4
      (lambda (user msg)
        (w/get-song
          msg
          (lambda (song)
            (if song
              (progn
                (w/write-chat-event (format "%s played a song: %s (sponsored by Bezelea)" user msg))
                (w/audio-muzak-enqueue user song))
              (w/write-chat-event (format "%s played the bells (sponsored by Bezelea)" user))
              (w/audio-muzak-enqueue user msg))))))
    (list
      "activate spell card" 4
      (lambda (user msg)
        (w/write-chat-event
          (format
            "%s Sign: \"%s\""
            (s-titleize user)
            (s-titleized-words (alist-get user w/twitch-chat-history (w/pick-random w/overlay-spellcard-names) nil #'cl-equalp))))
        (w/overlay-decode-shorthand-bml
          msg
          (lambda (data)
            (w/overlay-start-barrage data)))))
    (list "palette swap (hair)" 5 (w/handle-redeem-region-swap "hair"))
    (list "palette swap (highlight)" 5 (w/handle-redeem-region-swap "highlight"))
    (list "palette swap (eyes)" 5 (w/handle-redeem-region-swap "eyes"))
    (list "palette swap (hat)" 5 (w/handle-redeem-region-swap "hat"))
    (list "palette swap (hands)" 5 (w/handle-redeem-region-swap "hands"))
    (list "background swap (drawing)" 5
      (lambda (user inp)
        (if (w/user-authorized)
          (progn
            (w/write-chat-event (s-concat user " changes the drawing background: " inp))
            (if (w/allowed-video-url inp)
              (w/binary-pub "background url" inp)
              (w/write-chat-event (format "%s is not a recognized video site" inp))))
          (w/write-chat-event (format "%s is not authorized to change video" user)))))
    (list
      "run program" 6
      (lambda (user inp)
        (if (w/user-authorized)
          (progn
            (w/write-chat-event (s-concat user " runs program: " inp))
            (w/bless inp 50))
          (w/write-chat-event (format "%s is not authorized to run code" user)))))
    (list
      "encoded clarity" 7
      (lambda (user msg)
        (w/write-chat-event (format "%s demands greater program clarity: %s" user msg))
        (with-current-buffer (window-buffer)
          (w/flymake-error user msg))))
    (list
      "feed friend" 10
      (lambda (user inp)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user " feeds \"friend\" " inp))
            (w/friend-feed user inp)
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "talk to friend" 10
      (lambda (user inp)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user " talks to \"friend\": " inp))
            (w/friend-respond (format "%s says: %s" user inp))
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "friend composes song" 10
      (lambda (user inp)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user " asks \"friend\" to compose a song about: " inp))
            (w/friend-compose-song inp)
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "show friend wikipedia page" 10
      (lambda (user inp)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user " shows \"friend\" a Wikipedia page: " inp))
            (w/friend-react-wikipedia user inp)
            (setq w/twitch-redeem-sound-last cur)))))
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
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event (s-concat user "'s Gamer Sacrifice summoned an entity"))
            (soundboard//play-clip "videogame.ogg")
            (w/obs-activate-toggle 'thug-life)
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "arrow" 500
      (lambda (user msg)
        (w/write-chat-event (format "%s points and says %S" user msg))
        (w/obs-activate-toggle 'clickbait msg)))
    (list
      "antipiracy" 500
      (lambda (user _)
        (w/write-chat-event (format "%s does not condone any form of copyright infringement whatsoever." user))
        (w/obs-activate-toggle 'activate-nixos)))
    (list
      "super idol" 500
      (lambda (_ _)
        (let ((cur (float-time)))
          (when (> (- cur w/twitch-redeem-sound-last) 2)
            (w/write-chat-event "SuperIdoldexiaorongdoumeinidetianbayuezhengwudeyangguangdoumeiniyaoyanreai105Cdenididiqingchundezhen")
            (soundboard//play-clip "superidololdshortstyle.ogg" 0.5)
            (setq w/twitch-redeem-sound-last cur)))))
    (list
      "enable ad block" 500
      (lambda (user _)
        (w/write-chat-event (format "%s turned on UltraBlock HYPER: Community Edition" user)) 
        (w/banner-ad-block)))
    (list
      "hex" 500
      (lambda (user inp)
        (let* ((sp (s-split " " inp))
                (spell (car sp))
                (target (cadr sp)))
          (if (and spell target (stringp spell) (stringp target))
            (progn
              (w/write-chat-event (s-concat user " hexed " target " with: " spell))
              (when-let ((type (alist-get spell w/hex-types nil nil #'s-equals?)))
                (w/hex target user type)))
            (w/write-chat-event (s-concat user "'s hex fizzled out with a puff of smoke!"))))))
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
      "spatiotemporal clarity" 2000
      (lambda (user _)
        (w/stop-all-audio)
        (run-with-timer
          1 nil
          (lambda () (soundboard//play-clip "chinesebiblemozartfade.ogg")))
        (w/write-chat-event (s-concat user " established spatiotemporal clarity"))
        (w/obs-activate-toggle 'spatiotemporal-clarity)))
    (list
      "total clarity" 5000
      (lambda (user _)
        (w/stop-all-audio)
        (run-with-timer
          1 nil
          (lambda () (soundboard//play-clip "gong.ogg")))
        (w/write-chat-event (s-concat user " established total clarity"))
        (w/obs-activate-toggle 'total-clarity)))
    (list
      "canonize me" 20000
      (lambda (user _)
        (w/write-chat-event (s-concat user " was canonized!"))
        (w/bible-canonize user)))
    (list
      "cloning facility" 50000
      (lambda (user _)
        (w/write-chat-event (format "%s entered the cloning facility" user))))
    ))

(provide 'wasp-twitch-redeems)
;;; wasp-twitch-redeems.el ends here
