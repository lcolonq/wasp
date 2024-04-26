;;; wasp --- We Are So (back), 'Puter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(add-to-list 'load-path (f-canonical "./secret/"))
(add-to-list 'load-path (f-canonical "./src/"))
(add-to-list 'load-path (f-canonical "./src/gizmo/"))
(add-to-list 'load-path (f-canonical "./src/contrib/"))

;; do not open this on stream
(require 'wasp-sensitive)

;; core
(require 'wasp-utils)
(require 'wasp-hooks)
(require 'wasp-bus)
(require 'wasp-db)
(require 'wasp-user)
(require 'wasp-ai)
(require 'wasp-audio)
(require 'wasp-model)
(require 'wasp-obs)
(require 'wasp-chat)
(require 'wasp-twitch)

;; gizmos
(require 'wasp-pronunciation)
(require 'wasp-biblicality)
(require 'wasp-newspaper)
(require 'wasp-friend)
(require 'wasp-fakechat)
(require 'wasp-dna)
(require 'wasp-heartrate)
(require 'wasp-8ball)

;; configuration
(require 'wasp-user-whitelist)
(require 'wasp-fake-chatters)
(require 'wasp-twitch-chat-commands)
(require 'wasp-twitch-redeems)
(require 'wasp-voice-commands)
(require 'wasp-event-handlers)

;; user contrib
(require 'bezelea-muzak)

;; initialization
(w/connect)
(w/db-connect)
(w/create-chat-overlay-frame)
(w/show-chat-overlay-frame nil)
(w/twitch-7tv-update-emotes)
(w/twitch-update-title)

(w/twitch-run-shoutout-timer)
(w/twitch-run-emote-frame-timer)
(w/run-model-timer)
(w/run-obs-timer)
(w/run-stream-transcribe-timer)

(w/populate-bible-table)

;; (defun w/fix-user-database-ok (user)
;;   "Fix USER's database entry."
;;   (w/user-set user (fig//db2-serialize-old-entry (fig//load-db-old user))))

(provide 'wasp)
;;; wasp.el ends here
