;;; wasp --- We Are So (back), 'Puter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(add-to-list 'load-path (f-canonical "./secret/"))
(add-to-list 'load-path (f-canonical "./src/"))
(add-to-list 'load-path (f-canonical "./src/gizmo/"))
(add-to-list 'load-path (f-canonical "./src/contrib/"))

(add-to-list 'load-path (f-canonical "~/src/soundboard/"))
(add-to-list 'load-path (f-canonical "~/src/emacs-htmlize/"))
(add-to-list 'load-path (f-canonical "~/src/ttf.el/"))
(setq elisp-flymake-byte-compile-load-path load-path)

;; do not open this on stream
(require 'wasp-sensitive)

;; core
(require 'wasp-utils)
(require 'wasp-hooks)
(require 'wasp-bus)
(require 'wasp-bus-binary)
(require 'wasp-db)
(require 'wasp-user)
(require 'wasp-user-stats)
(require 'wasp-ai)
(require 'wasp-audio)
(require 'wasp-soundboard)
(require 'wasp-model)
(require 'wasp-obs)
(require 'wasp-chat)
(require 'wasp-twitch)
(require 'wasp-overlay)
(require 'wasp-auth)
(require 'wasp-setup)

;; gizmos
(require 'wasp-pronunciation)
(require 'wasp-biblicality)
(require 'wasp-glossary)
(require 'wasp-newspaper)
(require 'wasp-friend)
(require 'wasp-friend-journalism)
(require 'wasp-friend-voice)
(require 'wasp-friend-callout)
(require 'wasp-friend-reading)
(require 'wasp-friend-eating)
(require 'wasp-friend-music)
(require 'wasp-fakechat)
(require 'wasp-dna)
(require 'wasp-heartrate)
(require 'wasp-chatsummary)
(require 'wasp-8ball)
(require 'wasp-gcp)
(require 'wasp-aoc)
(require 'wasp-resolve)
(require 'wasp-hexamedia)
(require 'wasp-copfish)
(require 'wasp-shindaggers)
(require 'wasp-uwoomfie)
(require 'wasp-density)
(require 'wasp-wikipedia)
(require 'wasp-prod)
(require 'wasp-youtube)
(require 'wasp-hex)
(require 'wasp-bannerads)
(require 'wasp-aoc)
(require 'wasp-cyclone)
(require 'wasp-flymake)
(require 'wasp-genealogy)
(require 'wasp-irish)

;; configuration
(require 'wasp-user-whitelist)
(require 'wasp-fake-chatters)
(require 'wasp-twitch-chat-commands)
(require 'wasp-twitch-redeems)
(require 'wasp-voice-commands)
(require 'wasp-event-handlers)
(require 'wasp-event-handlers-binary)

;; user contrib
;; (require 'muzak)

;; (defun w/fix-user-database-ok (user)
;;   "Fix USER's database entry."
;;   (w/user-set user (fig//db2-serialize-old-entry (fig//load-db-old user))))

(setq elisp-flymake-byte-compile-load-path load-path)

(provide 'wasp)
;;; wasp.el ends here
