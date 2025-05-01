;;; wasp-friend-callout --- "friend" talks about things -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-friend)
(require 'wasp-twitch)
(require 'wasp-gcp)
(require 'wasp-aoc)
(require 'wasp-uwoomfie)

(defun w/friend-callout-holiday ()
  "Call to respond to the current holiday."
  (w/friend-respond "We're restoring a crumbling mansion to it's former glory. Say something about that please!"))

(defun w/friend-callout-hexamedia ()
  "Call to respond to a random recent chatter's Hexamedia card collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :hexamedia-cards (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (cards (cdr user))
         (coll (and cards (nth (random (length cards)) cards))))
    (when coll
      (w/friend-respond
       (format
        "%s has collected %s out of 20 cards in the %s collection. Please mention the collection name and the person collecting."
        (car user)
        (cdr coll)
        (car coll))))))

(defun w/friend-callout-copfish ()
  "Call to respond to a random recent chatter's Copfish ratio."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :copfish-ratio (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (when user
      (w/friend-respond
       (format
        "%s has collected %s out of %s fish in the Copfish fish catching collection. Please mention the collection name and the person collecting."
        (car user)
        (cadr user)
        (cddr user))))))

(defun w/friend-callout-uwoomfie ()
  "Call to respond to a random recent chatter's Uwoomfie status."
  (let* ((users
          (-filter
           #'cdr
           (--map
            (cons (car it) (w/uwoomfie-get-status (car it)))
            (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (cl-case (cdr user)
      (cool (w/friend-respond (format "According to UWOSLAB, %s is a very cool person. Make sure to mention their username." (car user))))
      (honored (w/friend-respond (format "According to UWOSLAB, %s is an honorary viewer. Make sure to mention their username." (car user))))
      (t nil))))

(defun w/friend-callout-shindaggers ()
  "Call to respond to a random recent chatter's Shindaggers knife collection."
  (let* ((users (-filter #'cdr (--map (cons (car it) (alist-get :shindaggers-knives (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users)))
         (knives (cdr user))
         (knife (and knives (nth (random (length knives)) knives))))
    (when knife
      (w/friend-respond
       (format
        "%s has collected the %s from shindig's Shindaggers knife collection. Please mention the collection name and the person collecting and the knife."
        (car user)
        knife)))))

(defun w/friend-callout-aoc ()
  "Call to respond to a random recent chatter's Advent of Code completion."
  (let* ((users (-filter #'cdr (--map (cons (car it) (w/aoc-lookup-stars (car it))) (-take 10 w/twitch-chat-history))))
         (user (and users (nth (random (length users)) users))))
    (w/friend-respond
     (format
      "%s has been doing Advent of Code this year, and they've completed %d out of %d problems so far."
      (car user)
      (cdr user)
      (w/aoc-max-stars)))))

(defun w/friend-callout-gcp ()
  "Call to respond to the current GCP dot."
  (w/gcp-dot
   (lambda (d)
     (w/friend-respond
      (format
       "The Global Consciousness Project indicator is currently as follows: %s"
       (w/gcp-describe d))))))

(defun w/friend-callout-resolution ()
  "Call to respond to a random recent chatter's resolve."
  (when-let*
      ((users (-filter #'cdr (--map (cons (car it) (alist-get :resolution2025 (w/user-cache-get (car it)))) (-take 10 w/twitch-chat-history))))
       (user (and users (nth (random (length users)) users))))
    (if (s-match (rx (one-or-more digit) (zero-or-more space) "x" (zero-or-more space) (one-or-more digit)) (cdr user))
        (w/friend-respond
         (format
          "%s snarkily said that their New Year's resolution was a screen resolution. What do you think about this?" (car user)))
      (w/friend-respond
       (format
        "%s made a New Year's resolution to %s. Ask them how it's going!"
        (car user)
        (cdr user))))))

(provide 'wasp-friend-callout)
;;; wasp-friend-callout.el ends here
