;;; wasp-user-stats --- User data: statistics -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'wasp-user)

(defvar w/user-faction-exemptions
  (list
   "LCOLONQ"
   "MODCLONK"
   "fn_lumi"))

(defun w/user-initial-faction (user)
  "Determine the initial faction for USER."
  (unless (-contains? w/user-faction-exemptions user)
    (let* ((factions '(nate lever tony)))
      (nth (random (length factions)) factions))))
(defun w/user-ensure-faction ()
  "Ensure that the current user has a faction assigned."
  (let ((cur (alist-get :faction w/user-current)))
    (unless cur
      (setf (alist-get :faction w/user-current)
        (w/user-initial-faction w/user-current-name)))))

(defconst w/user-elements
  '(("fire" "🔥" "red")
    ("water" "🌊" "blue")
    ("wind" "🍃️" "green")
    ("earth" "🪨" "brown")
    ("lightning" "⚡" "yellow")
    ("heart" "🩷" "pink")))
(defun w/user-initial-element ()
  "Determine the initial faction for USER."
  (let* ((factions w/user-elements))
    (car (nth (random (length factions)) factions))))
(defun w/user-ensure-element ()
  "Ensure that the current user has a faction assigned."
  (let ((cur (alist-get :element w/user-current)))
    (unless cur
      (setf (alist-get :element w/user-current)
        (w/user-initial-element)))))

(defun w/user-faction-total (faction)
  "Compute the boost totals for FACTION."
  (-sum
    (-non-nil
      (--map
        (alist-get :boost it)
        (--filter
          (and (listp it) (eq faction (alist-get :faction it)))
          (ht-values w/user-cache))))))
(defun w/user-faction-totals ()
  "Compute the boost totals for each FACTION."
  (list
    (w/user-faction-total 'nate)
    (w/user-faction-total 'tony)
    (w/user-faction-total 'lever)))

(defun w/user-ensure-name ()
  "Ensure that the current user has a name assigned."
  (let ((cur (alist-get :name w/user-current)))
    (unless cur
      (setf (alist-get :name w/user-current) w/user-current-name))))

(defun w/user-stats-update ()
  "Ensure that the current user has all stats."
  (w/user-ensure-name)
  (w/user-ensure-faction)
  (w/user-ensure-element))

(provide 'wasp-user-stats)
;;; wasp-user-stats.el ends here
