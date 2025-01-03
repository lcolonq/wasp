;;; wasp-cyclone --- Gizmocycling -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-friend)
(require 'wasp-heartrate)
(require 'wasp-chatsummary)
(require 'wasp-alert-message)

(defconst w/gizmo-buffer-names
  (list
   w/friend-buffer
   w/heartrate-buffer
   w/chatsummary-buffer
   w/alert-message-buffer))

(defvar w/gizmo-windows (list))

(defun w/gizmo-tag-window ()
  "Tag the current window as containing a gizmo."
  (interactive)
  (when-let ((w (selected-window)))
    (add-to-list 'w/gizmo-windows w)))

(defun w/gizmo-cycle-window (w)
  "Cycle the gizmo in W."
  (when-let*
      ((cur (buffer-name (window-buffer w)))
       (idx (--find-index (equal it cur) w/gizmo-buffer-names))
       (bufs (-non-nil (-map #'get-buffer w/gizmo-buffer-names)))
       (nidx (mod (+ idx 1) (length bufs)))
       (buf (nth nidx bufs)))
    (set-window-buffer w buf)))

(defun w/gizmo-cycle ()
  "Cycle all gizmo-bearing windows."
  (interactive)
  (--each w/gizmo-windows
    (w/gizmo-cycle-window it)))

(provide 'wasp-cyclone)
;;; wasp-cyclone.el ends here
