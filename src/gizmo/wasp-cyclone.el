;;; wasp-cyclone --- Gizmocycling -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-friend)
(require 'wasp-heartrate)
(require 'wasp-chatsummary)
(require 'wasp-alert-message)
(require 'wasp-fakechat)
(require 'wasp-bannerads)

(defconst w/gizmo-buffer-names
  (list
    w/friend-buffer
    w/heartrate-buffer
    w/chatsummary-buffer
    w/alert-message-buffer
    w/fake-chatter-enclosure-buffer
    w/banner-ad-buffer
    w/chat-event-buffer
    ))

(defvar w/gizmo-windows (list))

(defun w/gizmo-tag-window ()
  "Tag the current window as containing a gizmo."
  (interactive)
  (when-let* ((w (selected-window)))
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

(defun w/gizmo-ensure-shown (buf)
  "Ensure that BUF is shown in one of the windows."
  (unless (--any (s-equals? (buffer-name (window-buffer it)) (buffer-name (get-buffer buf))) w/gizmo-windows)
    (set-window-buffer (car w/gizmo-windows) buf)))

(defvar w/gizmo-cycle-timer nil)
(defun w/run-gizmo-cycle-timer ()
  "Run the gizmo cycle timer."
  (when w/gizmo-cycle-timer
    (cancel-timer w/gizmo-cycle-timer))
  (w/gizmo-cycle)
  (setq
    w/gizmo-cycle-timer
    (run-with-timer 300 nil #'w/run-gizmo-cycle-timer)))
(w/run-gizmo-cycle-timer)

(require 'htmlize)
(defvar w/gizmo-html-cache (ht-create))
(defun w/gizmo-render-html (buf)
  "Render BUF to HTML with embedded images."
  (let* ( (htmlize-output-type 'inline-css)
          (htmlize-force-inline-images t)
          (buf (htmlize-buffer buf))
          (html (with-current-buffer buf (buffer-string))))
    (kill-buffer buf)
    html))
(defun w/gizmo-upload (buf)
  "Upload the HTML contents of BUF to the database."
  (let* ( (b (get-buffer buf))
          (nm (buffer-name b))
          (render (w/gizmo-render-html b))
          (cached (ht-get w/gizmo-html-cache nm)))
    (unless (and cached (s-equals? cached render))
      (ht-set w/gizmo-html-cache nm render)
      (w/db-hset-then "gizmos" nm
        render
        (lambda (_)
          (w/pub '(gizmo buffer update) (list nm)))))))

(provide 'wasp-cyclone)
;;; wasp-cyclone.el ends here
