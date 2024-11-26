;;; wasp-telemetry --- Telemetry and Use Tracking -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'ht)
(require 'wasp-utils)

(defvar w/telemetry-stats (ht-create))
(defvar w/telemetry-current nil)
(defvar w/telemetry-current-duration 0)
(defvar w/telemetry-work-cooldown 0)

(defun w/telemetry-change (new)
  "Update the current telemetry state to NEW."
  (message "New status: %s" new)
  (when w/telemetry-current
    (ht-set!
     w/telemetry-stats w/telemetry-current
     (+ (ht-get w/telemetry-stats w/telemetry-current 0)
        w/telemetry-current-duration)))
  (setf w/telemetry-current new)
  (setf w/telemetry-current-duration 0))

(defun w/telemetry-update ()
  "Check and possibly update KPIs."
  (cl-incf w/telemetry-current-duration)
  (when-let*
      ((win (w/get-stream-primary-window))
       (b (window-buffer win)))
    (cond
     ((> w/telemetry-work-cooldown 0)
      (cl-decf w/telemetry-work-cooldown))
     ((s-contains? "Wikipedia" (buffer-name b))
      (w/telemetry-change 'wikipedia))
     ((not (eq w/telemetry-current 'yap))
      (w/telemetry-change 'yap))
     )))

(defun w/telemetry-change-handler (_ _ _)
  "Function for `after-change-functions' to track status."
  (setf w/telemetry-work-cooldown 12)
  (unless (eq w/telemetry-current 'lockedin)
    (w/telemetry-change 'lockedin)))
(add-to-list 'after-change-functions #'w/telemetry-change-handler)

(defvar w/telemetry-timer nil)
(defun w/run-telemetry-timer ()
  "Run the telemetry timer."
  (when w/telemetry-timer
    (cancel-timer w/telemetry-timer))
  (w/telemetry-update)
  (setq
   w/telemetry-timer
   (run-with-timer 10 nil #'w/run-telemetry-timer)))
(w/run-telemetry-timer)

(provide 'wasp-telemetry)
;;; wasp-telemetry.el ends here
