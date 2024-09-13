;;; wasp-bus --- Pub/sub bus client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)

(defgroup wasp nil
  "Pub/sub bus client."
  :group 'applications)

(defcustom w/bus-process "wasp-bus"
  "Name of process connected to network bus."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-buffer " *wasp-bus*"
  "Name of buffer used to store intermediate network data."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-host "shiro"
  "Hostname of the bus server."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-port 32050
  "Port of the bus server."
  :type '(integer)
  :group 'wasp)

(defvar w/bus-event-handlers nil
  "List of pairs of events and handler functions.")

(defun w/bus-clean-string (s)
  "Remove special characters from S."
  (replace-regexp-in-string "[^[:print:]]" "" s))

(defun w/handle-message (msg)
  "Handle the message MSG."
  (let* ((ev (car msg))
         (body (cdr msg))
         (handler (alist-get ev w/bus-event-handlers nil nil #'equal)))
    (if handler
        (funcall handler body)
      (message (format "Unknown incoming event: %S" ev)))))

(defun w/get-complete-line ()
  "Kill a line followed by a newline if it exists, and nil otherwise."
  (let ((l (thing-at-point 'line t)))
    (if (and l (s-contains? "\n" l))
        (progn
          (delete-region (line-beginning-position) (line-beginning-position 2))
          l)
      nil)))
(defun w/handle-lines ()
  "Call `w/handle-message' on every complete line of the current buffer."
  (let ((l (w/get-complete-line)))
    (when (and l (not (s-blank? l)))
      (w/handle-message (read (w/bus-clean-string l)))
      (w/handle-lines))))
(defun w/process-filter (proc data)
  "Process filter for pub/sub bus connection on PROC and DATA."
  (with-current-buffer (get-buffer-create w/bus-buffer)
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (w/handle-lines)))

(defun w/sub (ev)
  "Subscribe to the event EV."
  (process-send-string
   w/bus-process
   (s-concat
    (format "%S" `(sub ,ev))
    "\n")))

(defun w/pub (ev &optional d)
  "Publish the data D to the event EV."
  (process-send-string
   w/bus-process
   (s-concat
    (format "%S" `(pub ,ev ,@d))
    "\n")))

(defun w/sub-all ()
  "Subscribe to all events in `w/bus-event-handlers'."
  (--each w/bus-event-handlers
    (message (format "Subscribing to: %S" (car it)))
    (w/sub (car it))))

(defun w/disconnect ()
  "Disconnect from the pub/sub bus."
  (interactive)
  (when (process-live-p (get-process w/bus-process))
    (delete-process w/bus-process)))

(defun w/connect ()
  "Connect to the pub/sub bus."
  (interactive)
  (w/disconnect)
  (make-network-process
   :name w/bus-process
   :buffer nil
   :host w/bus-host
   :service w/bus-port
   :filter #'w/process-filter)
  (w/sub-all))

(provide 'wasp-bus)
;;; wasp-bus.el ends here
