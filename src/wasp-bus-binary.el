;;; wasp-bus-binary --- Pub/sub bus client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-utils)

(defcustom w/bus-binary-process "wasp-bus-binary"
  "Name of process connected to binary message bus."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-binary-buffer " *wasp-bus-binary*"
  "Name of buffer used to store intermediate binary message bus data."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-binary-host "shiro"
  "Hostname of the binary message bus."
  :type '(string)
  :group 'wasp)

(defcustom w/bus-binary-port 32051
  "Port of the binary message bus."
  :type '(integer)
  :group 'wasp)

(defvar w/bus-binary-event-handlers nil
  "List of pairs of events and handler functions.")

(defun w/bus-binary-read-bytes (len)
  "Read LEN bytes from the current buffer.
Advances point by LEN also."
  (let ((end (+ (point) len)))
    (when (<= end (point-max))
      (let ((istr (buffer-substring (point) end)))
        (forward-char len)
        istr))))

(defun w/bus-binary-read-int32le ()
  "Read a 32-bit little endian integer from the current buffer."
  (when-let* ((istr (w/bus-binary-read-bytes 4)))
    (-let [(x0 x1 x2 x3) (seq-into istr 'list)]
      (logior x0 (ash x1 8) (ash x2 16) (ash x3 24)))))

(defun w/bus-binary-read-length-prefixed ()
  "Read a length-prefixed string from the current buffer.
Return nil if unable."
  (let ((start (point)))
    (if-let* ((len (w/bus-binary-read-int32le)))
      (progn
        (w/bus-binary-read-bytes len))
      (goto-char start)
      nil)))

(defun w/bus-binary-read-message ()
  "Parse a message from the current buffer.
Return non-nil if a message was successfully parsed."
  (if-let* ( (start (point))
             (ev (w/bus-binary-read-length-prefixed))
             (d (w/bus-binary-read-length-prefixed)))
    (progn
      (delete-region start (point))
      (cons ev d))
    (goto-char start)
    nil))

(defun w/bus-binary-handle-message ()
  "Parse and handle a message from the current buffer.
Return non-nil if a message was successfully parsed."
  (when-let* ((msg (w/bus-binary-read-message)))
    (-let [(ev . d) msg]
      (when-let* ((handler (w/saget ev w/bus-binary-event-handlers)))
        (funcall handler d))
      t)))

(defun w/bus-binary-process-filter (proc data)
  "Process filter for binary message bus connection on PROC and DATA."
  (with-current-buffer (get-buffer-create w/bus-binary-buffer)
    (set-buffer-multibyte nil)
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (while (w/bus-binary-handle-message))))

(defun w/bus-binary-build-int32le (x)
  "Convert X into the bytes for a little endian 32-bit integer."
  (unibyte-string
    (logand x #xff)
    (logand (lsh x -8) #xff)
    (logand (lsh x -16) #xff)
    (logand (lsh x -24) #xff)))

(defun w/bus-binary-build-length-prefixed (s)
  "Turn S into a length-prefixed unibyte string."
  (s-concat
    (w/bus-binary-build-int32le (string-bytes s))
    s))

(defun w/bus-binary-convert-event (ev)
  "Convert the s-expression EV to a string event."
  (s-chop-suffix ")" (s-chop-prefix "(" (format "%s" ev))))

(defun w/binary-sub (ev)
  "Subscribe to the event EV."
  (process-send-string
    w/bus-binary-process
    (s-concat "s"
      (w/bus-binary-build-length-prefixed (w/bus-binary-convert-event ev)))))

(defun w/binary-sub-all ()
  "Subscribe to all events in `w/bus-binary-event-handlers'."
  (--each w/bus-binary-event-handlers
    (message (format "Subscribing to: %S" (car it)))
    (w/binary-sub (car it))))

(defun w/binary-pub (ev &optional d)
  "Publish the data D to the event EV."
  (process-send-string
    w/bus-binary-process
    (s-concat "p"
      (w/bus-binary-build-length-prefixed (w/bus-binary-convert-event ev))
      (w/bus-binary-build-length-prefixed (or d "")))))

(defun w/bus-binary-disconnect ()
  "Disconnect from Redis."
  (when (process-live-p (get-process w/bus-binary-process))
    (delete-process w/bus-binary-process)))

(defun w/bus-binary-connect ()
  "Connect to Redis."
  (w/bus-binary-disconnect)
  (with-current-buffer (get-buffer-create w/bus-binary-buffer)
    (set-buffer-multibyte nil)
    (erase-buffer))
  (make-network-process
   :coding 'no-conversion
   :name w/bus-binary-process
   :buffer nil
   :host w/bus-binary-host
   :service w/bus-binary-port
    :filter #'w/bus-binary-process-filter)
  (w/binary-sub-all))

(provide 'wasp-bus-binary)
;;; wasp-bus-binary.el ends here
