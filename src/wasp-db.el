;;; wasp-db --- Redis protocol -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'queue)
(require 'dash)
(require 'wasp-utils)

(defcustom w/db-process "wasp-db"
  "Name of process connected to Redis."
  :type '(string)
  :group 'wasp)

(defcustom w/db-buffer " *wasp-db*"
  "Name of buffer used to store intermediate Redis connection data."
  :type '(string)
  :group 'wasp)

(defcustom w/db-host "shiro"
  "Hostname of the Redis server."
  :type '(string)
  :group 'wasp)

(defcustom w/db-port 6379
  "Port of the Redis server."
  :type '(integer)
  :group 'wasp)

(defvar w/db-callback-queue (make-queue)
  "Queue of callbacks to handle incoming responses.")

(defun w/db-parse-rest ()
  "Parse everything before the \\r\\n terminator (and consume the terminator)."
  (let ((res (w/eat (lambda (c) (/= c ?\r)))))
    (w/munch ?\r)
    (w/munch ?\n)
    res))

(defun w/db-parse-value ()
  "Parse a single RESP value from the current buffer."
  ;; (w/write-log (format "parsing: %S" (buffer-string)))
  (when-let* ((c (char-after)))
    (delete-char 1)
    (cl-case c
      (?+ (w/db-parse-rest))
      (?: (string-to-number (w/db-parse-rest)))
      (?$
       (let ((len (string-to-number (w/db-parse-rest))))
         (if (= len -1)
             ""
           (let ((ret (w/devour (point) (+ (point) len))))
             (w/munch ?\r)
             (w/munch ?\n)
             ret))))
      (?*
       (let ((len (string-to-number (w/db-parse-rest))))
         (--map (w/db-parse-value) (-iota len))))
      (otherwise (error (format "Unknown Redis sigil: %s (contents: %S)" c (buffer-string)))))))

(defun w/db-parse-response ()
  "Try to parse a single RESP value from the current process buffer.
If successful, pass the value to the queued callback and return non-nil.
If not, return nil."
  (when-let* ((v (w/db-parse-value)))
    (when-let* ((cb (queue-dequeue w/db-callback-queue)))
      (funcall cb v))
    t))

(defun w/db-process-filter (proc data)
  "Process filter for Redis connection on PROC and DATA."
  (with-current-buffer (get-buffer-create w/db-buffer)
    (when (not (marker-position (process-mark proc)))
      (set-marker (process-mark proc) (point-max)))
    (goto-char (process-mark proc))
    (insert data)
    (set-marker (process-mark proc) (point))
    (goto-char (point-min))
    (when (s-suffix? "\r\n" (buffer-string))
      (condition-case err
          (while (w/db-parse-response))
        (error
         (w/write-chat-event (format "Database crashed, error: %s" err))
         (w/db-disconnect))))))

(defun w/db-encode (x)
  "Encode X for Redis."
  (cond
   ((listp x) ;; encode lists as arrays
    (format "*%d\r\n%s\r\n" (length x) (apply #'s-concat (-map #'w/db-encode x))))
   ((stringp x) ;; encode strings as bulk strings
    (format "$%d\r\n%s\r\n" (string-bytes x) x))))

(defun w/db-send-raw (msg)
  "Send MSG to Redis."
  ;; (w/write-log (format "sending to redis: %s" msg))
  (process-send-string w/db-process msg))

(defun w/db-cmd (cmd k)
  "Run CMD in Redis and pass the result to K."
  (let ((enc (w/db-encode cmd)))
    (queue-enqueue w/db-callback-queue k)
    (w/db-send-raw enc)))

(defun w/db-disconnect ()
  "Disconnect from Redis."
  (when (process-live-p (get-process w/db-process))
    (delete-process w/db-process)))

(defun w/db-connect ()
  "Connect to Redis."
  (w/db-disconnect)
  (queue-clear w/db-callback-queue)
  (with-current-buffer (get-buffer-create w/db-buffer)
    (set-buffer-multibyte nil)
    (erase-buffer))
  (make-network-process
   :coding 'no-conversion
   :name w/db-process
   :buffer nil
   :host w/db-host
   :service w/db-port
   :filter #'w/db-process-filter))

(defun w/db-keys (pat k)
  "Retrieve the list of keys matching PAT and pass them to K."
  (if (stringp pat)
      (w/db-cmd `("KEYS" ,pat) k)
    (error "Redis pattern must be string")))

(defun w/db-set (key val)
  "Set KEY to VAL in Redis."
  (if (and (stringp key) (stringp val))
      (w/db-cmd `("SET" ,key ,val) (lambda (_) nil))
    (error "Redis key and value must be strings")))

(defun w/db-get (key k)
  "Get KEY from Redis and pass the corresponding value to K."
  (if (stringp key)
      (w/db-cmd `("GET" ,key) k)
    (error "Redis key must be string")))

(defun w/db-mget (keys k)
  "Get KEYS from Redis and pass the corresponding values to K."
  (w/db-cmd `("MGET" ,@keys) k))

(defun w/db-hset-then (key hkey val k &rest vals)
  "Set HKEY in hash KEY to VAL in Redis.
Afterward call K."
  (w/db-cmd `("HSET" ,key ,hkey ,val ,@vals) k))

(defun w/db-hset (key hkey val &rest vals)
  "Set HKEY in hash KEY to VAL in Redis."
  (w/db-cmd `("HSET" ,key ,hkey ,val ,@vals) (lambda (_) nil)))
  ;; (w/db-hset-then key hkey val (lambda (_) nil) vals))

(defun w/db-hget (key hkey k)
  "Get HKEY in hash KEY from Redis and pass the corresponding value to K."
  (w/db-cmd `("HGET" ,key ,hkey) k))

(provide 'wasp-db)
;;; wasp-db.el ends here
