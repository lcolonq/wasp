;;; wasp-utils --- Miscellaneous utilities -*- lexical-binding: t; byte-compile-warnings: (not suspicious); -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'f)
(require 'rx)
(require 'cl-lib)
(require 'eieio)
(require 'request)
(require 'ef-themes)

(defcustom w/log-buffer "*wasp-log*"
  "Name of buffer used to store the log."
  :type '(string)
  :group 'wasp)

(defun w/read-sexp (s)
  "Read string S into a Lisp form.
Return nil on error."
  (condition-case nil (read s) (error nil)))

(defun w/write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun w/write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (w/write (concat line "\n") face))

(defun w/clean-string (s)
  "Remove special characters from S."
  (replace-regexp-in-string "[^[:print:]]" "" s))

(defun w/write-log (line &optional face)
  "Write LINE to the log buffer and apply FACE."
  (with-current-buffer (get-buffer-create w/log-buffer)
    (goto-char (point-max))
    (w/write-line (format "%s" line) face)
    (goto-char (point-max))))

(defun w/append-file (s path)
  "Append S to the file at PATH."
  (f--write-bytes (encode-coding-string s 'utf-8) path t))

(defmacro w/defstruct (name &rest body)
  "Define a structure with NAME (with the constructor under the w/ namespace).
BODY is passed directly to `cl-defstruct'."
  `(cl-defstruct
     (,name (:constructor ,(intern (s-concat "w/make-" (s-chop-prefix "w/" (symbol-name name)))))
       (:copier nil))
     ,@body))

(defmacro w/. (slot s)
  "Lookup SLOT in the struct S."
  `(eieio-oref ,s (quote ,slot)))

(defun w/pick-random (xs)
  "Pick a random element of XS."
  (and xs (nth (random (length xs)) xs)))

(defun w/shuffle (s)
  "Shuffle S."
  (if (seq-empty-p s)
    nil
    (let ((elt (seq-elt s (random (seq-length s)))))
      (cons elt (w/shuffle (remove elt s))))))

(defun w/list-to-pair (xs)
  "Turn the first two elements of XS into a pair."
  (cons (car xs) (cadr xs)))

(defun w/tempfile (prefix str &optional ext)
  "Write STR to a temporary file with PREFIX and return the path.
Optionally append EXT to the path."
  (let ((path (s-concat (make-temp-file prefix) (or ext ""))))
    (with-temp-file path (insert str))
    path))

(defun w/message-ping (msg)
  "Given MSG, extract a user pinged."
  (cadr (s-match (rx "@" (group (one-or-more (any alnum "_")))) msg)))

(defun w/utf8 (s)
  "Decode the unibyte string S as UTF-8."
  (decode-coding-string s 'utf-8))

(defun w/decode-string (s)
  "Decode the base64 UTF-8 string S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun w/encode-string (s)
  "Encode the base64 UTF-8 string S."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun w/slurp (path)
  "Read PATH and return a string."
  (f-read-text path 'utf-8))

(defun w/spit (path data)
  "Write DATA to PATH."
  (f-write-text data 'utf-8 path))

(defun w/unix-time ()
  "Return the current Unix timestamp."
  (float-time (current-time)))

(defun w/daily-log-path ()
  "Return the path to today's daily log file."
  (format-time-string "~/logs/log-%Y-%m-%d.txt" (current-time)))

(defun w/daily-log (msg)
  "Write MSG to today's daily log file."
  (write-region
    (s-concat (format-time-string "[%H:%M:%S]" (current-time)) "\t" msg "\n")
    nil (w/daily-log-path) t 'donotprintmessagety))

(defvar w/fetch-last-response nil)
(defun w/fetch (url &optional k)
  "Get URL, passing the returned data to K."
  (request
    url
    :type "GET"
    :success
    (cl-function
      (lambda (&key data &allow-other-keys)
        (setq w/fetch-last-response data)
        (when k
          (funcall k data)))))
  t)
(defun w/fetch-html (url &optional k)
  "Get URL, passing the returned HTML to K."
  (w/fetch
    url
    (lambda (data)
      (funcall
        k
        (with-temp-buffer
          (insert data)
          (libxml-parse-html-region (point-min) (point-max)))))))
(defun w/fetch-json (url &optional k)
  "Get URL, passing the returned JSON to K."
  (w/fetch
    url
    (lambda (data)
      (funcall k (json-parse-string data)))))

(defun w/devour (start end)
  "Delete and return the region from START to END."
  ;; (w/write-log (format "devouring: %s %s %s" start end (buffer-string)))
  (let ((ret (decode-coding-string (buffer-substring start end) 'utf-8)))
    (delete-region start end)
    ret))

(defun w/eat (p)
  "Consume characters from the current buffer while P yields true.
Return the consumed string."
  (let ((start (point)))
    (while-let ((char (char-after))
                 (cont (funcall p char)))
      (forward-char 1))
    (w/devour start (point))))

(defun w/peek (c)
  "Look at the character at point in the current buffer.
If it is C, consume it and return non-nil."
  (when-let* ( (char (char-after))
               (cont (= char c)))
    (delete-char 1)
    t))

(defun w/munch (c)
  "Look at the character at point in the current buffer.
If it is C, consume it.
Otherwise, throw an error."
  (if-let* ( (char (char-after))
             (cont (= char c)))
    (progn
      (delete-char 1)
      t)
    (error (format "While parsing, expected %c but found %c" c char))))

(defun w/get-stream-primary-window ()
  "Get the marked primary stream window."
  (window-at-x-y 1 1))

(defun w/open-link ()
  "Open URL in the primary stream window."
  (interactive)
  (when-let* ((url (thing-at-point 'url t)))
    (select-window (w/get-stream-primary-window))
    (browse-url url)))

(defun w/prevent-focus-frame (e)
  "Prevent focus from reaching popup frame E."
  (not (frame-parameter (cadr e) 'wasp-prevent-focus)))

(defconst w/asset-base-path (f-canonical "/home/llll/src/wasp/assets/"))
(defun w/asset (path)
  "Return the absolute path given an asset path PATH."
  (f-join w/asset-base-path path))

(defun w/image-text (path &optional text &rest props)
  "Return TEXT propertized with the image at PATH.
If TEXT is nil, use the empty string instead."
  (propertize
    (or text "i")
    'display
    (apply #'create-image path nil nil props)
    'rear-nonsticky t))

(defsubst w/saget (k a)
  "Retrieve the value for string key K in alist A."
  (alist-get k a nil nil #'s-equals?))

(defun w/change-theme (theme)
  "Change the current theme to THEME."
  (ef-themes-select theme)
  (ef-themes-with-colors
    (setenv "COLONQ_BGCOLOR" bg-main)
    (set-face-attribute
      'vertical-border nil
      :foreground bg-alt
      :background bg-alt)
    (set-face-attribute
      'fringe nil
      :foreground bg-alt
      :background bg-alt))
  (ef-themes-with-colors
    (set-face-attribute
      'eshell-prompt nil
      :foreground fg-main
      :background bg-alt
      :weight 'bold
      :extend t)))

(defun w/random-color ()
  "Return a random color string."
  (let ( (r (random 256))
         (g (random 256))
         (b (random 256)))
    (format "#%02x%02x%02x" r g b)))

(defun w/aref-u32-be (a idx)
  "Read a big-endian 32-bit integer starting at IDX from A."
  (logior
   (lsh (aref a idx) 24)
   (lsh (aref a (+ idx 1)) 16)
   (lsh (aref a (+ idx 2)) 8)
   (aref a (+ idx 3))))

(defun w/aref-u16-be (a idx)
  "Read a big-endian 16-bit integer starting at IDX from A."
  (logior
   (lsh (aref a idx) 8)
   (aref a (+ idx 1))))

(defun w/load-image-ff (path)
  "Load the Farbfeld image at PATH.
Return a list of the width, height, and pixels of the image."
  (when-let*
      ((data (f-read-bytes path))
       ((s-prefix? "farbfeld" data))
       (width (w/aref-u32-be data 8))
       (height (w/aref-u32-be data 12))
       (pixels
        (--map
         (let ((a (+ 16 (* it 8))))
           (list
            (lsh (w/aref-u16-be data a) -8)
            (lsh (w/aref-u16-be data (+ a 2)) -8)
            (lsh (w/aref-u16-be data (+ a 4)) -8)))
         (-iota (* width height)))))
    (list width height (seq-into pixels 'vector))))

(defun w/load-image-png (path)
  "Load the PNG image at PATH (by converting to Farbfeld first)."
  (let ((tmp "/tmp/udcff.ff"))
    (when (= 0 (call-process-shell-command (format "png2ff <'%s' >'%s'" path tmp) nil "*udc-png-error*"))
      (w/load-image-ff tmp))))

(provide 'wasp-utils)
;;; wasp-utils.el ends here
