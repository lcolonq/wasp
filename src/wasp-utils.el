;;; wasp-utils --- Miscellaneous utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'cl-lib)
(require 'eieio)
(require 'request)

(defcustom w/log-buffer "*wasp-log*"
  "Name of buffer used to store the log."
  :type '(string)
  :group 'wasp)

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
    (w/write-line (w/clean-string (format "%s" line)) face)
    (goto-char (point-max))))

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
  (nth (random (length xs)) xs))

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

(defun w/decode-string (s)
  "Decode the base64 UTF-8 string S."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun w/encode-string (s)
  "Decode the base64 UTF-8 string S."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun w/slurp (path)
  "Read PATH and return a string."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun w/spit (path data)
  "Write DATA to PATH."
  (write-region data nil path))

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
  (let ((ret (buffer-substring start end)))
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
  (when-let ((char (char-after))
             (cont (= char c)))
    (delete-char 1)
    t))

(defun w/munch (c)
  "Look at the character at point in the current buffer.
If it is C, consume it.
Otherwise, throw an error."
  (if-let ((char (char-after))
           (cont (= char c)))
      (progn
        (delete-char 1)
        t)
    (error (format "While parsing, expected %c but found %c" c char))))
    

(provide 'wasp-utils)
;;; wasp-utils.el ends here
