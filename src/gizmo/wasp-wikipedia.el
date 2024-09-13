;;; wasp-wikipedia --- Wikipedia integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)
(require 'dom)
(require 'wasp-utils)
(require 'wasp-chat)

(defcustom w/wikipedia-query-base "https://en.wikipedia.org/w/api.php?action=query&prop=extracts&format=json&exintro=1&titles="
  "Base URL for Wikipedia query."
  :type '(string)
  :group 'wasp)

(defvar w/wikipedia-last-response nil)

(defun w/fetch-wikipedia (page k)
  "Retrieve PAGE from Wikipedia.
Pass the resulting article summary to K."
  (let ((pagename (if (s-contains? "://" page) (url-file-nondirectory page) page)))
    (request
      (s-concat w/wikipedia-query-base (url-encode-url pagename))
      :type "GET"
      :parser #'json-parse-buffer
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)

         (setq w/wikipedia-last-response data)
         (if-let ((pages (car (ht-values (ht-get (ht-get w/wikipedia-last-response "query") "pages"))))
                  (ext (ht-get pages "extract"))
                  (dom (with-temp-buffer (insert ext) (libxml-parse-html-region (point-min) (point-max))))
                  )
             (funcall k (s-trim (dom-texts dom)))
           (w/write-chat-event (format "Could not find Wikipedia page: %s" pagename))))))
    nil))

(defcustom w/wiki-buffer "*wasp-wiki*"
  "Name of buffer used to display wiki."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/wiki-mode special-mode "Wikipedia"
  "Major mode for displaying wiki."
  :group 'wasp)

(defun w/get-wiki-buffer ()
  "Return the wiki buffer."
  (unless (get-buffer w/wiki-buffer)
    (with-current-buffer (get-buffer-create w/wiki-buffer)
      (w/wiki-mode)))
  (get-buffer w/wiki-buffer))

(defun w/wikipedia-summary (page)
  "Display a summary of PAGE from Wikipedia."
  (w/fetch-wikipedia
   page
   (lambda (sum)
     (with-current-buffer (w/get-wiki-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (w/write-line sum)))))
  )

(provide 'wasp-wikipedia)
;;; wasp-wikipedia.el ends here
