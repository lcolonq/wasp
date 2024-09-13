;;; wasp-hexamedia --- Hexamedia interface -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'rx)
(require 'cl-lib)
(require 'request)
(require 'dom)
(require 'wasp-user)

(defcustom w/hexamedia-server "https://hexa.media"
  "Server URL for Hexamedia."
  :type '(string)
  :group 'wasp)

(defvar w/hexamedia-last-response nil)

(defun w/hexamedia-get (loc k)
  "Get LOC from Hexamedia, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat w/hexamedia-server loc)
    :type "GET"
    :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/hexamedia-last-response data)
       (funcall k data))))
  t)

(defun w/hexamedia-parse-binder-string (bs)
  "Given BS, return the number of obtained cards."
  (let ((match
         (s-match-strings-all
          (rx
           string-start
           (group (zero-or-more (or alnum space)))
           " Set ("
           (group (one-or-more digit))
           )
          bs)))
    (cons (cadar match) (string-to-number (caddar match)))))

(defun w/hexamedia-get-card-totals (user k)
  "Retrieve the Hexamedia binder for USER.
Pass the resulting card totals to K."
  (w/hexamedia-get
   (s-concat "/binder/" (s-downcase user))
   (lambda (p)
     (funcall
      k
      (-map
       #'w/hexamedia-parse-binder-string
       (-filter
        #'stringp
        (--map
         (caddr it)
         (dom-by-tag p 'center))))))))

(defvar w/hexamedia-user-cache nil)
(defun w/hexamedia-update-user (user)
  "Update USER data from Hexamedia."
  (unless (-contains? w/hexamedia-user-cache user)
    (add-to-list 'w/hexamedia-user-cache user)
    (w/hexamedia-get-card-totals
     user
     (lambda (ct)
       (w/user-bind
        user
        (lambda ()
          (setf (alist-get :hexamedia-cards w/user-current) ct)))))))

(provide 'wasp-hexamedia)
;;; wasp-hexamedia.el ends here
