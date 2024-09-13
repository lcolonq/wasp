;;; wasp-shindaggers --- Shindaggers interface -*- lexical-binding: t; -*-
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

(defcustom w/shindaggers-server "https://shindaggers.io"
  "Server URL for Shindaggers."
  :type '(string)
  :group 'wasp)

(defvar w/shindaggers-last-response nil)

(defun w/shindaggers-get (loc k)
  "Get LOC from Shindaggers, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat w/shindaggers-server loc)
    :type "GET"
    :headers
    `(("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/shindaggers-last-response data)
       (funcall k data))))
  t)

(defun w/shindaggers-get-user-id (user k)
  "Retrieve the Shindaggers user number for USER and pass it to K."
  (let ((duser (s-downcase user)))
    (w/shindaggers-get
     (s-concat "/api/users?search=" duser)
     (lambda (p)
       (let ((res
              (car
               (--filter
                (s-equals? (s-downcase (ht-get it "name")) duser)
                (seq-into (ht-get p "Users") 'list)))))
         (funcall k (and res (string-to-number (ht-get res "id")))))))))

(defun w/shindaggers-get-collection (userid k)
  "Retrieve the Shindaggers collection for USERID and pass it to K."
  (w/shindaggers-get
   (format "/api/user/%s/collection" userid)
   (lambda (p)
     (funcall
      k
      (--map (ht-get it "name") (ht-get p "Collectables"))))))

(defvar w/shindaggers-user-cache nil)
(defun w/shindaggers-update-user (user)
  "Update USER data from Shindaggers."
  (unless (-contains? w/shindaggers-user-cache user)
    (add-to-list 'w/shindaggers-user-cache user)
    (w/shindaggers-get-user-id
     user
     (lambda (userid)
       (w/shindaggers-get-collection
        userid
        (lambda (knives)
          (w/user-bind
           user
           (lambda ()
             (setf (alist-get :shindaggers-knives w/user-current) knives)))))))))

(provide 'wasp-shindaggers)
;;; wasp-shindaggers.el ends here
