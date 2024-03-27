;;; wasp-user --- User data -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'wasp-utils)
(require 'wasp-db)

(defvar w/user-whitelist nil)

(defvar w/user-current-name nil)
(defvar w/user-current nil)

(defun w/user-db-key (nm)
  "Return the database key for user NM."
  (s-concat "user:" (s-downcase nm)))

(defun w/user-get (nm k)
  "Fetch user data for user NM.
Pass the resulting Lisp form to K."
  (when (and nm (stringp nm) (functionp k))
    (w/db-get
     (w/user-db-key nm)
     (lambda (d)
       (if-let*
           ((d)
            (stringp d)
            (res (w/read-sexp d)))
           (funcall k res)
         (funcall k nil))))))

(defun w/user-set (nm d)
  "Save the Lisp form D as the user data for NM."
  (when (and nm (stringp nm) d)
    (w/db-set
     (w/user-db-key nm)
     (format "%S" d))))

(defun w/user-bind (nm k)
  "Bind the data for user NM to `w/user-current' during K.
Save it back to the database after K returns."
  (w/user-get
   nm
   (lambda (d)
     (let ((w/user-current d)
           (w/user-current-name nm))
       (funcall k)
       (w/user-set nm w/user-current)))))

(defun w/user-authorized ()
  "Return non-nil if the current user is authorized to use advanced techniques."
  (let ((boost (alist-get :boost w/user-current)))
    (or (and boost (> boost 2))
        (and boost (< boost -2))
        (-contains? w/user-whitelist w/user-current-name))))

(defun w/user-boost (user)
  "Increase USER's boost power by 1."
  (w/user-get
   user
   (lambda (d)
     (cl-incf (alist-get :boost d 0))
     (w/user-set user d))))

(defun w/user-tsoob (user)
  "Decrement USER's boost power by 1."
  (w/user-get
   user
   (lambda (d)
     (cl-decf (alist-get :boost d 0))
     (w/user-set user d))))

(provide 'wasp-user)
;;; wasp-user.el ends here
