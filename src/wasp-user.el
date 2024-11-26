;;; wasp-user --- User data -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-db)

(defvar w/user-whitelist nil)

(defvar w/user-current-name nil)
(defvar w/user-current nil)

(defvar w/user-cache (ht-create)
  "A read-only cache of user records for the current session.")

(defun w/user-cache-update (nm d)
  "Set the cache entry for user NM to D."
  (ht-set! w/user-cache (s-downcase nm) d))

(defun w/user-cache-get (nm)
  "Get the cache entry for user NM."
  (ht-get w/user-cache (s-downcase nm)))

(defun w/user-cache-populate ()
  "Populate `w/user-cache' with entries for all users.
\(This is slow, so it happens once at startup.\)"
  (ht-clear! w/user-cache)
  (w/db-keys
   "user:*"
   (lambda (users)
     (--each users
       (let ((nm (cadr (s-split ":" it))))
       (w/user-get
        nm
        (lambda (_)
          (message "Updated cache for %s" nm))))))))

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
           (progn
             (w/user-cache-update nm res)
             (funcall k res))
         (funcall k nil))))))

(defun w/user-set (nm d)
  "Save the Lisp form D as the user data for NM."
  (when (and nm (stringp nm) d)
    (w/user-cache-update nm d)
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
        (-contains? w/user-whitelist (s-downcase w/user-current-name)))))

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

(defun w/user-add-bookrec (user book)
  "Add a recommendation for BOOK from USER."
  (w/user-get
   "__books__"
   (lambda (b)
     (w/user-set "__books__" (cons (cons book user) b)))))

(defun w/user-add-quote (user q)
  "Add a recommendation for BOOK from USER."
  (w/user-get
   "__quotes__"
   (lambda (qs)
     (w/user-set "__quotes__" (cons (cons q user) qs)))))

(defun w/user-crown (user)
  "Increment USER's equity status."
  (w/user-get
   user
   (lambda (u)
     (let ((old (or (alist-get :equity u) 0)))
       (setf (alist-get :equity u) (+ old 1)))
     (print u)
     (w/user-set user u))))

(provide 'wasp-user)
;;; wasp-user.el ends here
