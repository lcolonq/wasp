;;; wasp-copfish --- Copfish interface -*- lexical-binding: t; -*-
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

(defcustom w/copfish-server "https://cop.fish/api/"
  "Server URL for Copfish."
  :type '(string)
  :group 'wasp)

(defvar w/copfish-last-response nil)

(defun w/copfish-get (loc k)
  "Get LOC from Copfish, passing the returned HTML to K."
  (setf request-message-level -1)
  (request
    (s-concat w/copfish-server loc)
    :type "GET"
    :parser #'buffer-string
    :success
    (cl-function
      (lambda (&key data &allow-other-keys)
        (setq w/copfish-last-response data)
        (funcall k data))))
  t)

(defun w/copfish-get-fish (user k)
  "Retrieve USER's fish ratio from copfish API.
Pass the resulting fraction to K."
  (w/copfish-get
    (s-concat "fishdex/" user)
    (lambda (s)
      (let ((sp (s-split " " s)))
        (when (= (length sp) 2)
          (funcall k (cons (string-to-number (car sp)) (string-to-number (cadr sp)))))))))

(defvar w/copfish-user-cache nil)
(defun w/copfish-update-user (user)
  "Update USER data from Copfish."
  (unless (-contains? w/copfish-user-cache user)
    (add-to-list 'w/copfish-user-cache user)
    (w/copfish-get-fish
      user
      (lambda (ct)
        (w/user-bind
          user
          (lambda ()
            (setf (alist-get :copfish-ratio w/user-current) ct)))))))

(provide 'wasp-copfish)
;;; wasp-copfish.el ends here
