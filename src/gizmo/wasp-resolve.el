;;; wasp-resolve --- The Strength Of Our Resolve -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'wasp-user)

(defcustom w/resolve-buffer "*wasp-resolve*"
  "Name of buffer used to display resolve."
  :type '(string)
  :group 'wasp)

(defvar w/resolve-recorded nil)

(defun w/resolve-record-user (user)
  "Record resolve for USER if not already recorded."
  (unless (-contains? w/resolve-recorded user)
    (w/user-get
     user
     (lambda (data)
       (when-let ((resolution (alist-get :resolution data)))
         (add-to-list 'w/resolve-recorded user)
         (with-current-buffer (get-buffer-create w/resolve-buffer)
           (goto-char (point-max))
           (insert (format "%s: %s\n" user resolution))))))))

(provide 'wasp-resolve)
;;; wasp-resolve.el ends here
