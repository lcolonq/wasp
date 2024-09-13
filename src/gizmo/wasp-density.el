;;; wasp-density --- The Density Initiative -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom w/density-buffer "*wasp-density*"
  "Name of buffer used to display The Density Initiative."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/density-mode special-mode "The Density Initiative"
  "Major mode for displaying The Density Initiative."
  :group 'wasp)

(defun w/density-get-buffer ()
  "Return the \"friend\" buffer."
  (unless (get-buffer w/density-buffer)
    (with-current-buffer (get-buffer-create w/density-buffer)
      (w/density-mode)))
  (get-buffer w/density-buffer))

(provide 'wasp-density)
;;; wasp-density.el ends here
