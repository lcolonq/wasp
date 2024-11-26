;;; wasp-auth --- SSH-based authentication -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(defcustom w/auth-sign-process "wasp-auth-sign"
  "Name of process for signing messages."
  :type '(string)
  :group 'wasp)

(defcustom w/auth-error-buffer " *wasp-auth-error*"
  "Name of buffer used to store authentication errors."
  :type '(string)
  :group 'wasp)

(defun w/auth-sign (msg k)
  "Sign MSG using ~/.ssh/id_ed25519 and pass the signature to K."
  (let* ((buf (generate-new-buffer " *wasp-auth-sign*"))
         (proc
          (make-process
           :name w/auth-sign-process
           :buffer buf
           :stderr (get-buffer-create w/auth-error-buffer)
           :command
           `("ssh-keygen" "-Y" "sign" "-f" ,(f-canonical "~/.ssh/id_ed25519") "-n" "file" "-")
           :sentinel
           (lambda (_ _)
             (let ((sig (with-current-buffer buf (buffer-string))))
               (kill-buffer buf)
               (funcall k sig))))))
    (process-send-string proc msg)
    (process-send-string proc "\n")
    (process-send-eof proc)))

(provide 'wasp-auth)
;;; wasp-auth.el ends here
