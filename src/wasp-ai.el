;;; wasp-ai --- AI interaction -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'wasp-utils)

(defcustom w/ai-process "wasp-ai"
  "Name of process connected to ChatGPT."
  :type '(string)
  :group 'wasp)

(defcustom w/ai-buffer " *wasp-ai*"
  "Name of buffer used to store ChatGPT output."
  :type '(string)
  :group 'wasp)

(defcustom w/ai-error-buffer " *wasp-ai-error*"
  "Name of buffer used to store ChatGPT errors."
  :type '(string)
  :group 'wasp)

(defvar-local w/ai-callback nil)
(defun w/ai (question k &optional systemprompt user assistant)
  "Ai QUESTION to ChatGPT and pass the answer to K.
Optionally use SYSTEMPROMPT and the USER and ASSISTANT prompts."
  (let ((tmpfile (make-temp-file "wasp-ai"))
        (tmpfilesystem (make-temp-file "wasp-ai-system"))
        (tmpfileuser (make-temp-file "wasp-ai-user"))
        (tmpfileassistant (make-temp-file "wasp-ai-assistant"))
        (buf (generate-new-buffer w/ai-buffer)))
    (with-temp-file tmpfile (insert question))
    (when systemprompt
      (with-temp-file tmpfilesystem (insert systemprompt)))
    (when user
      (with-temp-file tmpfileuser
        (if (stringp user)
            (insert (s-concat user "\n"))
          (--each user
            (insert (s-concat it "\n"))))))
    (when assistant
      (with-temp-file tmpfileassistant
        (if (stringp assistant)
            (insert (s-concat assistant "\n"))
          (--each assistant
            (insert (s-concat it "\n"))))))
    (with-current-buffer buf
      (setq-local w/ai-callback k)
      (erase-buffer))
    (make-process
     :name w/ai-process
     :buffer buf
     :command
     (list
      "chatgpt"
      tmpfile
      (if systemprompt tmpfilesystem "systemprompt.txt")
      (if user tmpfileuser "userprompt.txt")
      (if assistant tmpfileassistant "assistantprompt.txt"))
     :stderr (get-buffer-create w/ai-error-buffer)
     :sentinel
     (lambda (_ _)
       (with-current-buffer buf
         (funcall w/ai-callback (s-trim (buffer-string))))))))

(provide 'wasp-ai)
;;; wasp-ai.el ends here
