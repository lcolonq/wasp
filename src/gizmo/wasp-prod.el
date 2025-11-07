;;; wasp-prod --- prodzpod interface -*- lexical-binding: t; -*-
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
(require 'wasp-chat)
(require 'wasp-twitch)
(require 'wasp-biblicality)
(require 'soundboard)

(defcustom w/prod-server "https://prod.kr"
  "Server URL for prodzpod."
  :type '(string)
  :group 'wasp)

(defvar w/prod-last-response nil)

(defun w/prod-get-raw (loc k)
  "Get LOC from prodzpod, passing the returned string to K."
  (setf request-message-level -1)
  (request
    (s-concat w/prod-server loc)
    :type "GET"
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/prod-last-response data)
       (funcall k data))))
  t)

(defun w/prod-get (loc k)
  "Get LOC from prodzpod, passing the returned JSON to K."
  (request
    (s-concat w/prod-server loc)
    :type "GET"
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/prod-last-response data)
       (funcall k data))))
  t)

(defun w/prod-clone-respond (prompt)
  "Pass PROMPT to the prodzpod clone and put the resulting response in chat room."
  (w/prod-get
   (s-concat "/api/clone?prompt=" (url-encode-url prompt))
   (lambda (resp)
     (when-let (((and resp (hash-table-p resp)))
                (name (ht-get resp "name"))
                (color (ht-get resp "color"))
                (sigil (ht-get resp "sigil"))
                (res (ht-get resp "res"))
                ((and (stringp name) (stringp color) (stringp sigil) (stringp res))))
       (let* ((trimmed (s-replace-regexp "^.+: " "" (s-replace "\n" " " (s-trim res))))
              (text-colored-bible-res (w/bible-colorize-sentence trimmed))
              (text-colored-bible (car text-colored-bible-res))
              (bible-score (cdr text-colored-bible-res)))
         (push (cons name trimmed) w/twitch-chat-history)
         (unless (string-empty-p text-colored-bible)
           (when (s-contains? "hexadiCoding" trimmed)
             (soundboard//play-clip "developers.ogg"))
             (w/chat-write-message
              (w/make-chat-message
               :user name
               :text (w/twitch-add-7tv-emotes text-colored-bible)
               :user-color color
               :sigil sigil
               :biblicality bible-score))))))))

(provide 'wasp-prod)
;;; wasp-prod.el ends here
