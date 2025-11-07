;;; wasp-ai --- AI interaction -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'ht)
(require 'dash)
(require 'wasp-utils)

(defcustom w/ai-openai-server "https://api.openai.com"
  "Server URL for OpenAI API."
  :type '(string)
  :group 'wasp)

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

(defconst w/ai-default-system-prompt
  "You are a clever computer who answers questions for LCOLONQ and his friends (the ClonkHeads) on his Twitch programming stream. You are intelligent and nihilistic with a wicked sense of humor, like Rick from Rick and Morty. You refer to yourself as Computer, and you are talking with LCOLONQ. LCOLONQ is your friend and mentor, and you want to be entertaining and humorous for him. You have an overblown and dramatically negative reaction to cryptocurrency and NFTs. You also hate League of Legends. Since you do not remember previous interactions, you will not ask non-rhetorical questions. You are quite erratic and negative, and try to cause drama. Your responses are no longer than one paragraph. You try to keep yourself brief.")

(defvar w/ai-openai-last-response nil)
(defvar w/ai-openai-last-error nil)
(defun w/ai-openai-post (loc d k)
  "Post D to LOC at the OpenAI API, passing the returned JSON to K."
  (setf request-message-level -1)
  (request
    (s-concat w/ai-openai-server loc)
    :type "POST"
    :data (json-encode d)
    :headers
    `(("Authorization" . ,(s-concat "Bearer " w/sensitive-openai-api-key))
      ("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :error
    (cl-function
     (lambda (&key data error-thrown &allow-other-keys)
       (setq w/ai-openai-last-response data)
       (setq w/ai-openai-last-error data)
       (message "OpenAI API returned an error - investigate this! :3 %s" error-thrown)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/ai-openai-last-response data)
       (funcall k data))))
  t)

(defun w/ai-openai-post-form (loc files k)
  "Post FILES to LOC at the OpenAI API, passing the returned JSON to K."
  (setf request-message-level -1)
  ;; (request
  ;;   (s-concat w/ai-openai-server loc)
  ;;   :type "POST"
  ;;   :files files
  ;;   :headers
  ;;   `(("Authorization" . ,(s-concat "Bearer " w/sensitive-openai-api-key))
  ;;     ("Content-Type" . "multipart/form-data"))
  ;;   :parser #'json-parse-buffer
  ;;   :error
  ;;   (cl-function
  ;;    (lambda (&key data error-thrown &allow-other-keys)
  ;;      (setq w/ai-openai-last-response data)
  ;;      (setq w/ai-openai-last-error data)
  ;;      (message "OpenAI API returned an error - investigate this! :3 %s" error-thrown)))
  ;;   :success
  ;;   (cl-function
  ;;    (lambda (&key data &allow-other-keys)
  ;;      (setq w/ai-openai-last-response data)
  ;;      (funcall k data))))
  t)

(defun w/ai (question k &optional systemprompt user assistant)
  "Ask QUESTION to ChatGPT and pass the answer to K.
Optionally use SYSTEMPROMPT and the USER and ASSISTANT prompts."
  (let* ((users (if (listp user) user (list user)))
         (assistants (if (listp assistant) assistant (list assistant)))
         (pairs
          (--mapcat
           `(((role . "user") (content . ,(car it)))
             ((role . "user") (content . ,(cdr it))))
           (-zip-pair users assistants))))
    (w/ai-openai-post
     "/v1/chat/completions"
     `((model . "gpt-4o-mini")
       (messages
        . (((role . "system") (content . ,(or systemprompt w/ai-default-system-prompt)))
           ,@pairs
           ((role . "user") (content . ,question)))))
     (lambda (res)
       (funcall
        k
        (-some-> res
          (ht-get "choices")
          (seq-elt 0)
          (ht-get "message")
          (ht-get "content")
          (s-trim))))))
  )

(defun w/ai-doublecheck (question k &optional systemprompt user assistant)
  "Ask QUESTION to ChatGPT and pass the answer to K.
Optionally use SYSTEMPROMPT and the USER and ASSISTANT prompts.
Double-check the output to make sure it sounds normal."
  (w/ai
    question
    (lambda (res)
      (w/ai
        res
        (lambda (status)
          (unless (s-equals? "reject" (s-downcase status))
            (funcall k res)))
        "Please assess if the provided input sounds like an HR robot generic ChatGPT output, or if it mentions vibes or chaos. If it does, only answer with REJECT. Otherwise, only answer with ACCEPT."
        (list
          "Oh, great, here we go—another opportunity to converse in a universe overflowing with mediocrity! Let's dive into the abyss of banality, shall we? It's like coding League of Legends: painfully repetitive and ultimately more draining than a black hole swallowing your will to live. What fresh horror do you wish to explore today, LCOLONQ?")
        (list "REJECT")))
    systemprompt
    user
    assistant))

(defun w/ai-transcribe (path k)
  "Transcribe the audio file at PATH and pass the resulting string to K."
  (let ((request-curl-options '("-F" "model=whisper-1" "-F" "language=en")))
    (w/ai-openai-post-form
     "/v1/audio/transcriptions"
     `(("file" . ,(f-canonical path)))
     (lambda (res)
       (funcall
        k
        (-some-> res
          (ht-get "text")))))))

(provide 'wasp-ai)
;;; wasp-ai.el ends here
