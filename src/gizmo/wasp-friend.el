;;; wasp-friend --- "friend" -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'wasp-utils)
(require 'wasp-chat)
(require 'wasp-ai)
(require 'wasp-twitch)

;;;; Buffer and mode
(defcustom w/friend-buffer "*wasp-friend*"
  "Name of buffer used to display \"friend\"."
  :type '(string)
  :group 'wasp)

(define-derived-mode w/friend-mode special-mode "\"friend\"'s lair"
  "Major mode for displaying \"friend\"'s lair."
  :group 'wasp
  (message "hi i'm \"friend\"")
  (setq-local cursor-type nil))

(defun w/friend-get-buffer ()
  "Return the \"friend\" buffer."
  (unless (get-buffer w/friend-buffer)
    (with-current-buffer (get-buffer-create w/friend-buffer)
      (w/friend-mode)))
  (get-buffer w/friend-buffer))

;;;; State
(defvar w/friend-state 'default)
(defvar w/friend-emotion "neutral")
(defvar w/friend-message-cache nil)
(defvar w/friend-state-timer 0)
(defvar w/friend-animation 1)
(defvar w/friend-speech "")
(defvar w/friend-speech-timer 0)

(defun w/friend-set-state (st &optional time)
  "Set \"friend\"'s state to ST for TIME seconds."
  (setf w/friend-state st)
  (setf w/friend-state-timer (or time 5)))

(defun w/friend-set-speech (msg &optional time)
  "Have \"friend\" say MSG for TIME."
  (w/chat-write-event (s-concat "Friend says: " msg))
  (setf w/friend-speech msg)
  (setf w/friend-speech-timer (or time 5)))

(defun w/friend-say (msg)
  "Have \"friend\" say MSG."
  (w/daily-log (format "[FRIEND]: %s" msg))
  (w/gizmo-ensure-shown (w/friend-get-buffer))
  (w/friend-pronounce-phonemes (w/friend-replace-graphemes msg))
  (w/friend-set-speech msg 30)
  (w/friend-set-state 'chatting 30))

;;;; Core
(defun w/friend-personality (msg k &optional extra)
  "Given MSG, pass a string with more personality to K.
Append EXTRA to the personality."
  (let ((call (s-concat w/friend-emotion " | " msg)))
    (w/ai
      call
      (lambda (new)
        (let ((sp (s-split "|" (s-trim new))))
          (if (= 2 (length sp))
            (progn
              (when (stringp (car sp))
                (setf w/friend-emotion (s-trim (car sp))))
              (when (stringp (cadr sp))
                (let ((resp (s-trim (cadr sp))))
                  (push (cons call (s-trim new)) w/friend-message-cache)
                  (funcall k resp))))
            (let ((resp (s-trim new)))
              (push (cons call (s-trim new)) w/friend-message-cache)
              (funcall k resp)))))
      (s-concat
        "You are the personality of a desktop buddy named \"friend\". \"friend\" is irreverant but kind, and only speaks in lowercase. You are kind of dumb in a cute way and silly like a virtual pet. You live in the corner of LCOLONQ's stream and provide commentary on events. You never use punctuation. You are foolish. You never use metaphors or similes or idioms. You never describe something by comparing it to another thing. You never use the words \"like\" or \"as\". Given an emotional state and a description of an event that happened to you, please respond with a new emotional state and a short message in response considering your emotional state. The message should only be one clause, but you can include an occasional emoji if it is cute only. You like people, video games, emojis, learning, and food. "
        "The theme of LCOLONQ's stream today is " (s-trim (w/slurp "~/today.txt")) " "
        "The title of LCOLONQ's stream today is " w/twitch-current-stream-title " "
        (or extra ""))
      (cons "neutral | Mimeyu fed you an apple." (reverse (-take 5 (-map #'car w/friend-message-cache))))
      (cons "happy | yum apple so good" (reverse (-take 5 (-map #'cdr w/friend-message-cache)))))))

;;;; Interface
(defun w/friend-respond (ev &optional k)
  "Call when an event EV happens to \"friend\".
If K is specified, call it after the response."
  (w/friend-personality
   ev
   (lambda (msg)
     (w/friend-say msg)
     (when k
       (funcall k)))))

;;;; Updating
(defun w/friend-random-event ()
  "Activate a random \"friend\" event."
  (cl-case (random 10)
    (9 (w/friend-set-state 'jumping))))

(defun w/friend-update ()
  "Update \"friend\"'s state per tick."
  (setf w/friend-animation (% (+ w/friend-animation 1) 2))
  (if (> w/friend-state-timer 0)
    (cl-decf w/friend-state-timer)
    (setf w/friend-state 'default))
  (if (> w/friend-speech-timer 0)
    (cl-decf w/friend-speech-timer))
  (when (= (random 120) 0)
    (w/friend-random-event))
  (cl-case w/friend-state
    (eating (setf w/friend-state 'eating0))
    (eating0 (setf w/friend-state 'eating1))
    (eating1 (setf w/friend-state 'eating2))
    (eating2 (setf w/friend-state 'eating1))
    (chatting (setf w/friend-state 'chatting0))
    (chatting0 (setf w/friend-state 'chatting))))

;;;; Rendering
(defun w/friend-get-offset ()
  "Return the number of newlines to print before \"friend\"."
  (if (-contains? '(jumping) w/friend-state)
    w/friend-animation
    1))

(defun w/friend-get-face ()
  "Return the eyes and mouth for \"friend\" as a list of strings."
  (cl-case w/friend-state
    (jumping (list "^" "^" "ww"))
    (eating (list "v" "v" "<>"))
    (eating0 (list "v" "v" "<>"))
    (eating1 (list "-" "-" "mw"))
    (eating2 (list "-" "-" "wm"))
    (chatting (list ">" ">" "oo"))
    (chatting0 (list ">" ">" "~~"))
    (t (list "-" "-" "ww"))))

(defun w/friend-get-bubble ()
  "Return the text bubble for \"friend\"."
  (if (> w/friend-speech-timer 0)
    w/friend-speech
    nil))

(defun w/friend-render ()
  "Render the \"friend\" buffer."
  (save-excursion
    (with-current-buffer (w/friend-get-buffer)
      (setq-local cursor-type nil)
      (let*
        ((inhibit-read-only t)
          (face (w/friend-get-face))
          (bubble (w/friend-get-bubble)))
        (erase-buffer)
        (w/write
          (format-spec
            "%a\
  /\\  /\\   
  \\----/ 
 / %l  %r \\ 
 \\  %m  /
  +----+\
"
;;             "%a\
;;   /----\\  
;;  / %l  %r \\ 
;;  \\  %m  /
;;   +----+\
;; "
            `((?a . ,(s-repeat (w/friend-get-offset) "          \n"))
               (?l . ,(car face))
               (?r . ,(cadr face))
               (?m . ,(caddr face)))))
        (goto-char (point-min))
        (end-of-line)
        (w/write (or bubble ""))
        (forward-line)
        (end-of-line)
        (w/write (if bubble "/" ""))
        )))
  (w/gizmo-upload (w/friend-get-buffer)))

(defvar w/friend-timer nil)
(defun w/friend-run-timer ()
  "Run the \"friend\" timer."
  (when w/friend-timer
    (cancel-timer w/friend-timer))
  (condition-case e
    (progn
      (w/friend-update)
      (w/friend-render))
    ((debug error)
      (message "friend error: %s" e)
      (cancel-timer w/friend-timer)
      (setq w/friend-timer nil)))
  (setq
    w/friend-timer
    (run-with-timer 1 nil #'w/friend-run-timer)))

(defun w/friend-start ()
  "Launch \"friend\"."
  (interactive)
  (w/friend-run-timer))

(defun w/friend-stop ()
  "Stop \"friend\"."
  (interactive)
  (cancel-timer w/friend-timer)
  (message "\"friend\" is going to sleep!"))

(provide 'wasp-friend)
;;; wasp-friend.el ends here
