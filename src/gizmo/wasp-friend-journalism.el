;;; wasp-friend-journalism --- "friend" writes the newspaper -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-friend)
(require 'wasp-newspaper)

(defun w/friend-journalism-input ()
  "Collect an input for \"friend\"'s journalism based on recent activities."
  (s-join
   "\n"
   (cons
    (format "LCOLONQ: %s" (s-trim w/last-stream-transcription))
    (--map
     (format "%s: %s" (car it) (cdr it))
     (reverse (-take 20 w/twitch-chat-history))))))

(defun w/friend-journalism (author headline)
  "Retrieve \"friend\"'s opinion on current events related to HEADLINE.
AUTHOR was a contributing author btw."
  (w/friend-personality
   (s-concat
    "Headline: " headline "\n\n"
    (w/friend-journalism-input))
   (lambda (resp)
     (when resp
       (w/chat-write-event (format "\"friend\" finished writing about: %s" headline))
       (funcall
        (if (= (random 5) 0) #'w/newspaper-screenshot (lambda (k) (funcall k nil)))
        (lambda (img)
          (when img
            (w/chat-write-event "...and the article included some photojournalism"))
          (push
           (w/make-newspaper-article
            :headline headline
            :author (format "\"friend\" and %s" author)
            :content (s-trim resp)
            :image img)
           w/newspaper-todays-articles)))))
    "Given a headline of a newspaper article and a summary of recent user activity, please do your best journalist impression and produce a one paragraph article about the situation that fits the headline."))

(provide 'wasp-friend-journalism)
;;; wasp-friend-journalism.el ends here
