;;; wasp-8ball --- 8 Ball Predictions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-ai)

(defconst w/8ball-answers
  '((1 "It is certain" "It is decidedly so" "Without a doubt" "Yes definitely" "You may rely on it")
    (2 "As I see it, yes" "Most likely" "Outlook good" "Yes" "Signs point to yes")
    (3 "Reply hazy, try again" "Ask again later" "Better not tell you now" "Cannot predict now" "Concentrate and ask again")
    (4 "Don't count on it" "My reply is no" "My sources say no" "Outlook not so good" "Very doubtful")
    (5 "Joel" "You are a silly goose (barnacle)" "Bet" "It's so over" "We're so back" "Commit spontaneous generation")))
(defun w/8ball (query k)
  "Consult the magic conch for QUERY. Pass the resulting answer K."
  (w/ai
   query
   (lambda (res)
     (let* ((num (string-to-number (s-trim res)))
            (answers (alist-get (if (or (< num 1) (> num 5)) (random 6) num) w/8ball-answers nil nil #'=))
            )
       (funcall
        k
        (nth (random (length answers)) answers)
        )))
   "Given the user's query, respond with a single number between 0 and 4 based on how likely the query is to be true. 1 is more likely, 5 is least likely."
   (list "Is the sky blue?" "Is today Friday?" "How to shot web?")
   (list "1" "3" "5")))

(provide 'wasp-8ball)
;;; wasp-8ball.el ends here
