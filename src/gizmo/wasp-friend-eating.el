;;; wasp-friend-eating --- "friend" can eat -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'wasp-friend)

(defvar w/friend-tastes " You love eating cranberries and lemons.")

(defun w/friend-feed (user food)
  "Call when USER fed FOOD to \"friend\"."
  (w/friend-personality
    (format "%s fed you %s" user food)
    (lambda (msg)
      (w/friend-say msg)
      (w/friend-set-state 'eating 6))
    w/friend-tastes))

(provide 'wasp-friend-eating)
;;; wasp-friend-eating.el ends here
