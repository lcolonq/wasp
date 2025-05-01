;;; wasp-friend-reading --- "friend" can read pages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'wasp-friend)
(require 'wasp-wikipedia)

(defun w/friend-react-wikipedia (user page)
  "Call when USER asks \"friend\" to react to PAGE on Wikipedia."
  (w/fetch-wikipedia
   page
   (lambda (sum)
     (w/friend-respond (format "%s asks you to react to the Wikipedia page for %s. The page summary is: %s" user page sum)))))

(provide 'wasp-friend-reading)
;;; wasp-friend-reading.el ends here
