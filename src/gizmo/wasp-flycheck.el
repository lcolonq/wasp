;;; wasp-flycheck --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'flycheck)

(flycheck-define-generic-checker 'wasp-twitch
  "Checker to display errors from Twitch redeems."
  :start
  (lambda (c x)
    (print c)
    (print x))
  :modes '(fundamental-mode)
  )

(provide 'wasp-flycheck)
;;; wasp-flycheck.el ends here
