;;; wasp-uwoomfie --- Uwoomfie interface -*- lexical-binding: t; -*-
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

(defcustom w/uwoomfie-server "https://uwoomfie.com"
  "Server URL for Uwoomfie."
  :type '(string)
  :group 'wasp)

(defvar w/uwoomfie-last-response nil)
(defvar w/uwoomfie-honorary-viewers nil)
(defvar w/uwoomfie-cool-people nil)

(defun w/uwoomfie-get (loc k)
  "Get LOC from Uwoomfie, passing the returned HTML to K."
  (request
    (s-concat w/uwoomfie-server loc)
    :type "GET"
    :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/uwoomfie-last-response data)
       (funcall k data))))
  t)

(defun w/uwoomfie-fetch-honorary-viewers (k)
  "Fetch the list of Uwoomfie honorary viewers and pass them to K."
  (w/uwoomfie-get
   "/honoraryviewers.html"
   (lambda (data)
     (funcall
      k
      (--map (car (s-split " " (caddr it))) (dom-by-tag data 'h1))))))

(defun w/uwoomfie-fetch-cool-people (k)
  "Fetch the list of Uwoomfie cool people and pass them to K."
  (w/uwoomfie-get
   "/coolpeople.html"
   (lambda (data)
     (funcall
      k
      (--map
       (s-replace "@" "" (car (last (s-split "/" (cdaadr it)))))
       (dom-by-tag data 'a))))))

(defun w/uwoomfie-get-status (user)
  "Return the Uwoomfie status for USER."
  (cond
   ((-contains? w/uwoomfie-cool-people user) 'cool)
   ((-contains? w/uwoomfie-honorary-viewers user) 'honored)
   (t nil)))

(w/uwoomfie-fetch-honorary-viewers
 (lambda (users)
   (setf w/uwoomfie-honorary-viewers users)))

(w/uwoomfie-fetch-cool-people
 (lambda (users)
   (setf w/uwoomfie-cool-people (cons "Watchmakering" users))))

(provide 'wasp-uwoomfie)
;;; wasp-uwoomfie.el ends here
