;;; wasp-aoc --- Advent of Code API access -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)
(require 'wasp-sensitive)

(defcustom w/aoc-leaderboard-url "https://adventofcode.com/2024/leaderboard/private/view/3307583.json"
  "URL for Advent of Code API."
  :type '(string)
  :group 'wasp)

(defvar w/aoc-last-response nil)
(defvar w/aoc-user-stars nil)
(defconst w/aoc-name-map
  '(("exodrifter_" . "exodrifter")
    ("cephon_altera" . "lainlayer")
    ("monochrome_0" . "monochrome")
    ("yoink2000" . "darius1702")
    ("lukeisun_" . "lukeisun")
    ("dwinkley_" . "dwinkley")
    ("lcolonq" . "llll colonq")
    ("fn_lumi" . "lumi")
    ("leadengin" . "leaden")
    ("vasher_1025" . "vash3r")
    ("andrewdtr" . "drawthatredstone")
    ("badcop_" . "cgsdev0")
    ("asrael_io" . "asrael")
    ("colinahscopy_" . "@colinahscopy")
    ("ctrl_o" . "control-o")
    ("whimsicallymade" . "aecepoglu")
    ("chromosundrift" . "christo")
    ))

(defun w/aoc-max-stars ()
  "Return the maximum Advent of Code stars for today."
  (min 50 (* 2 (string-to-number (format-time-string "%d" (current-time))))))

(defun w/aoc-lookup-stars (user)
  "Retrieve the Advent of Code stars for USER."
  (let* ((duser (s-downcase user))
         (cuser (s-downcase (alist-get duser w/aoc-name-map duser nil #'s-equals?))))
    (alist-get cuser w/aoc-user-stars nil nil #'s-equals?)))

(defun w/aoc-fetch-api (k)
  "Retrieve the current Advent of Code API.
Pass the resulting JSON to K."
  (request
    w/aoc-leaderboard-url
    :type "GET"
    :headers
    `(("Cookie" . ,(format "session=%s" w/sensitive-aoc-session-cookie)))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/aoc-last-response data)
       (funcall k data))))
  nil)

(defun w/aoc-update-user-stars ()
  "Update the Advent of Code stars list."
  (w/aoc-fetch-api
   (lambda (data)
     (setf
      w/aoc-user-stars
      (--map
       (cons (s-downcase (car it)) (cdr it))
       (--filter
        (stringp (car it))
        (--map
         (cons (ht-get it "name") (ht-get it "stars"))
         (ht-values (ht-get data "members")))))))))
;; (w/aoc-update-user-stars)

(provide 'wasp-aoc)
;;; wasp-aoc.el ends here
