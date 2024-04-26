;;; wasp-newspaper --- The Effort Post -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'wasp-utils)
(require 'wasp-db)

(defvar w/newspaper-todays-articles nil)

(defconst w/newspaper-slogans
  (list
   "hello computer"
   "only on !discord IRC"
   "GoMoCo HaThPl"
   "good morning computer"
   "hack the planet"
   "!oomfie"
   "All the news that's fit to prin1"
   "I use arch by the way"
   "play void stranger (2023)"
   "[i](this was sent from godot)[i]"
   "LCOLONQ Lies in LaTeX"
   "Super idol's smile / Is not as sweet as yours / The sunlight at noon in August / Does not shine like you / Love the 105 °C you / Distilled water that is pure every drop"
   "this is where we read about the computer"
   "brought to you by viewers like you. thank you!"
   ))

(defconst w/newspaper-prices
  (list
   "1 COLON"
   "3 to 5"
   "501 Internal Server Error"
   "$3.50"
   "206 bpm"
   "1 boost"
   "a snack for friend"
   "59 frames per second"))

(w/defstruct
 w/newspaper-article
 headline
 author
 content)

(defun w/newspaper-wrap-emoji (s)
  "Wrap emoji with appropriate TeX in S."
  (s-replace-regexp "[^[:ascii:]]" (lambda (c) (format "{\\\\figemote %s}" c)) s))

(defun w/newspaper-escape (s)
  "Apply appropriate subsitutions to S."
  (s-replace-regexp
   (rx "\"" (one-or-more (not "\"")) "\"")
   (lambda (x)
     (s-concat "``" (s-chop-suffix "\"" (s-chop-prefix "\"" x)) "''"))
   (s-replace-all
    '(("&" . "\\&")
      ("%" . "\\%")
      ("$" . "\\$")
      ("#" . "\\#")
      ("_" . "\\_")
      ("{" . "\\{")
      ("}" . "\\}")
      ("~" . "\\textasciitilde")
      ("^" . "\\textasciicircum")
      ("\\" . "\\textbackslash"))
    s)
   nil
   t))

(defun w/newspaper-article-tex (a)
  "Convert an article A to TeX source."
  (s-concat
   "\\byline{"
   (w/newspaper-wrap-emoji (w/newspaper-escape (w/newspaper-article-headline a)))
   "}{"
   (w/newspaper-wrap-emoji (w/newspaper-escape (w/newspaper-article-author a)))
   "}\n"
   (w/newspaper-wrap-emoji (w/newspaper-escape (w/newspaper-article-content a)))
   "\n\\closearticle\n"))

(w/defstruct
 w/newspaper
 slogan
 price
 articles
 (edition 1))

(defun w/newspaper-tex (np)
  "Convert a newspaper NP to TeX source."
  (s-replace-all
   (list
    (cons "FIG_EDITION" (number-to-string (w/newspaper-edition np)))
    (cons "FIG_SLOGAN" (w/newspaper-slogan np))
    (cons "FIG_PRICE" (w/newspaper-price np))
    (cons "FIG_ARTICLES" (apply #'s-concat (-map #'w/newspaper-article-tex (w/newspaper-articles np))))
    )
   (w/slurp (w/asset "newspaper/template.tex"))))

(defun w/newspaper-pdf (src k)
  "Build TeX SRC to PDF.
Pass the path of the generated PDF to K."
  (when (get-buffer "*wasp-newspaper-pdf*")
    (with-current-buffer "*wasp-newspaper-pdf*"
      (erase-buffer)))
  (let ((dir (make-temp-file "wasp-newspaper" t))
        (srcfile (w/tempfile "wasp-newspaper-src" src ".tex")))
    (make-process
     :name "wasp-newspaper-pdf"
     :buffer "*wasp-newspaper-pdf*"
     :command (list "print-newspaper" srcfile dir)
     :sentinel
     (lambda (_ _)
       (funcall k (f-join dir "newspaper.pdf"))))))

(defvar w/newspaper-test-issue
  (w/make-newspaper
   :slogan "hello computer" :price "3 to 5"
   :articles
   (list
    (w/make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (w/make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (w/make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    (w/make-newspaper-article
     :headline "omg hi oomfie"
     :author "Joel"
     :content "\\lipsum[1]")
    )))

(defun w/newspaper ()
  "Generate and open today's work-in-progress newspaper."
  (interactive)
  (w/db-get
   "newspaper:edition"
   (lambda (edition)
     (w/newspaper-pdf
      (w/newspaper-tex
       (w/make-newspaper
        :slogan (w/pick-random w/newspaper-slogans) :price (w/pick-random w/newspaper-prices)
        :edition (string-to-number edition)
        :articles
        w/newspaper-todays-articles))
      #'find-file))))

(defun w/newspaper-publish ()
  "Finalize and publish today's work-in-progress newspaper."
  (interactive)
  (w/db-get
   "newspaper:edition"
   (lambda (edstr)
     (let ((edition (string-to-number edstr)))
       (w/newspaper-pdf
        (w/newspaper-tex
         (w/make-newspaper
          :slogan (w/pick-random w/newspaper-slogans) :price (w/pick-random w/newspaper-prices)
          :edition edition
          :articles
          w/newspaper-todays-articles))
        (lambda (path)
          (make-process
           :name "fig-newspaper-publish"
           :command (list "scp" path (format "llll@pub.colonq.computer:~/public_html/news/%03d.pdf" edition))
           :sentinel
           (lambda (_ _)
             (w/db-set "newspaper:edition" (number-to-string (1+ edition)))
             (browse-url (format "https://pub.colonq.computer/~llll/news/%03d.pdf" edition))
             ))))))))

(provide 'wasp-newspaper)
;;; wasp-newspaper.el ends here
