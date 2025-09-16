;;; wasp-genealogy --- Colonq Family Genealogy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-ai)

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(load (w/asset "irish/genealogy-data.el"))

(cl-defstruct
    (w/ancestor
     (:constructor w/make-ancestor))
  title
  firstname
  middlename
  nickname
  namesake
  job
  dateofbirth
  dateofdeath
  causeofdeath)

(defun w/ancestor-serialize (a)
  "Serialize an ancestor A."
  (list
   (w/ancestor-title a)
   (w/ancestor-firstname a)
   (w/ancestor-middlename a)
   (w/ancestor-nickname a)
   (w/ancestor-namesake a)
   (w/ancestor-job a)
   (w/ancestor-dateofbirth a)
   (w/ancestor-dateofdeath a)
   (w/ancestor-causeofdeath a)))

(defun w/generate-name (era)
  "Determine an appropriate name from ERA."
  (let ((names (alist-get era w/era-names)))
    (nth (random (length names)) names)))

(defun w/generate-job (era)
  "Determine an appropriate job from ERA."
  (let ((jobs (alist-get era w/era-jobs)))
    (nth (random (length jobs)) jobs)))

(defun w/generate-nickname (era)
  "Determine an appropriate job from ERA."
  (let ((nicknames (alist-get era w/era-nicknames)))
    (nth (random (length nicknames)) nicknames)))

(defun w/generate-birth-year ()
  "Determine which year the astrally-focused ancestor was born."
  (+ 1000 (random 1000)))

(defun w/year-era (year)
  "Determine the era for YEAR."
  (cond
   ((< year 1200) 'medievaltimes)
   ((< year 1400) 'ageofsail)
   ((< year 1800) 'renaissance)
   ((< year 1900) 'steampunk)
   (t 'modern)))

(defun w/decide-title (era job)
  "Determine the title for JOB in ERA."
  (cl-case era
    (medievaltimes (alist-get job w/medieval-titles nil nil #'s-equals?))
    (steampunk "Sir")
    (t nil)))

(defun w/generate-cause-of-death (anc k)
  "Determine ANC's cause of death and pass it to K."
  (w/ai
   (w/describe-ancestor-short anc)
   k
   "Given a description of a fictional person, invent a plausible cause of death. The output should be no more than a single clause."
   "Kingkaliente Vasher_1025 \"Grimaldi\" Colonq
Born 1429
Died 1519
Employed as: painting hanger"
   "fell off a ladder"))

(defun w/generate-ancestor (user k)
  "Search the genealogical record to find USER's namesake and pass the result to K."
  (let* ((birthyear (w/generate-birth-year))
         (era (w/year-era birthyear))
         (job (w/generate-job era))
         (has-nickname (= 0 (random 10)))
         (nickname (when has-nickname (w/generate-nickname era)))
         (ret
          (w/make-ancestor
           :title (w/decide-title era job)
           :nickname nickname
           :namesake user
           :job job
           :dateofbirth birthyear
           :dateofdeath (+ birthyear (random 100))
           :firstname (w/generate-name era))))
    (if (= 0 (random 2))
        (w/generate-cause-of-death
         ret
         (lambda (causeofdeath)
           (setf (w/ancestor-causeofdeath ret) causeofdeath)
           (funcall k ret)))
      (setf (w/ancestor-causeofdeath ret) "unknown")
      (funcall k ret)))
  nil)

(defun w/ancestor-name (anc)
  "Return the full name of ANC."
  (s-concat
   (if-let* ((tit (w/ancestor-title anc))) (s-concat tit " ") "")
   (w/ancestor-firstname anc) " "
   (s-titleize (w/ancestor-namesake anc)) " "
   (if-let* ((nn (w/ancestor-nickname anc))) (s-concat "\"" nn "\" ") "")
   "Colonq"
   ))

(defun w/describe-ancestor-short (anc)
  "Describe ANC."
  (s-concat
   (w/ancestor-name anc) "\n"
   (format "Born %s\n" (w/ancestor-dateofbirth anc))
   (format "Died %s\n" (w/ancestor-dateofdeath anc))
   (format "Employed as: %s\n" (w/ancestor-job anc))))

(defun w/describe-ancestor (anc)
  "Describe ANC."
  (s-concat
   (w/describe-ancestor-short anc)
   (format "Cause of death: %s\n" (w/ancestor-causeofdeath anc))))

(provide 'wasp-genealogy)
;;; wasp-genealogy.el ends here
