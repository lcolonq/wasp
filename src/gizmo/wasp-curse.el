;;; wasp-curse --- A curse -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'cl-lib)

(defun w/curse-run (prog)
  "Run PROG and return the output."
  (let ((tmpfile (make-temp-file "curse.js")))
    (with-temp-file tmpfile (insert prog))
    (shell-command-to-string (format "node '%s'" tmpfile))))

(defun w/curse-name (name)
  "Convert NAME to a cursed name."
  (replace-regexp-in-string "[-]" "_" (format "%s" name)))

(defconst w/binary-ops
  '((or . "||")
    (and . "&&")
    (> . ">")
    (>= . ">=")
    (< . "<")
    (<= . "<=")
    (= . "===")
    (% . "%")
    (/ . "/")
    (bit-or . "|")
    (bit-and . "&")
    (bit-xor . "^")))

(defun w/curse-expr (expr)
  "Translate EXPR according to the nature of the curse."
  (cond
   ((null expr) "(null)")
   ((listp expr)
    (pcase (car expr)
      ((or '+ '- '*)
       (format
        "(%s)"
        (s-join (format "%s" (car expr)) (-map #'w/curse-expr (cdr expr)))))
      ((pred (lambda (x) (alist-get x w/binary-ops)))
       (format
        "(%s)"
        (s-join (alist-get (car expr) w/binary-ops) (-map #'w/curse-expr (cdr expr)))))
      ((or '<< '>>)
       (format
        "(%s%s%s)"
        (w/curse-expr (cadr expr))
        (format "%s" (car expr))
        (w/curse-expr (caddr expr))))
      ('comment "(null)")
      ('lambda
        (format
         "((%s)=>(%s))"
         (s-join "," (-map #'w/curse-name (cadr expr)))
         (s-join "," (-map #'w/curse-expr (cddr expr)))))
      ('async-lambda
        (format
         "(async(%s)=>(%s))"
         (s-join "," (-map #'w/curse-name (cadr expr)))
         (s-join "," (-map #'w/curse-expr (cddr expr)))))
      ('if
        (format
         "(%s?%s:%s)"
         (w/curse-expr (cadr expr))
         (w/curse-expr (caddr expr))
         (w/curse-expr (cadddr expr))))
      ('define
        (format
         "(globalThis.%s=%s)"
         (w/curse-name (cadr expr))
         (w/curse-expr (caddr expr))))
      ('set
        (format
         "(%s=%s)"
         (w/curse-name (cadr expr))
         (w/curse-expr (caddr expr))))
      ('aset
        (format
         "(%s[%s]=%s)"
         (w/curse-name (cadr expr))
         (w/curse-expr (caddr expr))
         (w/curse-expr (cadddr expr))))
      ('new
        (format
         "(new %s(%s))"
         (w/curse-expr (cadr expr))
         (s-join "," (-map #'w/curse-expr (cddr expr)))))
      ('await
        (format
         "(await %s)"
         (w/curse-expr (cadr expr))))
      ('not
        (format
         "(!%s)"
         (w/curse-expr (cadr expr))))
      ('let
        (format
         "(((%s)=>(%s))(%s))"
         (s-join "," (--map (w/curse-name (car it)) (cadr expr)))
         (s-join "," (-map #'w/curse-expr (cddr expr)))
         (s-join "," (--map (w/curse-expr (cadr it)) (cadr expr)))))
      ('async-let
        (format
         "((async(%s)=>(%s))(%s))"
         (s-join "," (--map (w/curse-name (car it)) (cadr expr)))
         (s-join "," (-map #'w/curse-expr (cddr expr)))
         (s-join "," (--map (w/curse-expr (cadr it)) (cadr expr)))))
      ('do
        (format
         "((()=>(%s))())"
         (s-join "," (-map #'w/curse-expr (cdr expr)))))
      ('iota
        (format
         "[...Array(%s).keys()]"
         (w/curse-expr (cadr expr))))
      ('array
       (format
         "[%s]"
         (s-join "," (-map #'w/curse-expr (cdr expr)))))
      ('object
       (format
        "{%s}"
        (s-join
         ","
         (--map (format "%s:%s" (w/curse-name (car it)) (w/curse-expr (cadr it))) (cdr expr)))))
      ('@
       (format
        "((%s)[%s])"
        (w/curse-expr (cadr expr))
        (w/curse-expr (caddr expr))))
      (_
       (format
        "((%s)(%s))"
        (w/curse-expr (car expr))
        (s-join "," (-map #'w/curse-expr (cdr expr)))))
      ))
   ((symbolp expr) (w/curse-name expr))
   ((numberp expr) (format "%s" expr))
   ((stringp expr) (format "\"%s\"" expr))
   (t "(null)")))

(defun w/curse-current-buffer ()
  "Transmute the current buffer according to the curse."
  (interactive)
  (let* ((srcfile (buffer-file-name))
         (jspath (s-concat (f-base srcfile) ".js"))
         (src (buffer-string)))
    (with-temp-buffer
      (insert src)
      (goto-char (point-min))
      (let ((acc "")
            (line (read (current-buffer))))
        (while (and line (not (eobp)))
          (setf acc (s-concat acc (w/curse-expr line) ";"))
          (setf
           line
           (condition-case nil
               (read (current-buffer))
             (error nil))))
        (write-region acc nil jspath)))))

(provide 'wasp-curse)
;;; wasp-curse.el ends here
