;;; prod-bless-sfri --- Super Required Free Ideas -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'fig-bless)
(require 'dash)
(require 's)

(defvar prod-bless-sfri
    (list
        ; irreplacable (within reasonable fuel) functions
        (cons 'reverse (fig//bless-helper-unary 'reverse))
        (cons 'concat (fig//bless-helper-binary (lambda (a b) (concat a b))))
        (cons 'substring (lambda (s) (fig//bless-manage-fuel) (cons (substring (nth 2 s) (cadr s) (car s)) (nthcdr 3 s))))
        (cons 'contains (fig//bless-helper-binary (lambda (a b) (s-contains? b a)))) ; car needle, cadr haystack
        (cons 'number (fig//bless-helper-unary 'string-to-number))
        (cons 'string (fig//bless-helper-unary 'number-to-string))
        (cons 'random (fig//bless-helper-unary 'random))
        ; semi-replacable functions
        (cons 'length (fig//bless-helper-unary 'length))
        (cons 'bundle (lambda (s) (fig//bless-manage-fuel) (cons (-take (car s) (-drop 1 s)) (-drop (+ (car s) 1) s))))
        (cons 'split (fig//bless-helper-binary (lambda (a b) (s-split b a))))
        (cons 'join (fig//bless-helper-binary (lambda (a b) (s-join b a))))
        (cons 'filter (fig//bless-helper-binary (lambda (a b) (-filter b a))))
        (cons 'any (fig//bless-helper-binary (lambda (a b) (-any? b a))))
        (cons 'all (fig//bless-helper-binary (lambda (a b) (-all? b a))))
        ; replacable (but forthy) functions
        (cons 'rot (lambda (s) (fig//bless-manage-fuel) (cons (-last-item s) (-butlast s))))
        (cons 'over (lambda (s) (fig//bless-manage-fuel) (cons (cadr s) s)))))