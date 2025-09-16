;;; wasp-flymake --- Flymake backend -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 'ht)
(require 's)
(require 'flymake)

(defvar w/flymake-errors (ht-create))

(defun w/flymake-thing-bounds (pos)
  "Return the bounds of the thing to highlight at POS."
  (save-excursion
    (goto-char pos)
    (or
      (bounds-of-thing-at-point 'symbol)
      (bounds-of-thing-at-point 'sexp)
      (bounds-of-thing-at-point 'line))))

(defun w/flymake-thing (pos)
  "Return the thing highlighted at POS."
  (-let [(begin . end) (w/flymake-thing-bounds pos)]
    (buffer-substring-no-properties begin end)))

(defun w/flymake-error (user msg)
  "Create a new Flymake error at the cursor from USER saying MSG."
  (when (and flymake-mode (buffer-file-name))
    (push (list (point) (w/flymake-thing (point)) user msg)
      (ht-get w/flymake-errors (buffer-file-name)))
    (flymake-start)))

(defun w/flymake-prune ()
  "Remove invalidated Flymake errors."
  (let*
    ( (errs (ht-get w/flymake-errors (buffer-file-name)))
      (pruned
        (--filter
          (when-let* ((sym (w/flymake-thing (car it))))
            (s-equals? sym (cadr it)))
          errs)))
    (ht-set! w/flymake-errors (buffer-file-name) pruned)))

(defun w/flymake-backend (report-fn &rest _)
  "Flymake backend for stream errors. Calls REPORT-FN."
  (w/flymake-prune)
  (-let [errs (ht-get w/flymake-errors (buffer-file-name))]
    (funcall report-fn
      (--map
        (-let [(begin . end) (w/flymake-thing-bounds (car it))]
          (flymake-make-diagnostic
            (current-buffer)
            begin end
            :note
            (format "%s: %s" (caddr it) (cadddr it))))
        errs))))

(defun w/flymake-setup ()
  "Setup stream Flymake errors."
  (add-hook 'flymake-diagnostic-functions #'w/flymake-backend nil t))
(add-hook 'prog-mode-hook #'w/flymake-setup)

(provide 'wasp-flymake)
;;; wasp-flymake.el ends here
