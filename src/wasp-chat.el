;;; wasp-chat --- Chat display -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'evil)
(require 'wasp-utils)
(require 'wasp-user)

(defcustom w/chat-buffer "*wasp-chat*"
  "Name of buffer used to store the chat log."
  :type '(string)
  :group 'wasp)

(defvar w/chat-joel-count 0)
(defvar w/chat-plus2-count 0)
(defvar w/chat-minus2-count 0)
(defvar w/chat-icant-count 0)
(defvar w/chat-bpm-count 0)

(defvar w/chat-header-line "")

(defun w/chat-update-header-line ()
  "Update `w/chat-header-line'."
  (setf
   w/chat-header-line
   (s-concat
    "  Joel: " (format "%s" w/chat-joel-count)
    " | ICANT: " (format "%s" w/chat-icant-count)
    " | +2: " (format "%s" w/chat-plus2-count)
    " | -2: " (format "%s" w/chat-minus2-count))))

(define-derived-mode w/chat-overlay-mode special-mode "ClonkHead Stats"
  "Major mode for displaying chatter statistics."
  :group 'wasp
  (setq mode-line-format nil))

(defun w/get-chat-overlay-buffer (user)
  "Return the stats buffer for USER."
  (let ((name (format "*wasp-chatter %s*" user)))
    (unless (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (w/chat-overlay-mode)))
    (get-buffer name)))

(defface w/chat-overlay-title
  '((t
     :foreground "white"
     :height 300
     ))
  "Face for title."
  :group 'wasp)

(defface w/chat-overlay-category
  '((t
     :foreground "green"
     ))
  "Face for title."
  :group 'wasp)

(defconst w/chat-overlay-element-display-info
  '(("fire" "🔥" "red")
    ("water" "🌊" "blue")
    ("wind" "🍃️" "green")
    ("earth" "🪨" "brown")
    ("lightning" "⚡" "yellow")
    ("heart" "🩷" "pink")
    ))
(defun w/chat-overlay-display-element (e)
  "Return a propertized string representing E."
  (if-let ((dinfo (alist-get e w/chat-overlay-element-display-info nil nil #'s-equals?)))
      (propertize
       (format "%s %s" (car dinfo) e)
       'face (list :foreground (cadr dinfo)))
    "O.O unknown?"))
(defun w/chat-overlay-render (user)
  "Render the stats buffer for USER."
  (w/user-get
   user
   (lambda (db)
     (with-current-buffer (w/get-chat-overlay-buffer user)
       (let* ((inhibit-read-only t)
              (faction (alist-get :faction db))
              (element (alist-get :element db))
              (boosts (alist-get :boost db)))
         (erase-buffer)
         (w/write-line user 'w/chat-overlay-title)
         (w/write
          (format
           "Faction: %s"
           (propertize
            (format "%s" (or faction "EXEMPT"))
            'face
            (list
             :foreground
             (cl-case faction
               (nate "pink")
               (lever "lightblue")
               (tony "lightgreen")
               (t "white"))))))
         (w/write-line
          (cond
           ((not boosts) " (objector)")
           ((> boosts 0) (format " (boost %s)" boosts))
           (t (format " (%s tsoob)" boosts))))
         (w/write-line
          (format
           "Element: %s"
           (w/chat-overlay-display-element element)))
         (goto-char (point-min)))))))

(defvar w/chat-overlay-frame nil)
(defvar w/chat-overlay-cur nil)
(defun w/create-chat-overlay-frame ()
  "Build a frame for displaying chatter stats on mouseover."
  (when (framep w/chat-overlay-frame)
    (delete-frame w/chat-overlay-frame))
  (setf
   w/chat-overlay-frame
   (make-frame
    (append
     `((name . "clonkhead-io")
       (wasp-prevent-focus . t)
       (unsplittable . t)
       (undecorated . t)
       (no-accept-focus . t)
       (no-focus-on-map . t)
       (override-redirect . t)
       (user-size . t)
       (width . 30)
       (height . 15)
       (user-position . t)
       (left . -1)
       (top . -1)
       (default-minibuffer-frame . ,(selected-frame))
       (minibuffer . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (cursor-type . nil)
       (background-color . "black"))))))

(defun w/show-chat-overlay-frame (vis)
  "If VIS is non-nil, make the chat overlay frame visible.
Otherwise make it invisible."
  (if vis
      (make-frame-visible w/chat-overlay-frame)
    (setq w/chat-overlay-cur nil)
    (make-frame-invisible w/chat-overlay-frame)))
(defun w/move-chat-overlay-frame (x y)
  "Move the chat overlay frame to X, Y."
  (modify-frame-parameters
   w/chat-overlay-frame
   (list
    (cons 'top y)
    (cons 'left x))))
(defun w/display-chat-overlay (user &optional x y)
  "Display the chat overlay buffer for USER.
Optionally display the window at X, Y"
  (unless w/chat-overlay-frame
    (w/create-chat-overlay-frame))
  (let ((window (frame-selected-window w/chat-overlay-frame)))
    (if (and x y)
        (w/move-chat-overlay-frame x y)
      (w/move-chat-overlay-frame -1 -1))
    (w/chat-overlay-render user)
    (setq w/chat-overlay-cur user)
    (set-window-buffer window (w/get-chat-overlay-buffer user))
    (w/show-chat-overlay-frame t)))
(defun w/update-chat-overlay (user pos)
  "Update the chat overlay frame for USER based on POS."
  (if (and user pos)
      (progn
        (unless (equal (cons user pos) w/chat-overlay-cur)
          (w/display-chat-overlay user (car pos) (cdr pos)))
        )
    (w/show-chat-overlay-frame nil)))
(defun w/handle-chat-overlay ()
  "Handle point movement for chat overlay popup."
  (with-current-buffer (w/get-chat-buffer)
    (w/update-chat-overlay
     (get-text-property (point) 'wasp-user)
     (window-absolute-pixel-position (point)))))

(define-derived-mode w/chat-mode special-mode "Chat"
  "Major mode for displaying chat."
  :group 'wasp
  (add-hook 'post-command-hook #'w/handle-chat-overlay nil t)
  (advice-add 'handle-switch-frame :before-while #'w/prevent-focus-frame)
  (setq-local window-point-insertion-type t)
  (cond
   (t (setq-local header-line-format '(:eval w/chat-header-line)))))

(defun w/get-chat-buffer (&optional nm)
  "Return the chat buffer.
Optionally, return the buffer NM in chat mode."
  (let ((bufnm (or nm w/chat-buffer)))
    (unless (get-buffer bufnm)
      (with-current-buffer (get-buffer-create bufnm)
        (w/chat-mode)))
    (get-buffer bufnm)))

(defun w/clear-chat ()
  "Clear the chat buffer."
  (interactive)
  (with-current-buffer (w/get-chat-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defvar-keymap w/chat-mode-map
  :suppress t
  "C-l" #'w/clear-chat)
(evil-define-key 'motion w/chat-mode-map (kbd "<return>") #'w/open-link)

(defun w/write-chat-event (ev)
  "Write the string EV to the chat buffer as an event (italicized)."
  (let ((inhibit-read-only t))
    (with-current-buffer (w/get-chat-buffer)
      (goto-char (point-max))
      (insert (propertize ev 'face 'italic))
      (insert "\n"))))

(w/defstruct
 w/chat-message
 user
 id
 text
 user-color
 sigil
 faction
 biblicality)

(defun w/chat-button-action (b)
  "Action run on button press for button B."
  (let ((user (get-text-property (button-start b) 'wasp-user)))
    (message user)))

(defconst w/chat-substitution-godot-logo
  (w/image-text (w/asset "misc/godot.png")))
(defconst w/chat-substitution-powershell-logo
  (w/image-text (w/asset "misc/powershell_small.png")))
(defconst w/chat-substitutions
  `(("[i](this was sent from godot)[/i]" . ,w/chat-substitution-godot-logo)
    ("bald" . "ball")
    ("pokemon" . "pal")
    ("Pokemon" . "Pal")
    ("POKEMON" . "PAL")
    ("pal" . "pokemon")
    ("Pal" . "Pokemon")
    ("PAL" . "POKEMON")
    ("darkrai" . "*******")
    ("hunter2" . "*******")
    ("*******" . "hunter2")))

(defun w/write-chat-message (msg)
  "Write MSG to the chat buffer as USER with USERID and COLOR."
  (let ((inhibit-read-only t))
    (with-current-buffer (w/get-chat-buffer)
      (goto-char (point-max))
      (insert-text-button
       (s-concat
        (if (w/. sigil msg) (s-concat (w/. sigil msg) " ") "")
        (w/. user msg))
       'face (list :foreground (or (w/. user-color msg) "#ffffff") :weight 'bold)
       'wasp-user (w/. user msg)
       'wasp-user-id (w/. id msg)
       'action #'w/chat-button-action)
      (insert
       (propertize
        ": "
        'face
        (list
         :foreground
         (cl-case (w/. faction msg)
           (nate "pink")
           (lever "lightblue")
           (tony "lightgreen")
           (t "white"))
         )
        ))
      (insert (s-replace-all w/chat-substitutions (w/. text msg)))
      (when (w/. biblicality msg)
        (let* ((wwidth (- (window-total-width (get-buffer-window (current-buffer))) 3))
               (bible-button-text (format "[biblicality %.2f]" (w/. biblicality msg)))
               (msgwidth (line-beginning-position))
               (lines (+ 1 (/ msgwidth wwidth))))
          (insert
           (propertize
            " " 'display
            `(space
              :align-to
              ,(- (+ (* wwidth lines) (- lines 1))
                  (length bible-button-text)
                  ))))
          (insert
           (propertize
            bible-button-text
           'face '(:foreground "#bbbbbb")))))
      (insert "\n"))))

(provide 'wasp-chat)
;;; wasp-chat.el ends here
