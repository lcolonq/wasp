;;; wasp-twitch --- Twitch integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'evil)
(require 'wasp-utils)
(require 'wasp-bus)
(require 'wasp-chat)
(require 'wasp-user)
(require 'wasp-hexamedia)
(require 'wasp-copfish)
(require 'wasp-shindaggers)
(require 'wasp-hex)

;; gizmos
(require 'wasp-biblicality)

(defcustom w/twitch-avatar-cache-dir (w/asset "avatars/")
  "The directory in which to store downloaded avatar images."
  :type '(string)
  :group 'wasp)

(defcustom w/twitch-emote-cache-dir (w/asset "emotes/")
  "The directory in which to store downloaded emote images."
  :type '(string)
  :group 'wasp)

(defcustom w/twitch-7tv-emote-cache-dir (w/asset "7tv-emotes/")
  "The directory in which to store downloaded 7TV emote images."
  :type '(string)
  :group 'wasp)

(defcustom w/twitch-api-server "https://api.twitch.tv/helix"
  "Server URL for Twitch API."
  :type '(string)
  :group 'wasp)

(defcustom w/twitch-7tv-api-server "https://7tv.io/v3"
  "Server URL for 7TV API."
  :type '(string)
  :group 'wasp)

(defvar w/twitch-last-response nil)
(defvar w/twitch-7tv-last-response nil)
(defconst w/twitch-vip-max 55)
(defvar w/twitch-vip-list nil)
(defvar w/twitch-7tv-emote-map nil)
(defvar w/twitch-chat-history nil)
(defvar w/twitch-current-stream-title nil)
(defvar w/twitch-emote-frame-counter 0)
(defvar w/twitch-emote-frame-timer nil)
(defvar w/twitch-redeems nil)
(defvar w/twitch-chat-commands nil)
(defvar w/twitch-gamer-counter 0)
(defvar w/twitch-sub-alert-cooldown 0)
(defvar w/twitch-boosters nil)
(defvar w/twitch-tsoobers nil)

(defun w/twitch-api-endpoint-test ()
  "Get LOC from the Twitch API, passing the returned JSON to K."
  (request
    "https://api.twitch.tv/helix/users?login=lcolonq"
    :type "GET"
    :headers
    `(("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :error
    (cl-function
     (lambda (&key response &allow-other-keys)
       (message "error:")
       (print response)))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (message "success:")
       (print data)))))

(defun w/twitch-api-get (loc k)
  "Get LOC from the Twitch API, passing the returned JSON to K."
  (request
    (s-concat w/twitch-api-server loc)
    :type "GET"
    :headers
    `(("Authorization" . ,w/sensitive-twitch-user-token)
      ("Client-Id" . ,w/sensitive-twitch-client-id)
      ("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/twitch-last-response data)
       (funcall k data))))
  t)

(defun w/twitch-api-post (loc fields k)
  "Post FIELDS to LOC at the Twitch API, passing the returned JSON to K."
  (request
    (s-concat w/twitch-api-server loc)
    :type "POST"
    :data (json-encode fields)
    :headers
    `(("Authorization" . ,w/sensitive-twitch-user-token)
      ("Client-Id" . ,w/sensitive-twitch-client-id)
      ("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :error
    (cl-function
     (lambda (&key data &allow-other-keys)
       (print data)
       (message "error")))
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/twitch-last-response data)
       (funcall k data))))
  t)

(defun w/twitch-7tv-api-get (loc k)
  "Get LOC from the 7TV API, passing the returned JSON to K."
  (request
    (s-concat w/twitch-7tv-api-server loc)
    :type "GET"
    :headers
    `(("Content-Type" . "application/json"))
    :parser #'json-parse-buffer
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (setq w/twitch-7tv-last-response data)
       (funcall k data))))
  t)
(defun w/twitch-7tv-update-emotes ()
  "Download the current list of 7TV emotes and populate `w/7tv-emote-map'."
  (w/twitch-7tv-api-get
   (s-concat "/users/twitch/" w/twitch-broadcaster-id)
   (lambda (data)
     (let* ((emotes (ht-get (ht-get data "emote_set") "emotes")))
       (setq w/twitch-7tv-emote-map (ht-create))
       (--each (seq-into emotes 'list)
         (ht-set! w/twitch-7tv-emote-map (ht-get it "name") (ht-get it "id")))))))

(defun w/twitch-cache-emote (name id)
  "Add an association between emote NAME and ID in the cache."
  (w/db-hset "emotes" name id))
(defun w/twitch-get-emote (name k)
  "Retrieve the emote ID for NAME and pass it to K."
  (w/db-hget
   "emotes" name
   (lambda (d)
     (funcall k (if (s-present? d) d nil)))))

(defun w/twitch-get-7tv-emote (name)
  "Retrieve the 7TV emote ID for NAME."
  (when w/twitch-7tv-emote-map
    (ht-get w/twitch-7tv-emote-map name)))

(defun w/twitch-user-avatar-path (user)
  "Get the path to USER's avatar."
  (s-concat w/twitch-avatar-cache-dir user ".png"))

(defun w/twitch-update-title ()
  "Get our stream title and update `w/twitch-current-stream-title'."
  (w/twitch-api-get
   (s-concat "/channels?broadcaster_id=" w/twitch-broadcaster-id)
   (lambda (data)
     (let ((title (ht-get (aref (ht-get data "data") 0) "title")))
       (setq w/twitch-current-stream-title title)))))

(defun w/twitch-create-redeem (title cost prompt color input)
  "Create a new channel point redeem with TITLE COST PROMPT COLOR and INPUT."
  (w/twitch-api-post
   (s-concat "/channel_points/custom_rewards?broadcaster_id=" w/twitch-broadcaster-id)
   `(("title" . ,title)
     ("cost" . ,cost)
     ("prompt" . ,prompt)
     ("background_color" . ,color)
     ("is_user_input_required" . ,input))
   (lambda (data)
     (ignore data)
     (message "Redeem created"))))

(defun w/twitch-get-user-id (user k)
  "Get the ID for USER and pass it to K."
  (w/twitch-api-get
   (s-concat "/users?login=" user)
   (lambda (data)
     (let ((id (ht-get (aref (ht-get data "data") 0) "id")))
       (funcall k id)))))

(defun w/twitch-get-recent-clips (userid k)
  "Get clips from the last week for USERID and pass them to K."
  (w/twitch-api-get
   (s-concat "/clips?broadcaster_id=" userid)
   (lambda (data)
     (funcall k (seq-map (lambda (it) (ht-get it "url")) (ht-get data "data"))))))

(defun w/twitch-get-user-recent-clips (user k)
  "Get clips from the last week for USER and pass them to K."
  (w/twitch-get-user-id
   user
   (lambda (userid)
     (w/twitch-get-recent-clips userid k))))

(defun w/twitch-get-user-avatar (user k)
  "Download the avatar for USER and save it to the avatar cache.
K is called when the download is finished."
  (let ((path (w/twitch-user-avatar-path user)))
    (if (f-exists? path)
        (funcall k)
      (w/twitch-api-get
       (s-concat "/users?login=" user)
       (lambda (data)
         (let ((url (ht-get (aref (ht-get data "data") 0) "profile_image_url")))
           (w/write-log (format "downloading avatar: %s %s" url path))
           (make-process
            :name "wasp-download-avatar"
            :buffer nil
            :command (list "get_avatar_smol" url path)
            :sentinel
            (lambda (_ _)
              (funcall k)))))))))

(defun w/twitch-get-vip-list-handler (data)
  "Handle VIP list DATA."
  (let ((inner (ht-get data "data"))
        (pagi (ht-get data "pagination")))
    (seq-map (lambda (it) (push (ht-get it "user_login") w/twitch-vip-list)) inner)
    (when (and pagi (ht-get pagi "cursor"))
      (w/twitch-api-get
       (format
        "/channels/vips?broadcaster_id=%s&after=%s"
        w/twitch-broadcaster-id
        (ht-get pagi "cursor"))
       #'w/twitch-get-vip-list-handler))))
(defun w/twitch-get-vip-list ()
  "Fetch current VIP list to `w/twitch-vip-list'."
  (setq w/twitch-vip-list nil)
  (w/twitch-api-get
   (s-concat "/channels/vips?broadcaster_id=" w/twitch-broadcaster-id)
   #'w/twitch-get-vip-list-handler)
  t)

(defun w/twitch-add-vip (user)
  "Give VIP status to USER."
  (w/pub '(monitor twitch vip add) (list user))
  (when (> (length w/twitch-vip-list) w/twitch-vip-max)
    (w/twitch-remove-random-vip)))

(defun w/twitch-remove-vip (user)
  "Remove VIP status from USER."
  (w/pub '(monitor twitch vip remove) (list user))
  (w/twitch-get-vip-list))

(defun w/twitch-remove-random-vip ()
  "Remove VIP status from a random user."
  (let ((user (w/pick-random w/twitch-vip-list)))
    (w/write-chat-event (format "Randomly removed VIP from %s - autofloor" user))
    (w/twitch-remove-vip user)))

(defun w/twitch-shoutout (user)
  "Shoutout USER."
  (w/pub '(monitor twitch shoutout) (list user)))
(defvar w/twitch-shoutout-queue nil)
(defun w/twitch-enqueue-shoutout (user)
  "Queue up a shoutout for USER."
  (push user w/twitch-shoutout-queue))
(defvar w/twitch-shoutout-timer nil)
(defun w/twitch-run-shoutout-timer ()
  "Run the shoutout timer."
  (when w/twitch-shoutout-timer
    (cancel-timer w/twitch-shoutout-timer))
  (when-let ((user (pop w/twitch-shoutout-queue)))
    (w/twitch-shoutout user))
  (setq
   w/twitch-shoutout-timer
   (run-with-timer 150 nil #'w/twitch-run-shoutout-timer)))

(defvar w/twitch-current-poll-callback nil
  "A callback that is called and passed the poll winner when the poll concludes.")

(defvar w/twitch-current-prediction-ids nil
  "Prediction and outcome identifiers for the current prediction.")

(defun w/twitch-create-poll (title options &optional callback)
  "Create a poll with TITLE and OPTIONS.
CALLBACK will be passed the winner when the poll concludes."
  (unless w/twitch-current-poll-callback
    (setq w/twitch-current-poll-callback callback)
    (w/pub
     '(monitor twitch poll create)
     (list (s-truncate 60 (s-trim title)) options))))

(defun w/twitch-create-prediction (title options)
  "Create a prediction with TITLE and OPTIONS."
  (unless w/twitch-current-prediction-ids
    (w/pub '(monitor twitch prediction create) (list title options))))

(defun w/twitch-finish-prediction (outcome)
  "Finish the current prediction with winning OUTCOME."
  (when w/twitch-current-prediction-ids
    (w/pub
     '(monitor twitch prediction finish)
     (list (car w/twitch-current-prediction-ids)
           (car (alist-get outcome (cadr w/twitch-current-prediction-ids) nil nil #'s-equals?))))))

(defun w/twitch-say (msg)
  "Write MSG to Twitch chat."
  (let ((trimmed (s-trim msg)))
    (w/write-chat-message
     (w/make-chat-message
      :user "LCOLONQ"
      :id "866686220"
      :text trimmed
      :user-color "#616161"))
    (w/pub '(monitor twitch chat outgoing) (list trimmed))))

(defun w/twitch-add-image-over (image msg start end)
  "Add IMAGE to MSG between START and END."
  (with-temp-buffer
    (insert msg)
    (add-text-properties
     start end
     `(display
       ,image
       rear-nonsticky t))
    (buffer-string)))

(defun w/twitch-replace-emotes-randomly (msg paths)
  "Replace emotes in MSG randomly with images at PATHS."
  (with-temp-buffer
    (insert msg)
    (let ((pos (point-min)))
      (while pos
        (let ((end (next-single-property-change pos 'display)))
          (when-let ((face (get-text-property pos 'display)))
            (add-text-properties
             pos (or end (point-max))
             `(display
               ,(create-image (w/pick-random paths))
               rear-nonsticky t)))
          (setf pos end))))
    (buffer-string)))

(defun w/twitch-emote-path (emoteid)
  "Get the canonical path for EMOTEID."
  (s-concat w/twitch-emote-cache-dir emoteid))

(defun w/twitch-7tv-emote-path (emoteid)
  "Get the canonical path for EMOTEID."
  (s-concat w/twitch-7tv-emote-cache-dir emoteid))

(defun w/twitch-download-emote-then (emoteid k)
  "Ensure that EMOTEID exists in the cache and then call K."
  (let* ((path (w/twitch-emote-path emoteid))
         (url (format "https://static-cdn.jtvnw.net/emoticons/v2/%s/default/dark/1.0" emoteid)))
    (unless (f-exists? path)
      (make-process
       :name "wasp-download-emote"
       :buffer nil
       :command (list "curl" "-L" url "-o" path)
       :sentinel
       (lambda (_ _)
         (funcall k))))))

(defun w/twitch-download-7tv-emote-then (emoteid k)
  "Ensure that EMOTEID exists in the cache and then call K."
  (let* ((path (w/twitch-7tv-emote-path emoteid))
         (url (format "https://cdn.7tv.app/emote/%s/1x.webp" emoteid)))
    (if (f-exists? path)
        (funcall k)
      (make-process
       :name "wasp-download-7tv-emote"
       :buffer " *wasp-download-7tv-emote-output*"
       :command (list "get_7tv_fixed" url path)
       :sentinel
       (lambda (_ _)
         (funcall k))))))

(defun w/twitch-download-emote (emoteid)
  "Ensure that EMOTEID exists in the cache."
  (w/twitch-download-emote-then emoteid (lambda () nil)))

(defun w/twitch-download-7tv-emote (emoteid)
  "Ensure that EMOTEID exists in the cache."
  (w/twitch-download-7tv-emote-then emoteid (lambda () nil)))
(defun w/twitch-download-many-7tv-emotes (xs)
  "Download every 7TV emote in XS."
  (when xs
    (message "Downloading emote: %s" (car xs))
    (w/twitch-download-7tv-emote-then
     (w/twitch-get-7tv-emote (car xs))
     (lambda ()
       (run-with-timer
        1 nil
        (lambda ()
          (w/twitch-download-many-7tv-emotes (cdr xs))))))))

(defun w/twitch-add-7tv-emotes (msg)
  "Propertize MSG with images corresponding to 7TV emotes."
  (let* ((sp (s-split " " msg)))
    (s-join
     " "
     (--map
      (if-let* ((eid (w/twitch-get-7tv-emote it))
                (path (w/twitch-7tv-emote-path eid))
                (img (create-image path)))
          (progn
            (propertize
             it
             'display
             img
             'rear-nonsticky t))
        it)
      sp))))

(defun w/twitch-insert-7tv-emote (nm)
  "Insert a 7TV emote with NM in the current buffer."
  (when-let* ((eid (w/twitch-get-7tv-emote nm))
              (path (w/twitch-7tv-emote-path eid))
              (img (create-image path)))
    (insert
     (propertize
      nm
      'display
      img
      'rear-nonsticky t))))

(defun w/twitch-process-emote-range (er msg)
  "Given a string ER of form emoteid:start-end, add the emote MSG."
  (if (string-empty-p er)
      msg
    (when-let* ((er-split (s-split ":" er))
                (emoteid (car er-split))
                (range-split (s-split "-" (cadr er-split)))
                (start (string-to-number (car range-split)))
                (end (string-to-number (cadr range-split)))
                (emotemsg (substring msg start (+ end 1)))
                (path (w/twitch-emote-path emoteid)))
      (w/twitch-cache-emote emotemsg emoteid)
      (w/twitch-download-emote emoteid)
      (let ((img (create-image path)))
        (w/twitch-add-image-over img msg (+ start 1) (+ end 2))
        ))))

(defun w/twitch-process-emote-ranges (ers msg)
  "Apply all of ERS to MSG."
  (--reduce-from (w/twitch-process-emote-range it acc) msg ers))

(defun w/twitch-advance-frame-in-chat-buffer ()
  "Advance all animated emotes in the (visible) chat buffer by 1 frame."
  (cl-incf w/twitch-emote-frame-counter)
  (save-excursion
    (with-current-buffer (w/get-chat-buffer)
      (goto-char (point-max))
      (forward-line -10)
      (goto-char (line-beginning-position))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-property-change (point) (current-buffer))
                   (point-max))))
          (when-let* ((plist-true plist)
                      (disp (plist-get plist 'display))
                      (is-image (equal (car disp) 'image))
                      (image-props (cdr disp))
                      (image-type (plist-get image-props :type))
                      (is-gif (equal image-type 'gif))
                      (multi-frame (or (plist-get (cdr disp) :animate-multi-frame-data) (image-multi-frame-p disp)))
                      )
            (let ((frame (% w/twitch-emote-frame-counter (car multi-frame))))
              (image-show-frame disp frame)))
          (goto-char next-change))))))

(defun w/twitch-run-emote-frame-timer ()
  "Run the emote frame timer."
  (when w/twitch-emote-frame-timer
    (cancel-timer w/twitch-emote-frame-timer))
  (w/twitch-advance-frame-in-chat-buffer)
  (setq
   w/twitch-emote-frame-timer
   (run-with-timer 0.03 nil #'w/twitch-run-emote-frame-timer)))

(defun w/twitch-badges-sigil (badges)
  "Return the sigil character BADGES for the current user."
  (let* ((equity (alist-get :equity w/user-current))
         (name (s-downcase w/user-current-name))
         (max-stars (w/aoc-max-stars))
         (aoc-stars (w/aoc-lookup-stars name)))
    (apply
     #'s-concat
     (-non-nil
      (list
       (when (and aoc-stars (>= aoc-stars max-stars)) "🎄")
       (when (-contains? badges "broadcaster/1") "(it me)")
       (when (-contains? badges "moderator/1") "⚔")
       (when (-contains? badges "artist-badge/1") "🖌️")
       (when (and equity (> equity 0))
         (cond ;; The Equity Lords
          ((s-equals? name "bezelea") "♿🔔")
          ((s-equals? name "altovt") "📈")
          ((s-equals? name "prodzpod") "🌠🌌🎑")
          ((s-equals? name "faeliore") "😹")
          ((s-equals? name "vasher_1025") "🕴")
          ((s-equals? name "leadengin") "💈")
          ;; ((s-equals? name "kettlestew") "")
          ((s-equals? name "blazynights") "🀄")
          ;; ((s-equals? name "must_broke_") "")
          ((s-equals? name "bvnanana") "🧉")
          ((s-equals? name "venorrak") "📺📜")
          ;; ((s-equals? name "tf_tokyo") "")
          ((s-equals? name "devts_de") "∃")
          ((s-equals? name "trap_exit") "💀")
          ((s-equals? name "essento") "🥚")
          ((s-equals? name "tyumici") "🤌")
          ;; clone is lord ((s-equals? name "liquidcake1") "")
          ;; ((s-equals? name "loufbread_") "")
          ((s-equals? name "yellowberryhn") "🪴")
          ;; ((s-equals? name "maradyne_") "")
          ;; ((s-equals? name "sampie159") "")
          ((s-equals? name "zamielpayne") "🦉")
          ((s-equals? name "xorxavier") "🌸")
          ((s-equals? name "6horntaurus") "⚰️")
          ((s-equals? name "bytomancer") (propertize "Q" 'display (create-image (w/twitch-emote-path "emotesv2_beb191005b81486c8b1c823931c88387"))))
          ;; ((s-equals? name "henriqmarq") "")
          ;; ((s-equals? name "wyndupboy") "")
          ((s-equals? name "hellpie") "🥧")
          ((s-equals? name "steeledshield") "⛨")
          ((s-equals? name "asrael_io") (propertize "Q" 'display (create-image (w/twitch-emote-path "emotesv2_a9dc5935824a4d6792f4b48f91031fcf"))))
          (t "EL.")))
       (when (-contains? badges "vip/1") "💎")
       (when (-contains? badges "subscriber/0") "💻")
       (when (-contains? badges "founder/0") "🖥️"))))))

(defun w/twitch-handle-incoming-chat (msg)
  "Write MSG to the chat buffer, processing any commands."
  (w/write-log (format "%s" msg))
  (let ((user (w/decode-string (car msg))))
    (w/user-bind
     user
     (lambda ()
       (let* ((tags (cadr msg))
              (userid (car (w/saget "user-id" tags)))
              (color (car (w/saget "color" tags)))
              (emotes (car (w/saget "emotes" tags)))
              (badges (s-split "," (car (w/saget "badges" tags))))
              (text (w/decode-string (caddr msg)))
              (biblicality (w/bible-colorize-sentence text))
              (text-colored-bible (car biblicality))
              (text-with-emotes
               (w/twitch-add-7tv-emotes
                (w/twitch-process-emote-ranges
                 (s-split "/" emotes)
                 text-colored-bible))))
         (push (cons user text) w/twitch-chat-history)

         (w/user-stats-update)
         (w/hexamedia-update-user user)
         (w/shindaggers-update-user user)
         (w/copfish-update-user user)
         (when (s-equals? (s-downcase user) "modclonk")
           (w/obs-activate-toggle 'modclonk))
         (w/resolve-record-user user)
         (w/hex-tick user)
         (w/hex-transform
          user
          (w/make-chat-message
           :user user
           :id userid
           :text text-with-emotes
           :user-color (when (s-present? color) color)
           :biblicality (cdr biblicality)
           :sigil (w/twitch-badges-sigil badges)))
         (--each w/twitch-chat-commands
           (when (s-contains? (car it) text)
             (funcall (cdr it) user text))))))))

(defun w/twitch-handle-redeem-helper (user redeem input &optional limit)
  "Handle the channel point redeem REDEEM from USER with INPUT.
Optionally, only apply redeems with point costs less than LIMIT."
  (unless (-contains? w/user-hell (s-downcase user))
    (let ((handler (alist-get redeem w/twitch-redeems nil nil #'cl-equalp)))
      (if handler
          (if (or (not limit) (< (car handler) limit))
              (w/user-bind
               user
               (lambda ()
                 (condition-case err
                     (funcall (cadr handler) user input)
                   (error
                    (w/write-chat-event (format "Error during redeem: %s" err))))))
            (w/write-chat-event (format "User %s attempted to activate overly expensive redeem \"%s\" via API" user redeem)))
        (w/write-chat-event (format "Unknown channel point redeem: %S" redeem))))))

(defun w/twitch-handle-redeem (r)
  "Handle the channel point redeem R."
  (w/write-log r)
  (let* ((user (car r))
         (redeem (cadr r))
         (encoded-input (caddr r))
         (input (when encoded-input (w/decode-string encoded-input))))
    (w/twitch-handle-redeem-helper user redeem input)))

(defun w/twitch-handle-redeem-api (r)
  "Handle a channel point redeem R coming from the API."
  (w/write-log r)
  (let* ((encoded-user (car r))
         (encoded-redeem (cadr r))
         (encoded-input (caddr r))
         (user (when encoded-user (w/decode-string encoded-user)))
         (redeem (when encoded-redeem (w/decode-string encoded-redeem)))
         (input (when encoded-input (w/decode-string encoded-input))))
    (w/twitch-handle-redeem-helper user redeem input 1000)))

(provide 'wasp-twitch)
;;; wasp-twitch.el ends here
