;;; wasp-audio --- On-stream audio input and output -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-ai)
(require 'wasp-db)
(require 'wasp-overlay)

(defcustom w/audio-play-process "wasp-audio-play"
  "Name of process for playing audio with mpv."
  :type '(string)
  :group 'wasp)

(defcustom w/audio-record-process "wasp-audio-record"
  "Name of process for transcribing speech using the Whisper API."
  :type '(string)
  :group 'wasp)

(defvar w/audio-record-process-current nil)
(defvar w/audio-keep-recording t)
(defvar w/audio-voice-commands nil)
(defvar w/last-stream-transcription "we're going down the rabbit hole")

(defun w/tts (msg)
  "Use TTS to say MSG."
  (start-process "wasp-tts" nil "say" (w/tempfile "wasp-tts" msg)))

(defun w/audio-play (clip &optional k volume)
  "Play CLIP using mpv.
Call K when done.
If VOLUME is specified, use it to adjust the volume (100 is default)."
  (make-process
   :name w/audio-play-process
   :buffer nil
   :command
   (list
    "mpv" "--ao=alsa" "--no-video"
    (format "--volume=%s" (or volume 100))
    clip)
   :sentinel
   (lambda (_ _)
     (when k
       (funcall k)))))

(defun w/stop-all-audio ()
  "Stop all audio by killing mpv processes."
  (interactive)
  (setq w/audio-muzak-queue nil)
  (start-process "pkill" nil "pkill" "mpv")
  (start-process "pkill" nil "pkill" "muzak"))

(defun w/recorded-chatter-name? (user)
  "Return non-nil if we've recorded USER's name."
  (f-exists?
   (w/asset (s-concat "rats/users/" user ".wav"))))
(defun w/say-chatter-name (user &optional volume k)
  "Pronounce USER's name in using mpv.
Call K when done.
If VOLUME is specified, use it :)."
  (w/audio-play (w/asset (s-concat "rats/users/" user ".wav")) k volume))

(defun w/multipart-audio-helper (user rest &optional uservol clipvol)
  "Player all of the files in REST intercalated with saying USER's name.
Adjust volumes by USERVOL and CLIPVOL."
  (when (car rest)
    (w/audio-play
     (car rest)
     (when (cdr rest)
       (lambda ()
         (w/say-chatter-name
          user uservol
          (lambda () (w/multipart-audio-helper user (cdr rest) uservol clipvol)))))
     clipvol)))

(defun w/audio-rats-rats-we-are-the-rats (user)
  "Rats rats we are the rats.
Celebrating yet another birthday bash.
USER it's your birthday today."
  (let ((parts (--map (w/asset (format "rats/rats/rats%d.ogg" (+ it 1))) (-iota 3))))
    (w/multipart-audio-helper user parts 120 100)))
(defun w/audio-rambling-sub-thanks (user)
  "Say the thing about USER."
  (let ((parts (--map (w/asset (format "rats/new/part%d.wav" it)) (-iota 5))))
    (w/multipart-audio-helper user parts 120 150)))
(defun w/thank-sub (user)
  "Thank USER for their subscription user a randomized thanking technique."
  (let* ((thankers-named
          '(w/audio-rats-rats-we-are-the-rats
            w/audio-rambling-sub-thanks))
         (thankers-unnamed
          '((lambda (_) (w/audio-play (w/asset "rats/sam.wav") nil 90))
            (lambda (_) (w/audio-play (w/asset "rats/tyumici.mp3") nil 150))
            (lambda (_) (w/audio-play (w/asset "rats/abuffseagull.flac") nil 150))
            (lambda (_) (w/audio-play (w/asset "rats/unrecorded.wav") nil 150))
            ))
         (thanker
          (w/pick-random
           (-concat
            (when (w/recorded-chatter-name? user)
              thankers-named)
            thankers-unnamed))))
    (funcall thanker user)))

(defun w/audio-record-start ()
  "Start recording audio to transcribe."
  (let ((tmp (s-concat (make-temp-name "/tmp/wasp-record-audio") ".wav")))
    (unless w/audio-record-process-current
      (setq
       w/audio-record-process-current
       (make-process
        :name w/audio-record-process
        :buffer nil
        :command (list "parecord" tmp)
        :sentinel
        (lambda (_ _)
          (setq w/audio-record-process-current nil)
          (w/ai-transcribe
           tmp
           (lambda (msg)
             (f-delete tmp)
             (w/daily-log (format "[VOICE]: %s" msg))
             (setq w/last-stream-transcription msg)
             (--each w/audio-voice-commands
               (when (s-contains? (car it) (s-downcase msg))
                 (funcall (cdr it))))))
          (when w/audio-keep-recording
            (w/audio-record-start))))))))

(defun w/audio-record-end ()
  "Stop recording audio to transcribe."
  (when w/audio-record-process-current
    (start-process "pkill" nil "pkill" "parecord")))

(defvar w/audio-record-end-timer nil)
(defun w/run-audio-record-end-timer ()
  "Run the audio recording timer."
  (when w/audio-record-end-timer
    (cancel-timer w/audio-record-end-timer))
  (w/audio-record-end)
  (setq
   w/audio-record-end-timer
   (run-with-timer 10 nil #'w/run-audio-record-end-timer)))

(defun w/start-audio-record ()
  "Start recording audio."
  (interactive)
  (setq w/audio-keep-recording t)
  (w/audio-record-start))
(defun w/stop-audio-record ()
  "Stop recording audio."
  (interactive)
  (setq w/audio-keep-recording nil)
  (w/audio-record-end))

(defconst w/audio-muzak-path "/home/llll/src/muzak-rs/target/release/muzak")
(defvar w/audio-muzak-now-playing nil)
(defvar w/audio-muzak-queue nil)

(defun w/audio-muzak (user song)
  "Play SONG by USER using muzak-rs courtesy The0x539."
  (setq w/audio-muzak-now-playing (cons user song))
  (w/overlay-muzak user song)
  (let ((proc
          (make-process
            :name "wasp-muzak"
            :connection-type '(pipe . pty)
            :buffer " *wasp-muzak-log*"
            :command (list w/audio-muzak-path "play")
            :sentinel
            (lambda (_ _)
              (w/overlay-muzak-clear)
              (setq w/audio-muzak-now-playing nil)))))
    (process-send-string proc song)
    (process-send-eof proc)))

(defun w/audio-muzak-enqueue (user song)
  "Enqueue a play for SONG by USER."
  (setq w/audio-muzak-queue (-concat w/audio-muzak-queue (list (cons user song)))))

(defun w/audio-muzak-update ()
  "Keep playing songs from the queue if they exist."
  (unless w/audio-muzak-now-playing
    (when-let* ((entry (pop w/audio-muzak-queue)))
      (w/audio-muzak (car entry) (cdr entry)))))
(defvar w/audio-muzak-timer nil)
(defun w/run-audio-muzak-timer ()
  "Run the muzak timer."
  (when w/audio-muzak-timer
    (cancel-timer w/audio-muzak-timer))
  (w/audio-muzak-update)
  (setq
   w/audio-muzak-timer
   (run-with-timer 1 nil #'w/run-audio-muzak-timer)))
(w/run-audio-muzak-timer)

(defun w/add-song (title notes-string)
  "Add a song to wasp db.
TITLE specifies the name of the song.
NOTES-STRING is a string of notes and rests."
  (let ((hash (md5 (s-downcase title))))
    (w/db-hset "songnames" hash title)
    (w/db-hset "songnotes" hash notes-string)))

(defun w/get-song (song-name k)
  "Look up notes of SONG-NAME from the database.
Pass the resulting notes to K."
  (let ((hash (md5 (s-downcase song-name))))
    (w/db-hget
     "songnotes" hash
     (lambda (notes)
       (if (and notes (stringp notes) (s-present? notes))
           (funcall k notes)
         (funcall k nil))))))

(provide 'wasp-audio)
;;; wasp-audio.el ends here
