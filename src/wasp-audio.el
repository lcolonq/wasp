;;; wasp-audio --- On-stream audio input and output -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)

(defcustom w/play-audio-process "wasp-play-audio"
  "Name of process for playing audio with mpv."
  :type '(string)
  :group 'wasp)

(defcustom w/transcribe-process "wasp-transcribe"
  "Name of process for transcribing speech using the Whisper API."
  :type '(string)
  :group 'wasp)

(defcustom w/transcribe-buffer " *wasp-transcribe*"
  "Name of buffer used to store transcription output."
  :type '(string)
  :group 'wasp)

(defcustom w/transcribe-error-buffer " *wasp-transcribe-error*"
  "Name of buffer used to store transcription errors."
  :type '(string)
  :group 'wasp)

(defcustom w/stream-transcribe-buffer " *wasp-fake-chat-transcribe*"
  "Name of buffer used to store stream transcription output."
  :type '(string)
  :group 'wasp)

(defcustom w/stream-transcribe-error-buffer " *wasp-fake-chat-transcribe-error*"
  "Name of buffer used to store fake chat transcription errors."
  :type '(string)
  :group 'wasp)

(defvar w/current-stream-transcribe-process nil)
(defvar w/last-stream-transcription "")
(defvar w/stream-keep-transcribing t)

(defun w/tts (msg)
  "Use TTS to say MSG."
  (start-process "wasp-tts" nil "say" (w/tempfile "wasp-tts" msg)))

(defun w/play-audio (clip &optional k volume)
  "Play CLIP using mpv.
Call K when done.
If VOLUME is specified, use it to adjust the volume (100 is default)."
  (make-process
   :name w/play-audio-process
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
  (muzak-stop)
  (start-process "pkill" nil "pkill" "mpv"))

(defvar-local w/transcribe-callback nil)
(defun w/begin-transcribe (k)
  "Start recording audio to transcribe, passing the result to K."
  (let ((buf (generate-new-buffer w/transcribe-buffer)))
    (with-current-buffer buf
      (setq-local w/transcribe-callback k)
      (erase-buffer))
    (message "Transcribing...")
    (make-process
     :name w/transcribe-process
     :buffer buf
     :command (list "transcribe")
     :stderr (get-buffer-create w/transcribe-error-buffer)
     :sentinel
     (lambda (_ _)
       (with-current-buffer buf
         (funcall w/transcribe-callback (buffer-string)))))))
(defun w/end-transcribe ()
  "Finish recording transcription audio."
  (interactive)
  (message "End of transcription")
  (start-process "pkill" nil "pkill" "parecord")
  nil)

(defun w/handle-stream-transcribe ()
  "Start recording audio to transcribe."
  (unless w/current-stream-transcribe-process
    (with-current-buffer (get-buffer-create w/stream-transcribe-buffer)
      (erase-buffer))
    (setq
     w/current-stream-transcribe-process
     (make-process
      :name "fig-fake-chat-transcribe"
      :buffer (get-buffer-create w/stream-transcribe-buffer)
      :command (list "transcribe")
      :stderr (get-buffer-create w/stream-transcribe-error-buffer)
      :sentinel
      (lambda (_ _)
        (setq w/current-stream-transcribe-process nil)
        (with-current-buffer (get-buffer-create w/stream-transcribe-buffer)
          (setq w/last-stream-transcription (buffer-string)))
        (when w/stream-keep-transcribing
          (w/handle-stream-transcribe)))))))

(defun w/handle-stream-end-transcribe ()
  "Stop recording audio to transcribe."
  (when w/current-stream-transcribe-process
    (start-process "pkill" nil "pkill" "parecord")))

(defvar w/stream-transcribe-timer nil)
(defun w/run-stream-transcribe-timer ()
  "Run the fake chatter transcription timer."
  (when w/stream-transcribe-timer
    (cancel-timer w/stream-transcribe-timer))
  (w/handle-stream-end-transcribe)
  (setq
   w/stream-transcribe-timer
   (run-with-timer 10 nil #'w/run-stream-transcribe-timer)))

(defun w/start-stream-transcribe ()
  "Start transcribing speech for fake chatters."
  (interactive)
  (setq w/stream-keep-transcribing t)
  (w/handle-stream-transcribe))
(defun fig/stop-fake-chat-transcribe ()
  "Stop transcribing speech for fake chatters."
  (interactive)
  (setq w/stream-keep-transcribing nil)
  (w/handle-stream-end-transcribe))

(provide 'wasp-audio)
;;; wasp-audio.el ends here
