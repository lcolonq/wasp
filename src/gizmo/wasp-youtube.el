;;; wasp-youtube --- YouTube integration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 's)
(require 'dash)
(require 'ht)
(require 'wasp-utils)
(require 'selector)

(defcustom w/youtube-mpv-process "wasp-mpv-search"
  "Name of process running MPV player for YouTube videos."
  :type '(string)
  :group 'wasp)

(defcustom w/youtube-search-process "wasp-youtube-search"
  "Name of process running YouTube search."
  :type '(string)
  :group 'wasp)

(defcustom w/youtube-search-buffer " *wasp-youtube-search*"
  "Name of buffer used to store YouTube search results."
  :type '(string)
  :group 'wasp)

(defun w/youtube-search (query k)
  "Search YouTube for QUERY and pass the resulting list of title-URL pairs to K."
  (let ((buf (generate-new-buffer w/youtube-search-buffer)))
    (make-process
     :name w/youtube-search-process
     :buffer buf
     :command
     (list
      "yt-dlp" "--dump-json" (format "ytsearch20:%s" query)
      "--skip-download" "--no-playlist" "--default-search" "ytsearch"
      "--no-check-certificate" "--geo-bypass" "--flat-playlist"
      "--quiet" "--ignore-errors")
     :sentinel
     (lambda (_ _)
       (with-current-buffer buf
         (funcall
          k
          (->>
           (buffer-string)
           (s-lines)
           (-map #'s-trim)
           (-filter #'s-present?)
           (-map #'json-parse-string)
           (--map (cons (ht-get it "title") (ht-get it "url"))))))))))

(defun w/youtube-play (url)
  "Play the YouTube video at URL."
  (message (format "Opening video at %s" url))
  (make-process
   :name w/youtube-mpv-process
   :command (list "mpv" "--force-window=yes" url)))

(defun w/youtube ()
  "Interactively search for and play a YouTube video."
  (interactive)
  (let ((query (read-string "YouTube: ")))
    (w/youtube-search
     query
     (lambda (results)
       (selector
        (list
         (selector-source-create
          (format "YouTube search: %s" query)
          :candidates
          (--map (selector-candidate-create (car it) :value (cdr it)) results)
          :actions
          '(w/youtube-play))))))))

(provide 'wasp-youtube)
;;; wasp-youtube.el ends here
