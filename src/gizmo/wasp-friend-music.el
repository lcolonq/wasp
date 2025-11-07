;;; wasp-friend-music --- "friend" can play music -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'wasp-friend)

(defconst w/friend-composition-examples
  '(("My Life Is Like A Video Game" . "A/A/c/c/c/dcc/c///a/a/a/f/g/f/f///a/a/a/a/g/g/ga//f//")
    ("Super Idol" . "gg[g#]gfg[CD#cG#][D#][CG#f][Cd#][Cc]C[Cd#]/[DFfd][FA#][DA#f]D[Dg][A#f][Dd#a#]f[GBgd]B[Gd#][GDc][Gd#]G[Gd#]/[D#Gc]G[D#cg][D#g][D#g#][dg][D#f][d#d#][D#Ggc]f[D#][D#Gg][D#c][D#][D#c][d#][DFdA#]F[DA#d][Dd][Dg]/[Da#g]/[D#d#][D#][D#][D#][D#][FD#][GA#][fd#][gA#]")
    ("Reindeer" . "FG/FD/B/A/G/////GAGAG/c/B///////FG/FD/B/A/G/////GAGAG/d/c/////|C4~~~G3~~~C4~~~G3~~~C~~~E3~D#3~D3~~~~~~~G3~~~D3~~~G3~~~D3~~~G3~~D3G3~B3/C4")))

(defun w/friend-compose-song (theme)
  "Compose a song about THEME to play on the bells."
  (w/ai
   theme
   (lambda (res)
     (let* ((sp (s-split ":" (s-trim res)))
            (name (s-trim (car sp)))
            (song (s-trim (cadr sp))))
       (when (and (stringp name) (stringp song))
         (w/friend-respond
          (format "You just composed a song about %s called %s! Say something about it!" theme name)
          (lambda ()
            (w/chat-write-event (format "The song is called %s: %s" name song))
            (w/add-song (s-concat "friend's " name) song)
            (w/audio-muzak-enqueue "\"friend\"" song))))))
   "Please compose a song about the provided theme. The format for the song is a sequence of characters with meanings as follows: / represents a rest, uppercase letters A through G indicate semitones, octaves are specified with a number following a semitone, ~ extends the duration of a note, square brackets like [] group notes together into a chord. The pipe character | separates tracks. Respond only with the song's name followed by a colon folowed by the song notes. Do not explain yourself. The song should ideally be 20 to 30 notes long."
   (-map #'car w/friend-composition-examples)
   (--map (format "%s: %s" (car it) (cdr it)) w/friend-composition-examples)))

(provide 'wasp-friend-music)
;;; wasp-friend-music.el ends here
