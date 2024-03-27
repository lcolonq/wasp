;;; bezelea-muzak.el --- Musical shiznit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 's)
(require 'dash)

(defvar muzak-music-alist '() "List of songs.")

(setq muzak-music-alist '(
                   ("At Hell's Gate" . "EEeEEdEEcEEA#EEBc EEeEEdEEcEEA#A#A#A#/")
                   ("Beethoven's 5th" . "AAAF////GGGE////")
                   ("Coffin Dance" . "D/DAG/F/E/EFG/FED/DcBcBcDDDcBcBc")
                   ("Crazy Train" . "F#F#c#F#dF#c#F#BAG#ABAG#EF#F#c#F#dF#c#F#BAG#ABAG#E")
                   ("Do Re Mi 5" . "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/G/F/GAAGFA////G/CDEFGA/A/DEFGABB")
                   ("Do Your Ears Hang Low" . "AGF/F/F/AAcdcAc/FGAAAAAAFGAGG/G/AGFFFFFFAAcdcAc/FGA///G///F")
                   ("Duvet" . "F/E/D/E/F/G//A//DDD/////C//////F/E/D/E/F/G//G//G///F///E")
                   ("Eye Of The Tiger" . "A/B/c///c/cc/B//A/G/G/A/B/A///A//B//c///c/cc/B//AG/B/A")
                   ("Fly Me To The Moon (Short)" . "c//BAG/E///EGc/B//AGF/EDE")
                   ("Fly Me To The Moon" . "c//BAG/E///EGc/B//AGF/EDE//////AGFE/D/E/F/A/G//FED/C//////DDAA///////c/B/G////////CCFF//////A/G/F//E/")
                   ("Frere Jacques (Slow)" . "C/D/E/C/C/D/E/C/E/F/G///E/F/G///GAGFE/C/GAGFE/C/G/C///C/G/C///")
                   ("Frere Jacques" . "CDECCDECEFG/EFG/")
                   ("Harry Potter" . "AcBA/ed//B//AcBG/AE///")
                   ("King of the Hill" . "C/CCCCCE/CCCCCF/FFFA/G///CCCCECCCF/E/D/DEDCCCCCECCCFFFAG//c/A/G/F/EEEFEDCcc")
                   ("Leaving on a Jet Plane" . "G///E///A/GF/G///G/E/G/F/GF/EC")
                   ("Major Scale" . "C/D/E/F/G/A/B/c/d/e/f/g/a/b/")
                   ("Mario (Fast)" . "AA/A/FA/c///C/")
                   ("Mario" . "A/A///A//F/A///c/////C/")
                   ("Megalovania" . "DDd/A//G#/G/F/DFG CCd/A//G#/G/F/DFG BBd/A//G#/G/F/DFG A#A#d/A//G#/G/F/DFG/")
                   ("Megalovania2" . "EEe/B//A#/A/G/EGA DDe/B//A#/A/G/EGA C#C#e/B//A#/A/G/EGA CCe/B//A#/A/G/EGA ")
                   ("Never Gonna Give You Up" . "CDFDA/A/G///CDFDG/G/G/F/ED///CDFD/F/G/E/DC/CC/G//F")
                   ("Pizza Tower 2" . "//A/E/G///A/E/G///ABABAGEDEE")
                   ("Pizza Tower" . "A/E/G///A/E/G///ABABAGEDE")
                   ("Santa Claus Is Coming To Town" . "EFG/G///ABc/c///EFG/G/G/AGF/F///E/G/C/E/D/F///B/c")
                   ("Saria's Song" . "FAB/FAB/FABed/BcBGE//DEGE/")
                   ("Silent Night" . "G//AG/E/////G//AG/E/////d///d/B/////c///c/G")
                   ("Smells Like Teen Spirit" . "E/EE//AAAAAG/GG//c/cccc")
                   ("Song of Healing" . "B//A//F//B//A//F//B//A//EDE/")
                   ("Song of Time" . "A/D///F/A/D///F/AcB/G/FGA/D/CED")
                   ("Star Spangled Banner" . "EDC/E/G/c//edc/E/F/G")
                   ("State Anthem of the Russian Federation" . "//G/c///G//AB///E/E/A///G//FG///C/C/D///D/E/F///F/G/A///B/c/dd")
                   ("Super Idol" . "ddd#dcA#G/cA#G/A#/c/c/dcA#cd/GGG/A#/G/ddd#dcA#dc/dG/A#A#A/AAd/d/A#/")
                   ("Take Me Out To The Ball Game" . "C/cAGEG//D//C/cAGEG")
                   ("Take Me Out" . "c/A///G/GA/////c/A///G/GA/////c/A/////G//G/A")
                   ("Take On Me" . "BBGE/E/A/A/ABBcdccGE/E/A/A/AGGAG")
                   ("Wish You Were Here" . "C/DEG/A///c/////A/c/A/G///////C/DEG/A///c//////A/c/A/G//////C/DEG/A//////A/G/E/D//////C/DEG/A//////A/G/E/D")
                   ("Zelda Secret" . "f#fdG#Gd#gb")
                   ))

(defun muzak--note-to-half-step (note)
  "Determine the step of NOTE in the C chromatic scale."
  (when-let* ((step (-elem-index (upcase note) '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))))
    (if (s-uppercase? note)
        (- step 9)
      (+ step 3)))) ; C is the 3rd semitone above A

(defun muzak--note-to-freq (note)
  "Calculate the frequency of NOTE."
  (if-let ((step (muzak--note-to-half-step note)))
      (round (* 440 (expt 2 (/ step 12.0))))
    0))

(defun muzak--parse-notes (note-string)
  "Parse notes from NOTE-STRING."
  (append (mapcar #'car (s-match-strings-all "/\\|[A-Za-z]#?" note-string))
          '("/")))

(defun muzak--get-notes (song-name)
  "Look up notes of SONG-NAME from `muzak-music-alist'."
  (muzak--parse-notes
        (alist-get song-name muzak-music-alist nil nil #'cl-equalp)))

(defun muzak-add-song (name notes &optional author)
  "Add a song to `muzak-music-alist'.
NAME specifies the name of the song.
NOTES is a string of notes and rests.
AUTHOR is the name of whoever the transcription can be attributed to."
  (when author (message "%s added %s to the muzak list." author name))
  (add-to-list 'muzak-music-alist '(name . notes)))

(defun muzak-stop ()
  "Fuck it, we ball."
  (interactive)
  (call-process-shell-command "killall ffplay"))

(defun muzak-play-notes (notes &optional duration)
  "Play notes.
NOTES should be a string or sequence of notes and rests. Each note is notated as
letters A-G in the chromatic scale, optionally followed by a # to denote a
sharp. Forward slashes are interpreted as rests. Uppercase letters are used for
the lower octave and lowercase letters are the higher octave.
DURATION is the length of each note in seconds."
  (if (stringp notes)
      (muzak-play-notes (muzak--parse-notes notes) duration)
    (call-process-shell-command
     (format "
for FREQ in %s; do
    ffmpeg -strict experimental -loglevel quiet -f lavfi -i \"sine=frequency=${FREQ}:duration=%f\" -f oga -filter tremolo -filter aphaser=in_gain=0.4:out_gain=0.74:delay=0.1:decay=0.2:speed=0.2 -filter volume=1.5 - 2>/dev/null
done | ffplay -loglevel quiet -autoexit -nodisp - &"
             (mapconcat (lambda (note)(format "%d" (muzak--note-to-freq note))) notes " ")
             (or duration 0.2)))))

(defun muzak-play-song (song-name)
  "Play SONG-NAME from `muzak-music-alist'."
  (interactive "sSong Name: ")
  (message "Playing %s" song-name)
  (when-let (song-data (muzak--get-notes song-name))
    (muzak-play-notes song-data)))

;(muzak-play-notes (-flatten (cl-map 'list (lambda (x) (append (muzak--get-notes x) (-repeat 3 "/"))) '("Zelda Secret" "Song of Healing" "Saria's Song" "Song of Time"))) 0.15)

(provide 'bezelea-muzak)
;;; bezelea-muzak.el ends here
