;;; bezelea-muzak.el --- Musical shiznit -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;    A simple text notation player and music library.
;;    Examples can be found in `muzak//song-table'.
;;    For more information, see https://pub.colonq.computer/~bezelea/bells/
;;
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 's)
(require 'wasp-db)

(defconst muzak//min-bpm  50 "Minimum beats per minute.")
(defconst muzak//max-bpm 200 "Maximum beats per minute.")
(defconst muzak//max-length 400 "Maximum number of notes in a song.")
(defconst muzak//max-duration 60 "Maximum song length in seconds.")
(defconst muzak//middle-octave 4 "Middle octave. Used as default.")
(defconst muzak//process-name "muzak-ffplay" "Name for FFplay processes.")
(defvar muzak//process nil "FFplay process for muzak.")
(defvar muzak//song-queue nil "Queued audio sources.")
(defvar muzak/note-duration 0.2 "Duration of each note in seconds.")
(defvar muzak/volume 0.4 "Amplitude used in FFplay invocations.")
(defvar muzak/instrument 'beep "Lead instrument.")

(defconst muzak//note-string-regexp
  (rx
   (submatch (or "/" (any "A-G")) (? "#"))
   (submatch (? digit))
   (submatch (0+ "~"))))

(defconst muzak//notes-string-regexp
  (rx (or (group "[" (* (not "]")) "]")
          (group (or "/" (: (any "A-G") (? "#") (? digit) (0+ "~")))))))

(defconst muzak//waveforms
  (list (cons 'sine         (lambda (f) (format "sin(t*%.2f)" (* 2 pi f))))
        (cons 'square       (lambda (f) (format "ceil(sin(t*%.2f))" (* 2 pi f))))
        (cons 'triangle     (lambda (f) (format "asin(sin(t*%.2f))" (* 2 pi f))))
        (cons 'sawtooth     (lambda (f) (format "(atan(tan(t*%.2f))/%.2f)" (* 2 pi f) (/ pi 2))))
        (cons 'sine-octaver (lambda (f) (format "(sin(t*%.2f)+sin(t*%.2f))" (* 2 pi f) (* pi f)))))
  "List of formatting functions for generating FFmpeg aevalsrc strings.")

(defconst muzak//waveform-effects
  (list (cons 'dampen     (lambda (s d) (format "pow(2.72,-10*%.1f*(t-%.1f))" muzak/note-duration s)))
        (cons 'swell      (lambda (s d) (format "((t-%.1f)/%.1f)" s (+ d))))
        (cons 'linear     (lambda (s d) (format "(1-(t-%.1f)*%.1f)" s (/ 1.0 (* 2 d)))))
        (cons 'noise      (lambda (s d) (format "(sin(10*random(0)))")))
        (cons 'rotary     (lambda (s d) (format "abs(1-mod(t,2))")))
        (cons 'rotaryslow (lambda (s d) (format "sin(%.2f*t)" (* 0.3 pi))))
        (cons 'rotaryfast (lambda (s d) (format "sin(%.2f*t)" (* 10 pi))))
        (cons 'rotarywtf  (lambda (s d) (format "(sin(2*PI*178.75*t)+sin(2*PI*181.25*t))")))
        (cons 'horror     (lambda (s d) (format "sin(2*PI*(360+2.5/2)*t)")))
        (cons 'aliens     (lambda (s d) (format "(0.8*mod(t*100,2))"))))
  "List of formatting functions for generating FFmpeg aevalsrc strings.")

(cl-defstruct (muzak/instrument (:constructor muzak/make-instrument))
  "Instrument definition."
  (waveform 'sine)
  (effects '(dampen))
  (sustain 0))

(defconst muzak//instruments
  (list
   (cons 'beep (muzak/make-instrument :waveform 'sine :effects nil))
   (cons 'bells (muzak/make-instrument :waveform 'square :effects '(dampen) :sustain 4))
   (cons 'keyboard (muzak/make-instrument :waveform 'square :effects '(linear)))
   (cons 'waterphone (muzak/make-instrument :waveform 'triangle :effects '(swell)))
   (cons 'test (muzak/make-instrument :waveform 'sine :effects '(aliens) :sustain 0)))
  "List of instruments.")

(cl-defstruct (muzak/note (:constructor muzak/make-note))
  "Representation of a musical note.

Each note's SYMBOL is notated as letters A-G in the chromatic scale, optionally
followed by a '#' to denote a sharp.
Forward slashes are interpreted as rests.
Uppercase and lowercase letters can be used in lieu of an OCTAVE for the middle
and higher octaves, respectively.

OCTAVE can be any number.

LENGTH determines the duration of the note when multiplied by `muzak/note-duration'.
"
  symbol
  (octave muzak//middle-octave)
  (length 1))

(defconst muzak//chromatic-scale
  (list "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
  "Chromatic scale.")

(defconst muzak//degrees
  `((major      . (0 2 4 5 7 9 11 12)) ; Ionian mode
    (minor      . (0 2 3 5 7 8 10 12)) ; Aeolian mode
    (dorian     . (0 2 3 5 7 9 10 12))
    (phrygian   . (0 1 3 5 7 8 10 12))
    (lydian     . (0 2 4 6 7 9 11 12))
    (mixolydian . (0 2 4 5 7 9 10 12))
    (locrian    . (0 1 3 5 6 8 10 12))
    (chromatic  . (0 1 2 3 4 5 6 7 8 9 10 11 12))
    (pentatonic . (0 3 5 7 10 12)))
  "Intervals of degrees within various scales.")

(defconst muzak//qualities
  `((major    . (0 4 7))    ; Root, major 3rd, and perfect 5th
    (minor    . (0 3 7))    ; Root, minor 3rd, and perfect 5th
    (maj7     . (0 4 7 11)) ; Major 3rd and 7th
    (min7     . (0 3 7 10)) ; Minor 3rd and 7th
    (dom7     . (0 4 7 10)) ; Major 3rd and minor 7th
    (majmin7  . (0 4 7 10)) ; Major 3rd and minor 7th (alias: dom7)
    (minmaj7  . (0 3 7 11)) ; Minor 3rd and major 7th
    (dim      . (0 3 6))    ; Minor 3rd, diminished 5th
    (dim7     . (0 3 6 9))  ; Minor 3rd, diminished 5th, and diminished 7th
    (halfdim7 . (0 3 6 10)) ; Minor 3rd, diminished 5th, and minor 7th
    (aug      . (0 4 8))    ; Major 3rd and augmented 5th
    (aug7     . (0 4 8 11)) ; The MrBeast of music?!
    (sus2     . (0 2 7))    ; Major 2nd and perfect 5th
    (sus4     . (0 5 7))    ; Perfect 4th and perfect 5th
    (power    . (0 7 12)))  ; Rock n' Roll
  "Intervals describing various chord qualities.")

(defvar muzak//song-table
  (ht
   ("All I Want For Christmas Is You"         "gbd/f#g/f#/e/d//a/g/gf#/g/f#/d//dc//e/g/ab/a/g/e//c/d#/g/aa#/a/d#//a/g/f#f#/g/f#/e/d//G/B/df#/g/f#/e/d//bbba/b/a/g/e//c/d#/gaga#/a/fd#//ga/f#/g/e/f#/d#///ga/f#/g/e/f#/d#//D/E/G/d/c/dc//b/a/g/e/d#/a///b/ag//B///dB/B//A")
   ("Among Us"                                "C/D#/F/F#/F/D#/C/A#/D/C//C/D#/F/F#/F/D#/F#/F#/F/D#/F#/F/D#/C//") ;DocMaho
   ("At Hell's Gate"                          "|EEeEEdEEcEEA#EEBc EEeEEdEEcEE[EA#~~~]/")
   ("Bad Apple"                               "DEFGA/dcA/D/AGFEDEFGA/GFEDEFEDCE DEFGA/dcA/D/AGFEDEFGA/GFE/F/G/A")
   ("Beethoven's 5th"                         "GGGD#////FFFD////")
   ("Billy Jean"                              "F#F#E/C#/F#F#/E/C#///F#F#E/C#/F#A/B/AG#F#////F#/F#c#/B/F#D/C#//")
   ("Butterfly"                               "GGGA#c/GA#cd#cA#G///F/FGA#/GD#FGFD#C")
   ("Canon in D"                              "dc#dDC#AEF#Ddc#Bc#f#abgf#egf#edc#BAGF#EGF#ED")
   ("Coffin Dance"                            "D/DAG/F/E/EFG/FED/DcBcBcDDDcBcBc")
   ("Crazy Train"                             "F#F#c#F#dF#c#F#BAG#ABAG#EF#F#c#F#dF#c#F#BAG#ABAG#E")
   ("Cruel Angel's Thesis"                    "C~~~D#~~~F~~D#~~F~F~F~A#~G#~GF~G~~~/G~~~A#~~~c~~F~~D#~A#~A#~G~A#~A#~~c~~~~~~")
   ("Deck The Halls"                          "c//A#A/G/F/G/A/F/GAA#GA//GF/E/F/")
   ("Do Re Mi"                                "C//DE/C/E/C/E//D//EFFEDF//E//FG/E/G/E/G/G/F/GAAGFA////G/CDEFGA/A/DEFGABB")
   ("Do Your Ears Hang Low"                   "AGF/F/F/AAcdcAc/FGAAAAAAFGAGG/G/AGFFFFFFAAcdcAc/FGA///G///F")
   ("Duvet"                                   "F/E/D/E/F/G//A//DDD/////C//////F/E/D/E/F/G//G//G///F///E")
   ("Eye Of The Tiger"                        "A/B/c///c/cc/B//A/G/G/A/B/A///A//B//c///c/cc/B//AG/B/A")
   ("Fly Me To The Moon"                      "c~~BAG~E~~/EGc~B~~AGF/EDE~/////AGFE/D/E/F/A/G~~FED/C~~////DDAA~~~////c~B~G~~~/////CCFF~~~///A~G~F~~E~~")
   ("Frere Jacques"                           "C/D/E/C/C/D/E/C/E/F/G///E/F/G///GAGFE/C/GAGFE/C/C/G3/C///C/G3/C///")
   ("Geiser's Tune"                           "c#Ag#A~c#g#Ac#G#g#G#~c#g#ec#F#g#F#~c#g#F#c#bag#f#~~~e~d#~B~~~c#bag#f#~~~e~d#~B~~|C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#C#EC#C#C#C#C#EC#C#C#C#C#C#C#C#C#EC#C#C#C#C#EC#C#C#|AEAEEAEEG#D#G#D#D#G#D#G#F#EF#EEF#EEEG#G#G#G#G#EG#G#G#G#G#EG#G#EEAAAAAEAAAAAE")
   ("God Save The King"                       "G/G/A/F#//GA/B/B/c/B//AG/A/G/F#/G///////////")
   ("Good King Wenceslas"                     "FFFGFFC/DCDEF/F/FFFGFFC/DCDEF/F/cA#AGAGF/DCDEF/F/CCDEFFG/cA#AGF/A#/FCFCF/F/|F2~C3~F2~C3~A#2~C3~F2~~~F2~C3~F2~C3~A#2~C3~F2~~~F3~C#3~D3~~~A#2~C3~F2~~~F2~E2~D2~A#2~C3~~~D3~A#2~F2F2F2~F2~F2~")
   ("Gremlins Theme"                          "d#d#d#d#/d#d/d#d#d#d#/d#d/c/cc/cdccB/B////")
   ("Hacking To The Gate"                     "AG/F//D/A#A/G/FGGE//CC/A#A/EGGF////AGF/DD//DAG/AE///CCA/AG/C/D///")
   ("Harry Potter"                            "AcBA~ed~~B~~AcBG~AE~~//")
   ("Hedwig's Theme"                          "EGF#E/BA//F#//EGF#D#/FB3///")
   ("Hino Nacional Brasileiro"                "C/FEFGAGAA#B//cF/C/FEGFAGA#AF#/G///D/GF#GAA#AA#cc#//dG//CGF#AGA#AcA#G/A/")
   ("Jingle Bells"                            "EEE/EEE/EGCDE///FFFFFEEEEDDED/G/EEE/EEE/EGCDE///FFFFFEEEGGEDC")
   ("King of the Hill"                        "C/CCCCCE/CCCCCF/FFFA/G///CCCCECCCF/E/D/DEDCCCCCECCCFFFAG//c/A/G/F/EEEFEDCcc")
   ("Leaving on a Jet Plane"                  "G///E///A/GF/G///G/E/G/F/GF/EC")
   ("Littleroot Town"                         "60 CFGA//GAGAA#/c/dA/Ac#d/e/d/AGFEFAd/DEF///cA#A#AF///dAAGF/////EDE//FG~|///F2C3A3C3F2C3A3C3 A2G3CG3A2G3C#G3 D3F3DF3C#3F3C#F3 C3F3CF3B2F3B3F3 A#2F3A#3F3A#2F3A#3F3 G2F3A#3F3G2F3A#3F3 C3G3A#3G3C3G3A#3G3 C3E3A#3E3C")
   ("Major Scale"                             "C0D0E0F0G0A0B0C1D1E1F1G1A1B1C2D2E2F2G2A2B2C3D3E3F3G3A3B3C4D4E4F4G4A4B4C5D5E5F5G5A5B5C6D6E6F6G6A6B6C7D7E7F7G7A7B7C8D8E8F8G8A8B8C9D9E9F9G9A9B9")
   ("Mario"                                   "AA/A/FA/c///C/")
   ("Megalovania"                             "120DDd/A//G#/G/F/DFG CCd/A//G#/G/F/DFG B3B3d/A//G#/G/F/DFG A#3A#3d/A//G#/G/F/DFG/")
   ("Money"                                   "B~bf#B~F#~A~B~d~B~")
   ("My Heart Will Go On"                     "F~~~~~~~G~~~~~C~c~~~A#~AG~~~~A~A#A~~~~G~FE~F~~E~~D~~~~~C~~~~~")
   ("My Life Is Like A Video Game"            "A/A/c/c/c/dcc/c///a/a/a/f/g/f/f///a/a/a/a/g/g/ga//f//")
   ("Never Gonna Give You Up"                 "CDFDA//A//G/////CDFDG//G//F/ED//CDFDF//G//E/DC///C/G//F//")
   ("Pizza Tower"                             "A/E/G///A/E/G///ABABAGEDEE")
   ("Rudolph The Red-Nosed Reindeer"          "FG/FD/B/A/G/////GAGAG/c/B///////FG/FD/B/A/G/////GAGAG/d/c/////|C4~~~G3~~~C4~~~G3~~~C~~~E3~D#3~D3~~~~~~~G3~~~D3~~~G3~~~D3~~~G3~~D3G3~B3/C4")
   ("Santa Claus Is Coming To Town"           "EFG/G///ABc/c///EFG/G/G/AGF/F///E/G/C/E/D/F///B/c")
   ("Saria's Song"                            "FAB/FAB/FABed/BcBGE//DEGE/")
   ("Silent Night"                            "G//AG/E/////G//AG/E/////d///d/B/////c///c/G")
   ("Smells Like Teen Spirit"                 "E~EE//AAAAAG~GG//c/cccc")
   ("Song of Healing"                         "B/A/F/B/A/F/B/A/EDE/")
   ("Song of Time"                            "A/D///F/A/D///F/AcB/G/FGA/D/CED")
   ("Star Spangled Banner"                    "dBG/B/d/g///bag/B/c#/d///ddb//ag/f#///ef#g/g/d/B/G///////")
   ("State Anthem of the Russian Federation"  "G/c///G//AB///E/E/A///G//FG///C/C/D///D/E/F///F/G/A///B/c/d~")
   ("Super Idol"                              "ddd#dcA#G/cA#G/A#/c/c/dcA#cd/GGG/A#/G/ddd#dcA#dc/dG/A#A#A/AAd/d/A#/")
   ("Super Idol Good" "gg[g#]gfg[CD#cG#][D#][CG#f][Cd#][Cc]C[Cd#]/[DFfd][FA#][DA#f]D[Dg][A#f][Dd#a#]f[GBgd]B[Gd#][GDc][Gd#]G[Gd#]/[D#Gc]G[D#cg][D#g][D#g#][dg][D#f][d#d#][D#Ggc]f[D#][D#Gg][D#c][D#][D#c][d#][DFdA#]F[DA#d][Dd][Dg]/[Da#g]/[D#d#][D#][D#][D#][D#][FD#][GA#][fd#][gA#]")
   ("Sweet Child O' Mine"                     "DdAGgAf#A DdAGgAf#A EdAGgAf#A EdAGgAf#A GdAGgAf#A GdAGgAf#A DdAGgAf#A DdAGgAf#A D")
   ("Take Me Home, Country Roads"             "ABc#/////c#AB/////c#BA/////c#ef#/////f#f#ec#/////c#ABc#////c#BA////ABA")
   ("Take Me Out To The Ball Game"            "C/cAGEG//D//C/cAGEG")
   ("Take Me Out"                             "c/A///G/GA/////c/A///G/GA/////c/A/////G//G/A")
   ("Take On Me"                              "BBGE/E/A/A/ABBcdccGE/E/A/A/AGGAG")
   ("The Entertainer"                         "gafe/gd/cdBA/cG/GAFE/GD///CEDG/CE/DG//EDGC/ED/CC//CEDG/CE/DG///DCDEGA/EDGAAA///CEDG/CE/DG//EDGC/ED/CC///EDEG/EDGEDEC/EDGEDEC/ED/CC")
   ("The Pretender"                           "AAA/A/AA/A/AAG//AAA/A/AA/A/AAGA///A/A///AAAA/G//AGA/A/AA/A/AAG/")
   ("Tubular Bells"                           (s-join "|" (list "/E5A5E5B5E5G5A5E5c6E5d6E5B5c6 E5A5E5B5E5G5A5E5c6E5d6E5B5c6 E5B5 E5A5E5B5E5G5A5E5c6E5d6E5B5c6 E5A5E5B5E5G5A5E5c6E5d6E5B5c6"
                                                                "[Bd~][Ac~][Bd~][GB][Ac~][ce~][df~][Bd][ce~] [Ac~][Bd~][GB][Ac~][ce~][df~][Bd][ce~][Bd~] [Ac~][Bd~][GB][Ac~][ce~][df~][Bd][ce~][Ac~][Bd~] [GB][Ac~][ce~][df~][Bd][ce~]")))
   ("Thomas The Tank Engine"                  "GABc/de/G#//////AF/AG//G#AF/AGF#GF#G//G//////")
   ("Westminster Quarters - First Quarter"    "G#4/F#4/E4/B3")
   ("Westminster Quarters - Second Quarter"   "E4/G#4/F#4/B3////E4/F#4/G#4/E4////")
   ("Westminster Quarters - Third Quarter"    "G#4/E4/F#4/B3////B3/F#4/G#4/E4////G#4/F#4/E4/B3////")
   ("Westminster Quarters - Full Hour"        "E4/G#4/F#4/B3////E4/F#4/G#4/E4////G#4/E4/F#4/B3////B3/F#4/G#4/E4////")
   ("Wish You Were Here"                      "C/DEG/A///c/////A/c/A/G///////C/DEG/A///c//////A/c/A/G//////C/DEG/A//////A/G/E/D//////C/DEG/A//////A/G/E/D")
   ("You're A Mean One, Mr. Grinch"           "FGAD////FAG/////DA//AB//Bc#////AAdcA#AA#GGGcA#AGA/FED///c#Bc#Bc#Bc#B///Ac#Bc#/d//")
   ("Zelda Secret"                            "110 f#fdG#Gd#gb~")
   ("Ameno" "[f3a3][g3A3~]/////[f3a3][g3A3~~][f3a3][g3A3~~][f3a3][g3A3~]/////[c4a3][g3A3][g3A3]/[c4a3][g3A3~]/[g3A3][f3a3~]/////[g3A3][f3a3~]/[f3a3]///[f3a3][g3A3~]/////////[c5c4a4a3][d5d4A4A3~]/////[c5c4a4a3][d5d4A4A3~~][c5c4a4a3][d5d4A4A3~~][c5c4a4a3][d5d4A4A3][d5d4A4A3]/////[c5c4a4a3][A4g4g3A3~]/[c5c4a4a3][A4g4g3A3~]/[A4g4g3A3][a4f4f3a3~]/////[A4g4g3A3][a4f4f3a3~]/[a4f4f3a3]///[a4f4f3a3][A4g4g3A3~]/////////c4d4~~c4d4~~~D4D4~/g4d4D4c4d4~~c4d4~d4~D4~~/[g5g4][d5d4][D5d4]/////[g5g3][d5d3][D5D3][g5g3]/[g5g3]//////[f5f3~~][D5D3]/[g5g3]/[g5g3]////////[f5f4f3][f5f4][f5f4f3][D5D4D3]/[A5A4A3]/[A5A4A3]////[g5g3][d5d3][D5D3][g5g3]////[g5g3][d5d3][D5D3][g5g3]/[g5g3]//////[f5f3~~][D5D3]/[g5g3]/[g5g3]////////[f5f4f3][f5f4f3~][D5D4D3]/[A5A4A3]/[A5A4A3]///[G5G3][g5g3][f5f3][D5D3][g5g3]/////////////////////c5d5~d5c5d5~~~D5~~/g5d5D5[g5g3]/[g5g3]//////[f5f3~~][D5D3]/[g5g3]/[g5g3]////////[f5f4f3][f5f4][f5f4f3][D5D4D3]/[A5A4A3]/[A5A4A3]////[g5g3][d5d3][D5D3][g5g3]////[g5g3][d5d3][D5D3][g5g3]/[g5g3]//////[f5f3~~][D5D3]/[g5g3]/[g5g3]////////[f5f4f3~~][D5D4D3]/[A5A4A3]/[A5A4A3]///[G5G3][g5g3][f5f3][D5D3][g5g3]////////c5d5~~c5d5~d5d5D5~~/[g5g3][d5d3][D5D3]////g5////////////g5d5D5/////////////gdD///////")
  ("SICP" "eABc#eddf#eeag#aec#ABc#def#edc#Bc#AG#AB/G#Bdc#B|A~~c#~~B~~c#~~f#~~c#~~B~~c#~~d~~e~~G#~~E~~")
  ("Friend's First Song" "E4E4D4C4/G3G3G3~|A3~~A3C4E4~~~~|E4E4D4C4/G3G3G3~|A3~~A3C4E4~~~~|C4C4C4G3/A3A3A3~|F3~~F3A3C4~~~~|G3F3E3D3/C3C3C3~|G3~~G3E3C3~~~~|")
  ("forsen's theme" "C/C/DEFGAGF/G/DC///C/C/DEFGAGFG/F/EDC///|A3~~~E4~~~A4~~~E4~~~C4~~~E4~D#4~D4~~~~~~~A3~~~E4~~~A4~~~E4~~~A3~~E4A4~E5/C5~")
  ("blank" "157f~~g~~~~fg~~~~d#f~~d#~~d//f~~g~~~~a#g~~f//g~~//////d#~~f~~~~d#d~~~~d#d~~A#~~G~~A#~~c~~d~d#d//A#~~G~~//////A#~~c~~~~d#d~~~~gf~~d#~~d~~a#~~g~~~~~~~~d#~~f~/f~d#f~~g~~d#~~///G~~A#~~c~~~d~~~d#~~~f~~~~~~~~g~~g#~~~~~~~~|///[G#D#~~~]/[G#D#~~~~~~][BD~]/[BD~~~~~~~~][GD#~~~]/[GD#~~~~~~][FC~]/[FC~]///////[G#D#~~~]/[G#D#~~~~~~][FD~]/[FD~~~~~~~~][D#C~~~]/[D#C~~~~~~][ED~]/[ED~]///////[G#D#~~~]/[G#D#~~~~~~][BD~]/[BD~~][A#F~~~~~][GD#~~~]/[GD#~~~~~~][FE~]/[FE~~~~]/[FE~~][D#C~~~~~~~~~~]/[D#C~~]/[D#C~~]/[D#C~~]A#3[A#3C][A#3CD#][A#3CD#A#~~~~~~~~]G#3[G#3C][G#3CD#][G#3CD#G#~~~~~~~~]")
  ("Rainy Capsule Paraphrase" "140[B3B2]F#Ac#[C#6ad~]F#[dc#F#B3]/[adc#FB3][adc#FB3][adc#FB3]F#3[dcAD#F3][A#3f][f#BG#E3~][BB3][f#D][gD#][F6D6g#BE][C#6C#][F#6bd][F6aE][E6f#]D6[f#B3F#3][dBG#F#E3]/[c#AE][ad][eF#3]/G3[f#BD][fA]e[f#c#G][D6adA3][C#6a#d#G3][bec#F#3]f#f[edBGC~][f#G3][gd#BAF#3][aF3][dc#G#E3][dc#G#E3g]F#6[E6c#GE3][bd][BF#E3][dAF3][eBF#3][f#c#][A#C#][D6D#A3][C#6D#G3][bA#GE][aF#3]c[c#A]|[bB~]//[B6b~]//[bB~]/[bB~~]/[bB~][bB][aA][aA][bB][c#C#][dD][f#F#][C#7C#6~][D7D6~][C#7C#6][B6b][A6a]///[bB]/[bB][aA][bB][aA][bB]/[F#6f#]/[E6e]//[bB][D6d]/[bB][C#6c#][D6d]/[bB][C#6c#~~~~]")
  ("friend's alien song" "E3/G3/B3/D4/C4///A3/C4/E4/G4/F4///E3/G3/B3/D4/C4///G3/B3/D4/F4/E4///|C4~~~~/C4G3E3////B3~~~~/B3G3E3////A3~~~~/A3F3D3////G3~~~~/G3E3C3///")
  )
  "List of songs.")

;;; These functions assume that our piano's keyboard begins at C for convenience.
;;; This is typical for 61-key pianos.
;;; NOTE: A "piano key" here is different from a "key", or a musical scale.

(defun muzak/piano-key-color (piano-key)
  "Determine the color of KEY on a piano keyboard."
  (if (-contains? (alist-get 'major muzak//degrees) (mod piano-key 12))
      'white
    'black))

(defun muzak/piano-key-to-note (piano-key &optional l)
  "Construct a note based on the position of KEY on a piano."
  (let ((symbol (nth (mod (1- piano-key) 12) muzak//chromatic-scale))
        (octave (+ 2 (/ (1- piano-key) 12))))
    (muzak/make-note :symbol symbol :octave octave :length l)))

(defun muzak/piano-key-61-to-88 (piano-key)
  "Map a key number from a 61-key piano to an 88-key piano."
  (when piano-key
    (+ piano-key 15)))

(defun muzak/piano-key-to-midi (piano-key)
  "Map PIANO-KEY to a MIDI note number."
  (when-let ((piano-key (muzak/piano-key-61-to-88 piano-key)))
    (+ piano-key 20)))

(defun muzak/midi-to-piano-key (midi-note)
  "Map MIDI-NOTE to a piano key."
  (when midi-note
    (- midi-note 35)))

(defun muzak/midi-to-freq (midi-note)
  "Calculate the frequency of MIDI-NOTE."
  (* 440 (expt 2 (/ (- midi-note 69) 12.0))))

(defun muzak/midi-to-note (midi-note &optional l)
  "Construct a note that represents MIDI-NOTE."
  (muzak/piano-key-to-note
   (muzak/midi-to-piano-key midi-note) l))

(defun muzak/note-to-midi (note)
  "Get the MIDI code that represents NOTE."
  (muzak/piano-key-to-midi
   (muzak/note-to-piano-key note)))

(defun muzak/note-to-piano-key (note)
  "Determine an appropriate piano key for NOTE."
  (when (not (muzak/rest-p note))
    (+ 1
       (muzak//note-to-half-step note)
       (-elem-index "A" muzak//chromatic-scale)
       (muzak/octaves (- muzak//middle-octave 2)))))

(defun muzak//note-to-half-step (note)
  "Determine the step of NOTE relative to middle A."
  (when-let* ((symbol (muzak/note-symbol note))
              (index (-elem-index (s-upcase symbol) muzak//chromatic-scale))
              (octave (or (muzak/note-octave note) muzak//middle-octave)))
    (+ -9
       index
       (muzak/octaves (- muzak//middle-octave))
       (muzak/octaves octave))))

(defun muzak/note-to-freq (note)
  "Calculate the frequency of NOTE."
  (if (muzak/rest-p note)
      0
    (let ((step (muzak//note-to-half-step note)))
      (* 440 (expt 2 (/ step 12.0)))))) ; (min 4187 ...)

(defun muzak/note-same-p (n1 n2)
  "Is N1 equal to N2?

This does not compare octaves or durations."
  (let ((s1 (if (stringp n1) n1 (muzak/note-symbol n1)))
        (s2 (if (stringp n1) n2 (muzak/note-symbol n2))))
    (cl-equalp s1 s2)))

(defun muzak/note-equals-p (n1 n2)
  "Is N1 equal to N2?

This does not compare durations."
  (and (muzak/note-same-p n1 n2)
       (= (or (muzak/note-octave n1) 0)
          (or (muzak/note-octave n2) 0))))

(defun muzak/note-compare (n1 n2)
  "Compare notes N1 and N2."
  (let ((k1 (muzak/note-to-piano-key n1))
        (k2 (muzak/note-to-piano-key n2)))
    (cond ((> k1 k2)  1)
          ((= k1 k2)  0)
          ((< k1 k2) -1))))

(defun muzak/note-add (note &optional n)
  "Increment NOTE by N half steps."
  (if (and (muzak/note-p note) (not (muzak/rest-p note)))
      (let* ((n (or n 1))
             (piano-key (+ n (muzak/note-to-piano-key note)))
             (piano-note (muzak/piano-key-to-note piano-key)))
        (muzak/make-note
         :symbol (muzak/note-symbol piano-note)
         :octave (muzak/note-octave piano-note)
         :length (muzak/note-length note)))
    note))

(defun muzak/note-sub (note &optional n)
  "Decrement NOTE by N half steps."
  (muzak/note-add note (- (or n 1))))

(defun muzak//notes-add (notes n)
  "Shift NOTES up or down by N half steps."
  (--tree-map (muzak/note-add it n) notes))

;; (defun muzak/note-p (note)
;;   "Is NOTE a note?"
;;   (cl-typep note 'muzak/note))

(defun muzak/rest-p (note)
  "Is NOTE a rest?"
  (s-equals?
   "/"
   (cond ((stringp note) note)
         ((muzak/note-p note) (muzak/note-symbol note))
         ((null note) "/"))))

(defun muzak/make-scale (tonic &optional mode)
  "Make a list of note names of scale MODE where the root is TONIC."
  (let ((tonic (if (stringp tonic)
                   (muzak/make-note :symbol tonic)
                 tonic)))
    (--map
     (muzak/note-symbol (muzak/note-add tonic it))
     (alist-get (or mode 'major) muzak//degrees))))

(defun muzak/nth-degree (scale degree)
  "Get the note name at DEGREE in SCALE."
  (nth (mod (1- degree) (1- (length scale))) scale))

(defun muzak/scale-contains-p (scale note)
  "Determine if NOTE is contained within SCALE."
  (-contains?
         scale
         (if (stringp note)
             note
           (muzak/note-symbol note))))

(defun muzak/make-chord (tonic &optional quality)
  "Construct a triad chord whose root note is TONIC.

QUALITY can be supplied to modify the quality of the chord."
  (let ((root (if (stringp tonic) (muzak/make-note :symbol tonic) tonic))
        (intervals (alist-get (or quality 'major) muzak//qualities)))
    (--map (muzak/note-add root it) intervals)))

(defun muzak/make-power-chord (tonic)
  "Construct a power chord. Just an example."
  (muzak/make-chord tonic 'power))

(defun muzak/invert-chord (chord inversion)
  "Invert CHORD INVERSION times."
  (if (<= inversion 0)
      chord
    (muzak/invert-chord
     (-snoc (cdr chord) (muzak/note-add (car chord) (muzak/octaves 1)))
     (- inversion 1))))

(defun muzak/octaves (n)
  "Steps of N octaves."
  (* n 12))

(defun muzak/bpm-seconds (bpm)
  "Determine the time of a quarter note in seconds according to BPM.

This assumes that each note is a quarter note, and a beat is every 4 notes."
  (/ 15.0 bpm))

(defun muzak/bpm ()
  "Get the current BPM.

This assumes that each note is a quarter note, and a beat is every 4 notes. "
  (/ 15.0 muzak/note-duration))

(defun muzak/set-bpm (bpm)
  "Set `muzak/note-duration' according to BPM."
  (let* ((bpm (if (> bpm muzak//max-bpm) muzak//max-bpm bpm))
         (bpm (if (< bpm muzak//min-bpm) muzak//min-bpm bpm)))
    (setf muzak/note-duration (muzak/bpm-seconds bpm))))

(defun muzak/parse (tracks-string)
  "Divine the notes within TRACKS-STRING by their stellar alignment.

Use discount code LCOLONQ for 10% off on all GNU Emacs purchases. Offer expires 1/28."
  (let ((tracks (-take 3 (s-split "|" tracks-string))))
    (--map (muzak/parse-notes it) tracks)))

(defun muzak/parse-notes (notes-string)
  "Parse a list of notes from NOTES-STRING."
  (save-match-data
    (let ((matches ())
          (idx 0))
      (while (and
              (< idx (length notes-string))
              (string-match muzak//notes-string-regexp notes-string idx))
        (setq idx (cadr (match-data)))
        (let ((match (match-string 0 notes-string)))
          (if (s-starts-with? "[" match)
              (when-let ((i (s-index-of "]" match)))
                (let* ((chord
                        (-take 4
                         (-uniq
                          (-flatten (muzak/parse-notes
                                     (s-left (1- i) (s-chop-left 1 match)))))))
                       (longest (muzak//longest-length chord)))
                  (--each chord (setf (muzak/note-length it) longest))
                  (push chord matches)))
            (push match matches))))
      (-take muzak//max-length
             (nreverse
              (--tree-map
               (if (muzak/note? it)
                   it
                 (let* ((match (if (stringp it)
                                   (s-match muzak//note-string-regexp it)
                                 (list "Z" nil nil nil)))
                        (grp (-first-item match))
                        (sym (-second-item match))
                        (oct (-third-item match))
                        (dur (-fourth-item match)))
                   (muzak/make-note
                    :symbol (if (muzak/rest-p grp) grp (upcase sym))
                    :octave (if (string-empty-p oct)
                                (when (s-lowercase? sym)
                                  (1+ muzak//middle-octave))
                              (string-to-number oct))
                    :length (unless (string-empty-p dur) (1+ (length dur))))))
               matches))))))

(defun muzak/serialize (tracks)
  "Serialize TRACKS as a string."
  (s-join "|" (--map (muzak//serialize-notes it) tracks)))

(defun muzak//serialize-notes (notes)
  "Serialize NOTES as a string."
  (--reduce-from
   (concat
    acc
    (cond ((sequencep it) (concat "[" (muzak//serialize-notes it) "]"))
          ((muzak/rest-p it) "/")
          ((muzak/note-p it)
           (concat
            (muzak/note-symbol it)
            (when-let ((oct (muzak/note-octave it)))
              (if (= oct muzak//middle-octave) "" (number-to-string oct)))
            (s-repeat (1- (or (muzak/note-length it) 1)) "~")))))
   ""
   notes))

(defun muzak//add-song (title notes-string)
  "Add a song to `muzak//song-table'.
TITLE specifies the name of the song.
NOTES-STRING is a string of notes and rests."
  (let ((hash (md5 (s-downcase title))))
    (w/db-hset "songnames" hash title)
    (w/db-hset "songnotes" hash notes-string)))

(defun muzak//get-song (song-name k)
  "Look up notes of SONG-NAME from `muzak//song-table'.
Pass the resulting notes to K."
  (let ((hash (md5 (s-downcase song-name))))
    (w/db-hget
     "songnotes" hash
     (lambda (notes)
       (if (and notes (stringp notes) (s-present? notes))
           (funcall k notes)
         (funcall k nil))))))

(defun muzak//push-song (audio-src)
  "Add AUDIO-SRC to `muzak//song-queue'"
  (add-to-list 'muzak//song-queue audio-src t (lambda (_ _) nil)))

(defun muzak//pop-song ()
  "Pop from `muzak//song-queue'"
  (pop muzak//song-queue))

(defun muzak//longest-length (notes)
  "Find the longest length among a list of notes."
  (--reduce-from
   (let ((len (cond ((sequencep it) (muzak//longest-length it))
                    ((muzak/note-p it) (or (muzak/note-length it) 1))
                    (t 1))))
     (max len acc))
   0
   notes))

(defun muzak//notes-length (notes)
  "Sum the lengths of NOTES."
  (--reduce-from
   (+ acc
      (cond ((muzak/note-p it)
             (or (muzak/note-length it) 1))
            ((muzak/rest-p it) 1)
            ((sequencep it)
             (muzak//longest-length it))
            (t 1)))
   0
   notes))

(defun muzak//notes-duration (notes)
  "Get the duration of NOTES in seconds."
  (min muzak//max-duration
       (+ (* (muzak//notes-length notes) muzak/note-duration)
          ;; (* muzak/note-duration 4)
          muzak/note-duration ; prevents stuttering at the end
          (muzak/instrument-sustain
           (alist-get muzak/instrument muzak//instruments)))))

(defun muzak/note-duration (note)
  "Determine the duration of NOTE in seconds.

The length of a chord, represented as a list of notes, is the length of its
longest constituent note."
  (cond ((muzak/note-p note)
         (* (or (muzak/note-length note) 1) muzak/note-duration))
        ((sequencep note)
         (let ((longest (muzak//longest-length note)))
           (* longest muzak/note-duration)))
        (t muzak/note-duration)))

(defun muzak//build-note-source (note start dur &optional instrument)
  "Format a note as an FFmpeg aevalsrc string."
  (let ((instrument (or instrument (alist-get muzak/instrumenet muzak//instruments))))
    (cond ((sequencep note)
           (mapconcat
            (lambda (n) (muzak//build-note-source n start dur instrument))
            note
            "+"))
          ((muzak/note-p note)
           (format "%s%.2f*between(t,%.2f,%.2f)*%s"
                   (mapconcat
                    (lambda (it)
                      (concat (funcall (alist-get it muzak//waveform-effects) start dur) "*"))
                    (muzak/instrument-effects instrument))
                   muzak/volume
                   start
                   (+ start dur (or (muzak/instrument-sustain instrument) 0))
                   (funcall
                    (alist-get (muzak/instrument-waveform instrument) muzak//waveforms)
                    (muzak/note-to-freq note)))))))

(defun muzak//build-notes-source (notes &optional instrument)
  "Build an FFmpeg aevalsrc string from NOTES."
  (let ((instrument (or instrument (alist-get muzak/instrument muzak//instruments)))
        (cur-time 0))
    (s-join
     "+"
     (-non-nil
      (--mapcat
       (let* ((dur (muzak/note-duration it))
              (src (muzak//build-note-source it cur-time dur instrument)))
         (cl-incf cur-time dur)
         (list src))
       notes)))))

(defun muzak/play-tracks (tracks &optional k)
  "Play TRACKS, a list of lists of notes.

Calls callback K when the process exits."
  (if (stringp tracks)
      (muzak/play-tracks (muzak/parse tracks))
    (let ((song-duration
           (-max (--map
                  (muzak//notes-duration it)
                  tracks)))
          (audio-source
           (s-join "+"
                   (--map-indexed
                    (let ((muzak/instrument
                           (if (= it-index 0)
                               muzak/instrument
                             'keyboard)))
                      (muzak//build-notes-source it))
                    (--filter it tracks)))))
      (muzak/play-audio-source
       (format "aevalsrc='%s:d=%.2f'" audio-source song-duration)
       k))))

(defun muzak/play-notes (notes &optional k)
  "Play NOTES, a list of notes.

Calls callback K when the process exits."
  (muzak/play-tracks
   (if (stringp notes)
       (list (muzak/parse-notes notes))
     (list notes))
   k))

(defun muzak/play-audio-source (audio-src &optional k)
  "Play AUDIO-SRC.

Calls callback K when the process exits."
  (if muzak//process
      (muzak//push-song audio-src)
    (setq
     muzak//process
     (make-process
      :name muzak//process-name
      :buffer nil
      :noquery t
      :command
      (list
       ;; "ffplay"
       ;; "-loglevel" "error"
       ;; "-autoexit"
       ;; "-nodisp"
       ;; "-af" "lowpass=f=540"
       ;; "-f" "lavfi"
       "playfilter"
       audio-src)
      :sentinel
      (lambda (_ _)
        (setq muzak//process nil)
        (when k (funcall k))
        (when-let ((next-song (muzak//pop-song)))
          (muzak/play-audio-source next-song))))))
  nil)

(defun muzak/play-notes-old (notes &optional duration)
  "Play notes.

This is nonblocking and does not use the song queue."
  (if (stringp notes)
      (muzak/play-notes-old
       (muzak/parse-notes (concat notes "/"))
       duration)
    (call-process-shell-command
     (format "
for FREQ in %s; do
    ffmpeg -strict experimental -loglevel quiet -f lavfi -i \"sine=frequency=${FREQ}:duration=%f\" -f oga -filter tremolo -filter aphaser=in_gain=0.4:out_gain=0.74:delay=0.1:decay=0.2:speed=0.2 -filter volume=%.1f - 2>/dev/null
done | ffplay -loglevel quiet -autoexit -nodisp - &"
             (mapconcat (lambda (note)(format "%d" (muzak/note-to-freq note))) notes " ")
             (or duration muzak/note-duration)
             (* 6 muzak/volume)))))

(defun muzak/stop ()
  "Terminate `muzak//process' and clear `muzak//song-queue'."
  (interactive)
  (setq muzak//song-queue nil)
  (when muzak//process
    (kill-process muzak//process)))

(defun muzak/play-song (song-name &optional k)
  "Play SONG-NAME from `muzak//song-table'.

Calls callback K when the process exits."
  (interactive "sSong Name: ")
  (muzak//get-song
   song-name
   (lambda (notes-string)
     (when notes-string
       (message "Playing %s" song-name)
       (let* ((n (string-to-number notes-string))
              (muzak/note-duration
               (if (and (> n muzak//min-bpm)
                        (< n muzak//max-bpm))
                   (muzak/bpm-seconds n)
                 muzak/note-duration)))
         (muzak/play notes-string k))))))

(defmacro muzak/with-volume (vol &rest body)
  `(let ((muzak/volume ,vol))
     ,@body))

(defmacro muzak/with-instrument (instrument &rest body)
  `(let ((muzak/instrument ,instrument))
     ,@body))

(defmacro muzak/with-duration (dur &rest body)
  `(let ((muzak/note-duration ,dur))
     ,@body))

(defmacro muzak/with-bpm (bpm &rest body)
  `(let ((muzak/note-duration (muzak/bpm-seconds ,bpm)))
     ,@body))

(defalias 'muzak/play       'muzak/play-tracks)
(defalias 'muzak/kill       'muzak/stop)
(defalias 'muzak-stop       'muzak/stop)
(defalias 'muzak-play-notes 'muzak/play-notes)
(defalias 'muzak-play-song  'muzak/play-song)
(defalias 'muzak-add-song   'muzak/add-song)

(defalias 'muzak/scale-contains? 'muzak/scale-contains-p)
(defalias 'muzak/note-same?   'muzak/note-same-p)
(defalias 'muzak/note-equals? 'muzak/note-equals-p)
(defalias 'muzak/note?        'muzak/note-p)
(defalias 'muzak/rest?        'muzak/rest-p)

;;; Demos

(defun muzak/westminster-quarters ()
  (when (boundp 'muzak/westminster-timer)
    (cancel-timer westminster-timer))
  (setq
   westminster-timer
   (run-with-timer
    1
    1
    (lambda ()
      (let ((now (decode-time (current-time)))
            (muzak/note-duration 0.3)
            (muzak/instrument 'bells))
        (when (zerop (decoded-time-second now))
          (cl-case (decoded-time-minute now)
            (0 (let ((hour (mod (decoded-time-hour now) 12)))
                 (muzak/play-notes
                  (append
                   (car (muzak//get-tracks "Westminster Quarters - Full Hour"))
                   (muzak/parse-notes (s-repeat
                                       (if (zerop hour) 12 hour)
                                       "G#////"))))))
            (15 (muzak/play-song "Westminster Quarters - First Quarter"))
            (30 (muzak/play-song "Westminster Quarters - Second Quarter"))
            (45 (muzak/play-song "Westminster Quarters - Third Quarter")))))))))

;; (when (featurep 'fig-geiser)
;;   (defun fig/announce-geiser ()
;;     "All rise."
;;     (unless (zerop (fig//geiser-counter))
;;       (muzak/play-song "Geiser's Tune"))))
;; 
;; (when (featurep 'fig-piano)
;;   (defun fig/play-user-note-fake (user)
;;     "Play a user's note."
;;     (when-let* ((midi-note (fig//get-chatter-note user))
;;                 (note (muzak/midi-to-note midi-note)))
;;       (muzak/play-notes (list note)))))

;; (when (featurep 'fig)
;;   (defun fig/show-real-face-in-4k-and-also-ssn-and-tax-info-and-embarrassing-baby-photos ()
;;     "TODO."
;;     ;; (let ((muzak/instrument 'keyboard) (muzak/note-duration 0.12)) (muzak/play-notes "C/A3F3e"))
;;     (muzak/with-instrument 'bells
;;      (muzak/with-bpm 250
;;       (muzak/play-notes
;;        (muzak/make-chord "F" 'sus2))))
;;     nil))
;; (muzak/with-instrument 'keyboard (muzak/play-song "Zelda Secret"))
(provide 'bezelea-muzak)
;;; bezelea-muzak.el ends here
