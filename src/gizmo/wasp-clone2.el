;;; wasp-clone2 --- Activate the cloning device (SEQUEL) -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-twitch)
(require 'wasp-dna)

(w/defstruct
  w/c2-clone
  userid
  name
  disposition
  typingstyle
  favoriteword
  biography
  zodiac
  mbti
  enneagram
  humor
  bloodtype
  alignment
  kikibouba
  class
  stack
  element
  faction
  machiavellianism
  narcissism
  psychopathy
  quality
  head
  thorax
  abdomen)

(defun w/c2-take-random (n xs)
  "Select N random elements from XS."
  (cond
    ((> n 0) (cons (w/pick-random xs) (w/c2-take-random (- n 1) xs)))
    (t nil)))

(defmacro w/c2-gen-evaluate (trait prompt)
  "Generate a function that describes a user's TRAIT using PROMPT."
  `(defun ,(intern (format "w/c2-evaluate-%s" trait)) (logs k)
     (w/ai
       (s-join "\n" (--map (s-concat (car it) ": " (cdr it)) (w/c2-take-random 50 logs)))
       k
       ,prompt)))

(w/c2-gen-evaluate typingstyle
  "Given the provided chatroom logs for a Twitch user, perform a forensic stylometry analysis of the form of the text. Try to identify key features of the text such as usage of abnormal punctuation/capitalization that would uniquely identify the user. Your analysis should be summarized in at most two brief sentences. Do not provide any examples please. Please be brief and do not use unnecessary adjectives or adverbs! Be as short as possible! I am trying to mimic this user; present the information in a way that makes such mimicry easy!")

(w/c2-gen-evaluate zodiac
  "Given the provided chatroom logs for a Twitch user, determine that user's zodiac sign according to conventional astrology. Output only the zodiac sign and nothing else.")

(w/c2-gen-evaluate mbti
  "Given the provided chatroom logs for a Twitch user, determine that user's Myers-Briggs personality type according to conventional psychology. Output only the MBTI and nothing else.")

(w/c2-gen-evaluate enneagram
  "Given the provided chatroom logs for a Twitch user, determine that user's enneagram type according to conventional psychology. Output only the enneagram and nothing else.")

(w/c2-gen-evaluate humor
  "Given the provided chatroom logs for a Twitch user, determine that user's classical humor type according to humorism. Output only the humor and nothing else. Your only options are: sanguine, choleric, melancholic, phlegmatic")

(w/c2-gen-evaluate bloodtype
  "Given the provided chatroom logs for a Twitch user, determine that user's blood type according to Japanese blood type personality theory. Output only the blood type and nothing else. Your only options are: A+, A-, B+, B-, AB+, AB-, O+, O-")

(w/c2-gen-evaluate alignment
  "Given the provided chatroom logs for a Twitch user, determine that user's Dungeons & Dragons alignment. Output only the alignment and nothing else.")

(w/c2-gen-evaluate kikibouba
  "Given the provided chatroom logs for a Twitch user, determine whether that user is more \"kiki\" or more \"bouba\". Output only kiki or bouba and nothing else.")

(defun w/c2-new (name disposition biography)
  "Build a new (randomized) clone for NAME with DISPOSITION based on their DNA.
The clone has a BIOGRAPHY describing it."
  (let ( (clone
           (w/make-c2-clone
             :name name
             :disposition disposition
             :favoriteword "aardvark"
             :biography biography
             :class (w/pick-random (list "Fighter" "Magic-user" "Cleric" "Thief"))
             :stack (w/pick-random (list "LAMP" "MEAN" "MERN" "MEVN" "JAM" "CLONK"))
             :element (w/pick-random (list "fire" "water" "wind" "earth" "lightning" "heart"))
             :faction (w/pick-random (list "nate" "lever" "tony"))
             :quality (w/pick-random (list "F" "D" "C" "B" "A" "A+" "S-" "S+" "S++" "S+++"))
             :narcissism (w/random-probability)
             :machiavellianism (w/random-probability)
             :psychopathy (w/random-probability)
             :head
             (w/encode-string
               (f-read-bytes
                 (if (f-exists? (w/twitch-user-avatar-path name))
                   (w/twitch-user-avatar-path name)
                   (w/twitch-user-avatar-path "default"))))
             :thorax (w/random-color)
             :abdomen (w/random-color)))
         (total 0)
         (timeout 0)
         (logs (w/c2-take-random 30 (w/dna-user-log name)))
         (fields '(typingstyle zodiac mbti enneagram humor bloodtype alignment kikibouba)))
    (w/twitch-get-user-id name
      (lambda (userid)
        (setf (w/c2-clone-userid clone) userid)
        (cl-incf total)))
    (cl-decf total)
    (--each fields
      (funcall (intern (format "w/c2-evaluate-%s" it))
        logs
        (lambda (x)
          (setf (eieio-oref clone it) x)
          (cl-incf total)))
      (cl-decf total))
    (while (and (not (= total 0)) (< timeout 20))
      (message "Generating%s" (s-repeat timeout "."))
      (sleep-for 0.5)
      (cl-incf timeout))
    clone))

(defun w/c2-upload (c)
  "Upload the clone C to the database."
  (let ((countkey (format "c2_count:%s" (w/c2-clone-userid c))))
    (w/db-get countkey
      (lambda (old)
        (let* ( (idx (if (s-present? old) (string-to-number old) 0))
                (key (format "c2_clone:%s:%s" (w/c2-clone-userid c) idx)))
          (w/db-hmset key
            "userid" (w/c2-clone-userid c)
            "name" (w/c2-clone-name c)
            "disposition" (w/c2-clone-disposition c)
            "typingstyle" (w/c2-clone-typingstyle c)
            "favoriteword" (w/c2-clone-favoriteword c)
            "biography" (w/c2-clone-biography c)
            "zodiac" (w/c2-clone-zodiac c)
            "mbti" (w/c2-clone-mbti c)
            "enneagram" (w/c2-clone-enneagram c)
            "humor" (w/c2-clone-humor c)
            "bloodtype" (w/c2-clone-bloodtype c)
            "alignment" (w/c2-clone-alignment c)
            "kikibouba" (w/c2-clone-kikibouba c)
            "class" (w/c2-clone-class c)
            "stack" (w/c2-clone-stack c)
            "element" (w/c2-clone-element c)
            "faction" (w/c2-clone-faction c)
            "machiavellianism" (format "%s" (w/c2-clone-machiavellianism c))
            "narcissism" (format "%s" (w/c2-clone-narcissism c))
            "psychopathy" (format "%s" (w/c2-clone-psychopathy c))
            "quality" (w/c2-clone-quality c)
            "head" (w/c2-clone-head c)
            "thorax" (w/c2-clone-thorax c)
            "abdomen" (w/c2-clone-abdomen c))
          (w/db-set countkey (format "%s" (+ idx 1))))))))

(provide 'wasp-clone2)
;;; wasp-clone2.el ends here
