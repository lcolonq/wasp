;;; wasp-clone2 --- Activate the cloning device (SEQUEL) -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wasp-utils)
(require 'wasp-twitch)

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
  "Given the provided chatroom logs for a Twitch user, perform a forensic stylometry analysis of the form of the text. Try to identify key features of the text such as usage of abnormal punctuation/capitalization that would uniquely identify the user. Only describe the things that differ from a typical Twitch user. Your analysis should be summarized in at most two brief sentences. Do not provide any examples please. Please be brief and do not use unnecessary adjectives or adverbs! Be as short as possible! I am trying to mimic this user; present the information in a way that makes such mimicry easy!")

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

(defun w/c2-new (name k)
  "Build a new (randomized) clone for NAME based on their DNA and profile.
Pass the resulting clone to K."
  (w/twitch-get-user-id name
    (lambda (userid)
      (let ( (clone
               (w/make-c2-clone
                 :userid userid
                 :name name)))
        (funcall k clone)))))

(provide 'wasp-clone2)
;;; wasp-clone2.el ends here
