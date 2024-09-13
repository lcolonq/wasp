;;; wasp-pronunciation --- Canonical pronunciation -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 's)
(require 'wasp-utils)

(defconst w/pronunciation-premade ;; funny options
  '("LCOLONQ"
    "Joel"
    "mod clonk"
    "Columbo"
    "/ɛ:l.kʰɔloʊŋkʰ/"
    "Γ Column"
    "notgeiser"
    "funny magic man"
    "Lucius Coloncus Quintilianus"
    "rogueliTe"
    "Heidy Barnett"
    "Krya"
    "Laconic"
    "Loincloth"
    "Costco"
    "L Cluster"
    "Love, Chastity, Organized, Love again, Organized again, Nice, Qomputer"
    "Elkhunk"
    "late late late late show with llll colonq"
    ))

(defconst w/pronunciation-part1 ;; the LLLL
  '("El"
    "Eel"
    "El El El El"
    "La"
    "Le"
    "Luh"
    "Loo"
    "Lo"
    "Al"
    "All"
    "Ale"
    "Ail"
    "Fifty"
    "Long"
    "Long Long Long Long"
    ))

(defconst w/pronunciation-part2 ;; the Colon
  '("Colon"
    "Cologne"
    "Collin"
    "Clon"
    "Clown"
    "Clone"
    "Clun"
    "Cuhlun"
    "See"
    "Cloin"
    "Coloin"
    ))

(defconst w/pronunciation-part3 ;; the Q
  '("Kuh"
    "Queue"
    "Kweh"
    "Kiu"
    "Kiew"
    "Coo"
    "Kewl"
    ))

(defun w/pronuciation ()
  "Determine the canonical pronunciation of LCOLONQ."
  (if (= 0 (random 10))
      (w/pick-random w/pronunciation-premade)
    (let ((part1 (w/pick-random w/pronunciation-part1))
          (part2 (w/pick-random w/pronunciation-part2))
          (part3 (w/pick-random w/pronunciation-part3))
          (skip1 (= 0 (random 5)))
          (skip3 (= 0 (random 5)))
          (merge (= 0 (random 2))))
      (s-concat
       (if skip1 "" (s-concat part1 " "))
       part2
       (if skip3
           ""
         (if merge
             (s-downcase part3)
           (s-concat " " part3)))))))

(provide 'wasp-pronunciation)
;;; wasp-pronunciation.el ends here
