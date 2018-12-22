;--------------------------------------------------
;
; PARETO
; Patchs d'Analyse et de Resynthèse des Echelles des Musiques de Tradition Orale
; Original patches by Fabien Lévy, 2001 with the technical help and advises from Carlos Agon, Gerard Assayag, Hans Tutschku and Frederic Voisin
; User Library by Jean Bresson, IRCAM, 2009
;
;--------------------------------------------------

(defpackage "Pareto"
  (:use "CL-USER" "COMMON-LISP" "OM" "OM-API")
  (:nicknames "PARETO"))

(in-package :pareto)

;--------------------------------------------------
; files to load 
;--------------------------------------------------

(om::require-library "morphologie" t)

(om::require-library "om_asx" t)

(unless ASX::*om-as-parameter-folder*
  (setf ASX::*om-as-parameter-folder* (namestring (om::infile nil))))



(defvar *pareto-files* nil)
(setf *pareto-files* '(
                       "transcription"
                       "analyse"
                       "modifscale"
                       "resynthesis"
                       ))


;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'(lambda (filename) 
          (compile&load (om-relative-path '("sources") filename)))
      *pareto-files*)

;--------------------------------------------------
;filling packages
;--------------------------------------------------

(fill-library '(
                ("Transcription" nil nil (transcribe-TRC transcribe-F0) nil)
                ("Analysis" nil nil (list-of-notes analyse-scale main-pitches moving-average BPF-peaks) nil)
                ("Modif scales" nil nil (approx-scale modif-scale ajust-scale) nil)
                ("Resynthesis" nil nil (sound-transpose-params) nil)
                ))


              