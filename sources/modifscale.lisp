;--------------------------------------------------
;
; PARETO
; Patchs d'Analyse et de Resynthèse des Echelles des Musiques de Tradition Orale
; Original patches by Fabien Lévy, 2001 with the technical help and advises from Carlos Agon, Gerard Assayag, Hans Tutschku and Frederic Voisin 
; User Library by J. Bresson, IRCAM, 2009
;
;--------------------------------------------------


(in-package :pareto)


(defmethod! approx-scale ((self chord-seq) approx)
     :initvals '(nil 0)
     :icon 1
     :indoc '("a chord-seq" "subdivision approx")
     :menuins '((1 (("None" 0) ("1/2 tones" 2) ("1/4 tones" 4) ("1/8 tones" 8))))
     :doc "Approximates a scale in half tones (approx = 2), quarter tones (approx = 4), eight tones (approx = 8), ..."
     (let ((approx-factor (if (and (numberp approx) (> approx 0))
                              (round 200 approx) nil))
           (rep (clone self)))
       (if approx-factor 
           (setf (om::lmidic rep) (om* (om-round (om/ (om::lmidic self) approx-factor)) approx-factor)))
       rep))


(defmethod! modif-scale ((self list) initscale newscale)
     :initvals '(nil nil nil)
     :icon 1
     :indoc '("a list of pitches or a chord-seq" "original scale (list)" "modified scale (list)")
     :doc "Moves piches in <self> from <initscale> to <newscale>"
     (let ((init (sort initscale '<))
           (new (sort newscale '<)))
       (remove nil
               (loop for element in self 
                     collect (if (listp element)
                                 (modif-scale element initscale newscale)
                               (let ((pos (position element init :test '=)))
                                 (if (and pos (nth pos new)) (nth pos new) element)))))
       ))

(defmethod! modif-scale ((self chord-seq) initscale newscale)
     (let ((rep (clone self)))
       (setf (om::lmidic rep)
             (modif-scale (om::lmidic self) initscale newscale))
       rep))



(defun select-in-scale (num list)
  (let ((val num))
    (loop for item in list do
          (when (and (>= num (first item))
                     (< num (second item)))
            (setf val (third item))))
    val))

(defmethod! ajust-scale ((self list) scale-list)
     :initvals '(nil nil)
     :icon 1
     :indoc '("a list of pitches or a chord-seq" "a formatted scale modif list")
     :doc "Adjusts <self> according to <scale-list>.

<scale-list> is formatted as ((min1 max1 val1) (min2 max2 val2) ...)
This function replaces every element in <self> by valN if n >= minN and n < maxN."
     (loop for elt in self collect
           (if (listp elt)
               (ajust-scale elt scale-list)
             (select-in-scale elt scale-list))))

(defmethod! ajust-scale ((self chord-seq) scale-list)
     (let ((rep (clone self)))
       (setf (om::lmidic rep)
             (ajust-scale (om::lmidic self) scale-list))
       rep))