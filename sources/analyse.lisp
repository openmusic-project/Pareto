;--------------------------------------------------
;
; PARETO
; Patchs d'Analyse et de Resynthèse des Echelles des Musiques de Tradition Orale
; Original patches by Fabien Lévy, 2001 with the technical help and advises from Carlos Agon, Gerard Assayag, Hans Tutschku and Frederic Voisin 
; User Library by J. Bresson, IRCAM, 2009
;
;--------------------------------------------------


(in-package :pareto)


(defmethod! list-of-notes ((self chord-seq))
     :initvals '(nil)
     :icon 1
     :indoc '("a chord-seq")
     :doc "Returns the notes present in a chord-seq"
     (remove-duplicates (flat (om::lmidic self)) :test '=))

(defun get-morphologie-stats (list)
  (mapcar #'(lambda (elt) (if (numberp (car (last-n elt 2)))
                              (last-n elt 2)
                            (first-n (last-n elt 4) 2)))
          list))

(defmethod! analyse-scale ((self chord-seq) &optional (output 'bpf))
     :initvals '(nil bpf)
     :icon 1
     :indoc '("a chord-seq" "type of output data")
     :menuins '((1 (("BPF" bpf) ("Raw data" 'raw))))
     :doc "Returns a structured analysis of the scale in the input chord-seq (pitches / number of occurences)"
     (let* ((morphologie-analysis (morph::ptrn-reson (flat (om::lmidic self))                                                    
                                                     (length (om::lmidic self))))
            (scaledata (sort-list 
                        (get-morphologie-stats morphologie-analysis)
                        :test '< :key 'car)))
       (if (equal output 'raw) 
           scaledata
         (let ((bpfdata (mat-trans scaledata)))
           (om::simple-bpf-from-list  (car bpfdata) (cadr bpfdata) 'bpf 0)))
       ))

(defmethod! main-pitches ((self chord-seq) (n number))
     :initvals '(nil 12)
     :icon 1
     :indoc '("a chord-seq" "number of pitches")
     :numouts 2
     :doc 
"Returns :
   - The main pitches in the chord-seq scales and their number of occurences (first output)
   - The main pitches only, to connect to chord object (second output)"

     (let* ((morphologie-analysis (morph::ptrn-reson (flat (om::lmidic self)) (length (om::lmidic self))))
            (scaledata (sort-list (first-n (sort-list (get-morphologie-stats morphologie-analysis) :test '> :key 'second) n) :test '< :key 'car)))
       (values scaledata
               (car (mat-trans scaledata)))
       ))
            

(defmethod! BPF-peaks ((self bpf))
     :initvals '(nil)
     :icon 1
     :indoc '("a bpf")
     :doc 
     "Returns teh maxima of a BPF (pitches / number of occurences)"
     (let ((abscisses (om::x-points self))
           (ordonnees (om::y-points self)))
       (remove nil (loop for i from 1 to (- (length abscisses) 2)
                         collect
                         (when (and (< (nth (- i 1) ordonnees) (nth i ordonnees))
                                    (>= (nth i ordonnees) (nth (+ i 1) ordonnees)))
                           (list (nth i abscisses) (nth i ordonnees)))))
       ))
            

(defmethod! moving-average ((self bpf))
     :initvals '(nil)
     :icon 1
     :indoc '("a bpf")
     :doc 
     "Smoothing of a curve (BPF) by moving average"
     (let* ((ypts (om::y-points self))
            (npts (length ypts))
            (averaged-y (append (list (/ (+ (first ypts) (second ypts)) 2))
                                (loop for i from 1 to (- npts 2) collect
                                      (/ (+ (+ (nth (- i 1) ypts) (* (nth i ypts) 2))
                                            (nth (+ i 1) ypts))
                                         4))
                                (list (/ (+ (nth (- npts 2) ypts) (nth (- npts 1) ypts)) 2)))))
       
       (om::simple-bpf-from-list (om::x-points self) averaged-y 'bpf 0)
       ))
            