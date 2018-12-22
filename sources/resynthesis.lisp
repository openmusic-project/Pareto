;--------------------------------------------------
;
; PARETO
; Patchs d'Analyse et de Resynthèse des Echelles des Musiques de Tradition Orale
; Original patches by Fabien Lévy, 2001 with the technical help and advises from Carlos Agon, Gerard Assayag, Hans Tutschku and Frederic Voisin 
; User Library by J. Bresson, IRCAM, 2009
;
;--------------------------------------------------


(in-package :pareto)


(defmethod! sound-transpose-params ((init chord-seq) (modif chord-seq))
     :initvals '(nil nil)
     :icon 1
     :indoc '("init chord-seq" "modified chord-seq")
     :doc "Generates the transposition parameter file for SuperVP (uses OM_ASX library tools)"
     (let ((filename "tranpose-params.par")
           (minit (mapcar 'car (om::lmidic init)))
           (mmodif (mapcar 'car (om::lmidic modif))))
           
       (asx::trans-melody 
       ;(list 
        (flat (om::x-append 6000
                      (loop for i in minit
                            for m in mmodif
                            collect (om::om+ (om::om- m i) 6000))))
        (cons (car (om::lonset init))
              (om::x->dx (om::lonset init)))
        6000
        filename)))
       