;--------------------------------------------------
;
; PARETO
; Patchs d'Analyse et de Resynthèse des Echelles des Musiques de Tradition Orale
; Original patches by Fabien Lévy, 2001 with the technical help and advises from Carlos Agon, Gerard Assayag, Hans Tutschku and Frederic Voisin 
; User Library by J. Bresson, IRCAM, 2009
;
;--------------------------------------------------

(in-package :pareto)

;;; foramts the raw BPF out of AudioSculpt format
(defun tribpf (data)
  (let ((r (quote nil))) 
    (dotimes (n (nth 1 data) (reverse r)) 
      (let ((tempt (quote nil)) 
            (tempf (quote nil)) 
            (tempi (quote nil)))
        (dotimes (i (nth 1 (nth (+ 2 n) data)) (push (list (reverse tempt) (reverse tempf) (reverse tempi)) r)) 
          (push (nth (+ 2 (* 3 i)) (nth (+ 2 n) data)) tempt) 
          (push (nth (+ 3 (* 3 i)) (nth (+ 2 n) data)) tempf) 
          (push (nth (+ 4 (* 3 i)) (nth (+ 2 n) data)) tempi))))))


;;;
;;; PROCESSING of list of (onset midics vels)
;;;
(defun remove-repeated-onsets (thelist)
  (x-append (list (first thelist))
            (remove nil (loop for i from 1 to (- (length thelist) 1) collect
                              (if (/= (car (nth (- i 1) thelist)) (car (nth i thelist)))
                                  (nth i thelist)
                                nil
                                )))))

(defun merge-repeated-onsets (thelist)
  (let ((rep (copy-list thelist)))
    (loop for i from 1 to (- (length thelist) 1) collect
                      (when (= (car (nth (- i 1) thelist)) (car (nth i thelist)))
                          (setf (nth i rep)
                                (list (car (nth i rep))
                                      (append (list! (second (nth (- i 1) rep))) (list! (second (nth i rep))))
                                      (append (list! (third (nth (- i 1) rep))) (list! (third (nth i rep)))))
                                (nth (- i 1) rep) nil)
                          ))
    (remove nil rep)))

(defun poly-to-mono (thelist)
  (mapcar #'(lambda (item)
              (if (consp (second item))
                  (let* ((min (list-min (second item)))
                        (pos (position min (second item))))
                    (list (car item) min (if (consp (third item)) (nth pos (third item))  (third item))))
                item))
          thelist))

(defun cs-ordinate (thelist)
  (sort-list  thelist :test '< :key 'car))



(defmethod! transcribe-TRC ((inputdata list) mode &optional (minamp -36) (maxamp 6))
   :icon 1
   :initvals '(nil 'mono -36 6)
   :menuins '((1 (("mono" 'mono) ("poly" 'poly))))
   :indoc '("a data list or SDIF file" "monodic/polyphonic" "min amplitude (dB)" "max amplitude (dB)")
   :doc "Creates a raw chord-seq (no approximations) from sound analysis data. 

Supported data formats are lists from AudioSculpt partials text output or SDIF files of type 1TRC.
1TRC SDIF format can be written for instance by AudioSculpt, SuperVP/pm2, Spear, ... 
"
   (let* ((cseqdata (mat-trans (tribpf (if (consp (car inputdata)) (car inputdata) inputdata))))
          (midics (f->mc (flat (second cseqdata))))
          (onsets (om-round (om* (flat (first cseqdata)) 1000)))
          ;(vels (om-round (om+ 100 (om* (flat (third cseqdata)) 100))))
          (vels (om-round (om-scale (db->lin (flat (third cseqdata))) 
                                    20 127 (db->lin minamp) (db->lin maxamp))))
          (datalist (mat-trans (list onsets midics vels))))

     
     (setf datalist (merge-repeated-onsets (cs-ordinate datalist)))
     
     (if (equal mode 'mono) 
         (setf datalist (poly-to-mono datalist)))
     
     (setf datalist (mat-trans datalist))
     
     (make-instance 'chord-seq 
                    :lmidic (second datalist)
                    :lonset (first datalist)
                    :lvel (third datalist)))
   )


#|
(defmethod! transcribe-TRC ((inputdata sdiffile) mode &optional (minamp -36) (maxamp 6))
   (let* ((cseqdata (mat-trans (om::chord-seq-raw-data inputdata)))
          (midics (f->mc (flat (second cseqdata))))
          (onsets (om-round (om* (flat (first cseqdata)) 1000)))
          ;(vels (om-round (om+ 100 (om* (flat (third cseqdata)) 100))))
          (vels (om-round (om-scale (db->lin (flat (third cseqdata))) 20 127)))
          (datalist (mat-trans (list onsets midics vels))))
     
     (setf datalist (merge-repeated-onsets (cs-ordinate datalist)))
     
     (if (equal mode 'mono) 
         (setf datalist (poly-to-mono datalist)))
     
     (setf datalist (mat-trans datalist))
     
     (make-instance 'chord-seq 
                    :lmidic (second datalist)
                    :lonset (first datalist)
                    :lvel (third datalist)))
   )
|#


(defmethod! transcribe-TRC ((inputdata sdiffile) mode &optional (minamp -36) (maxamp 6))
  (let* ((allnotes (sort 
                    (loop for partial in (om::chord-seq-raw-data inputdata) append
                          (let ((notes-in-this-partial nil))
                            (loop for f in (f->mc (second partial))
                                  for time in (car partial) 
                                  for vel in (om-scale (db->lin (third partial)) 20 127)  
                                  unless (and notes-in-this-partial (= f (second (car notes-in-this-partial))))
                                  do (push (list (round (* time 1000)) f vel) notes-in-this-partial))
                            (reverse notes-in-this-partial)))
                   '< :key 'car)))

    (when (equal mode 'mono) 
      (let ((tmplist nil))
        (loop for item in allnotes 
              do (let ((pos (position (car item) tmplist :test '= :key 'car)))
                   (if pos 
                       (when (< (second item) (second (nth pos tmplist)))
                         (setf (nth pos tmplist) item))
                     (push item tmplist))))
        (setf allnotes (reverse tmplist))))
    
    (let ((datalist (mat-trans allnotes)))
      
      (make-instance 'chord-seq 
                     :lmidic (second datalist)
                     :lonset (first datalist)
                     :lvel (third datalist)))))
  
              
        



;;; F0

(defmethod! transcribe-F0 ((inputdata list))
   :icon 1
   :initvals '(nil)
   :indoc '("a data list or SDIF file")
   :doc "Creates a raw chord-seq (no approximations) from sound analysis data. 

Supported data formats are lists from Diphone partials text output or SDIF files of type 1FQ0
1FQ0 SDIF format can be written for instance by AudioSculpt, SuperVP/pm2, ... 
"
   (let* ((cseqdata (if (consp (car inputdata)) (mat-trans inputdata) (list-modulo inputdata 2)))
          (midics (f->mc (second cseqdata)))
          (onsets (om-round (om* (first cseqdata) 1000))))
     (make-instance 'chord-seq 
                    :lmidic midics
                    :lonset onsets)
     ))


(defmethod! transcribe-F0 ((inputdata sdiffile))
   :icon 1
   :initvals '(nil)
   :indoc '("a data list or SDIF file")
   :doc "Creates a raw chord-seq (no approximations) from sound analysis data. 

Supported data formats are lists from Diphone partials text output or SDIF files of type 1FQ0
1FQ0 SDIF format can be written for instance by AudioSculpt, SuperVP/pm2, ... 
"
   (multiple-value-bind (m o) (om::getsdifdata inputdata 0 "1FQ0" "1FQ0" nil nil nil nil nil)   
     (let ((midics (f->mc (flat m)))
           (onsets (om-round (om* o 1000))))
       (make-instance 'chord-seq 
                      :lmidic midics
                      :lonset onsets)
       )))



