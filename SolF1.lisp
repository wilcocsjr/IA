
;;; These functions, and any other ones needed must be implemented

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
  (not 
   (nth (cdr pos) 
        (nth (car pos) (state-track-env track)))))

(defun isGoalp (st) 
  (loop for x in (state-track-endpositions st)
    do(equal x (state-pos st))))

(defun nextState (st act)
	(let ((POSx (+ (car (state-pos st)) (car (state-vel st))))
		(POSy (+ (car (cdr (state-pos st))) (car (cdr (state-vel st)))))
		(pos (list POSx POSy))
		(VELx (+ (car (state-vel st)) (car act)))
		(VELy (+ (car ((cdr state-vel st))) (car (cdr act))))
		(vel (list VELx VELy)))
  	(make-STATE :POS pos
	      :VEL vel
	      :ACTION act
	      :COST -100)))

	