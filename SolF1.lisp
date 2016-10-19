
;;; These functions, and any other ones needed must be implemented

(load "datastructures.lisp")
(load "auxfuncs.lisp")

(defun isObstaclep (pos track) 
   (not (nth (car (reverse pos)) 
        (nth (car pos) (track-env track)))))

(defun isGoalp (st)
  (let ((posfinal 0))
   (loop for x in (track-endpositions (state-track st))
        do(if (equal x (state-pos st)) (setf posfinal 1)))
   (if (equal posfinal 1) t)))

(defun nextState (st act)
	(let ((POSx (+ (car state-pos st) (car state-vel st)))
		(POSy (+ (car (cdr state-pos st)) (car (cdr state-vel st))))
		(pos '(list POSx POSy))
		(VELx (+ (car state-vel st) (car act)))
		(VELy (+ (car (cdr state-vel st)) (car (cdr act))))
		(vel '(list VELx VELy)))
  	(make-STATE :POS pos
	      :VEL vel
	      :ACTION act
	      :COST -100)))

	
