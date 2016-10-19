
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
	(make-STATE 
		:POS (list (+ (car (state-pos st)) (car (state-vel st))) (+ (car (cdr (state-pos st))) (car (cdr (state-vel st))))) 
		:VEL (list (+ (car (state-vel st)) (car act)) (+ (car (cdr (state-vel st))) (car (cdr act)))) 
		:ACTION act 
		:COST -100))




