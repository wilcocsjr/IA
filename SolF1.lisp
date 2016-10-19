
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
	(make-STATE 
		:POS (list (+ (car (state-pos st)) (car (state-vel st))) (+ (car (cdr (state-pos st))) (car (cdr (state-vel st))))) 
		:VEL (list (+ (car (state-vel st)) (car act)) (+ (car (cdr (state-vel st))) (car (cdr act)))) 
		:ACTION act 
		:COST -100))




