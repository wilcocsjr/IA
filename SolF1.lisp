
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
  "generate the nextState after state st and action act"
  (make-STATE :POS '(3 16)
	      :VEL '(1 3)
	      :ACTION act
	      :COST -100))


