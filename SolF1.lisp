
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
	(let((position (list (+ (car (state-pos st)) (+ (car (state-vel st)) (car act))) (+ (car (cdr (state-pos st))) (+ (car (cdr (state-vel st))) (car (cdr act))))))
		(velocity (list (+ (car (state-vel st)) (car act)) (+ (car (cdr (state-vel st))) (car (cdr act))))))
	(make-STATE 
		:POS position
		:VEL velocity
		:ACTION act 
		:COST (cond
				((checkPositionGoal st position) (- (state-cost st) 100))
				((isObstaclep position (state-track st)) (+ (state-cost st) 20))
				(t (+ (state-cost st) 1))
				))))


(defun checkPositionGoal (st position)
	(isGoalp (make-STATE :POS position :TRACK (state-track st))))

