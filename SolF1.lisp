;;; 79664 Joao Pedro Martins Serras, 79714 Daniel Caramujo, Grupo 63

;;;(load "datastructures.lisp")
;;;(load "auxfuncs.lisp")

(load "datastructures.fas")
(load "auxfuncs.fas")

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
				((checkPositionGoal st position) -100)
				((isObstaclep position (state-track st)) 20)
				(t 1)
				)
		:TRACK (state-track st)
		:OTHER (state-other st))))


(defun checkPositionGoal (st position)
	(isGoalp (make-STATE :POS position :TRACK (state-track st))))

