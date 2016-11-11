;;; 79664 Joao Pedro Martins Serras, 79714 Daniel Caramujo, Grupo 63

;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

(load "datastructures.fas")
(load "auxfuncs.fas")

;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
	(let ((ret '()))
		(loop for el in (possible-actions)
			do(push (nextState st el) ret))
	ret))



;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
		(creatList (limdepthfirstsearch_aux (stateToNode nil (problem-initial-state problem)) problem lim)))
		
		
(defun limdepthfirstsearch_aux (state problem limite )
	(let ((ret nil)(no nil))
	(if (funcall (problem-fn-isGoal problem) (node-state state)) (return-from limdepthfirstsearch_aux  state))
	(if (zerop limite) (return-from limdepthfirstsearch_aux nil))
	(loop for x in (funcall (problem-fn-nextstates problem) (node-state state))
			do((lambda()
				(setf no (stateToNode state x))
				(setf ret (limdepthfirstsearch_aux no problem (- limite 1)))
				(cond ((eq ret nil)())
					((funcall (problem-fn-isGoal problem) (node-state ret)) (return-from limdepthfirstsearch_aux   ret))
				)))	
	))
)

(defun creatList (node)
	(let ((sol '()) (parentNode (node-parent node))) 
	(push (node-state node) sol)	
	(loop
		(push (node-state parentNode) sol)
		(if (equal (node-parent parentNode) nil) (return-from creatList sol))
		(setf parentNode (node-parent parentNode))
	)
))

		
(defun stateToNode (parentState CurrentState)
	(make-NODE 
 		:PARENT parentState
	  	:STATE CurrentState
	  	:F nil
	  	:G nil
	  	:H nil))
	
;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
	(let ((limite 0) (ObjectiveNode nil))
	(loop 
		(setf ObjectiveNode (limdepthfirstsearch_aux (stateToNode nil (problem-initial-state problem)) problem limite))
		(if (equal ObjectiveNode nil) (setf limite (+ limite 1)) (return-from iterlimdepthfirstsearch (creatList ObjectiveNode)))
	))
)