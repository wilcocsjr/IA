(load "datastructures.lisp")
(load "auxfuncs.lisp")


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
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
	(list (make-node :state (problem-initial-state problem))) )
				      

;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
	(list (make-node :state (problem-initial-state problem))) )
	
;; Solution of phase 3

;; Heuristic
(defun compute-heuristic (st)
  (let ((track (state-track st)))
    (setf a ( - (car (cdr (car (track-endpositions track)))) (car (cdr (state-pos st))))))
    (- a 1))

	    
;;; A*
(defun a* (problem)
  (creatList (a*_aux (problem-initial-state problem) problem)))
  
(defun a*_aux (Istate problem)
	(let ((nodelist '()) (actualnode (stateToNode nil Istate problem)))
	(setf nodelist (append nodelist (nodelistconvert (funcall (problem-fn-nextStates problem) Istate) actualnode problem)))
	(setf actualnode nil)
	(loop for x from 1 
		do(loop for x in nodelist
			do((lambda ()
				(if (eq actualnode nil ) (setf actualnode x))
				(if (> (node-F actualnode) (node-F x)) (setf actualnode x))))
		)
		(if (funcall (problem-fn-isGoal problem) (node-state actualnode)) (return-from a*_aux actualnode))
		(if (eq actualnode nil) (return-from a*_aux nil))
		(setf nodelist (append nodelist (nodelistconvert (funcall (problem-fn-nextStates problem) (node-state actualnode)) actualnode problem)))
		
		(setf nodelist (remove actualnode nodelist))
		(setf actualnode nil)

	))
)

(defun nodelistconvert (nodelist parent problem)
	(let ((nl '()))
	(loop for x in nodelist
		do(push (stateToNode parent x problem) nl)
	) nl)
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

 
 (defun stateToNode (parent CurrentState problem)
	(make-NODE 
 		:PARENT parent
	  	:STATE CurrentState
	  	:F (if (not(eq parent nil)) (+ (+ (state-cost CurrentState) (node-g parent)) (funcall (problem-fn-h problem)CurrentState)) 
			(+ (state-cost CurrentState) (funcall (problem-fn-h problem)CurrentState)))
	  	:G (if (not(eq parent nil)) (+ (state-cost CurrentState) (node-g parent)) (state-cost CurrentState))
	  	:H (funcall (problem-fn-h problem)CurrentState)))
