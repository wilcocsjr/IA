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
	;"(list (make-node :state (problem-initial-state problem)))"
		;(stateToNode nil (problem-initial-state problem))
		(print (limdepthfirstsearch_aux (problem-initial-state problem) problem lim)))
		;;(print x))
		
(defun limdepthfirstsearch_aux (state problem limite)
	(print limite)
	(if (isGoalp state) (return-from limdepthfirstsearch_aux  state))
	(if (zerop limite) (return-from limdepthfirstsearch_aux nil))
	(loop for x in (nextStates state) do
			(setf ret (limdepthfirstsearch_aux x problem (- limite 1)))
			(cond ((eq ret nil)())
				((isGoalp ret) (return-from limdepthfirstsearch_aux '(x ret)))
				((isGoal (car last ret)) (return-from limdepthfirstsearch_aux (push x ret)))
			)
	)	
)

	
(defun stateToNode (parentState CurrentState)
	(make-NODE 
 		:PARENT parentState
	  	:STATE CurrentState
	  	:F nil
	  	:G nil
	  	:H nil))
	
	
	
	
	
	
	
	;(let ((retorno ':corte) (actual-state ()))
	;	(if (eq lim 0) (return-from limdepthfirstsearch_aux retorno))
	;		(setq actual-state (problem-initial-state problem))
	;	(setf sucessores (funcall (problem-fn-nextstates problem) actual-state))
	;		(setq retorno nil)
	;		(loop 
	;			(if (null sucessores)
	;				(return-from limdepthfirstsearch_aux retorno)
	;				(lambda()
	;					(setq actual-state (car sucessores))
	;					(if (funcall problem-isGoal problem) actual-state)
	;						(return-from limdepthfirstsearch_aux  actual-state)
	;						(lambda()
	;							(if (eql lim (list-length estado-actual))
	;								(lambda() (setf sucessores (rest sucessores))
	;									     (setq retorno ':corte))
	;								(setf sucessores (append (funcall (problem-fn-nextstates problem) actual_state) (cdr sucessores)))
	;							)
	;						)
	;					)
	;				)	
	;			)
	;		)
	;	)
		
	
		
		
		
;;;(defun limdepthfirstsearch_aux (problem lim)
	;;;		(cond 	((< lim 0) nil)
		;;;			((isGoalp problem) problem)
			;;;		((and (> lim 0) (not(isGoalp problem)))
				;;;		(lambda()
					;;;		(print "entrei")
						;;;	(let ((lista (nextStates problem))))
							;;;(if (equal (list-length lista) 0) nil)
							;;;(loop for x in lista
								;;;do((lambda()
									;;;(let ((var (limdepthfirstsearch_aux x (- lim 1)))))
									;;;(if (or (isGoalp var) (isGoalp (nth (list-length var) var))) 
										;;;(if (listp var) (push problem var) '(problem var)) )
			;;;					))
		;;;					)
							
		;;;				)
		;;;			)
		;;;	)
		;;;	nil
		;;;	)
		
		
;;;(defun limdepthfirstsearch_aux (problem lim)
;;;	(let ((ret '()) (lista (nextStates problem)))
	;;;(print "==================================================")
	;;;(print "lim:")
	;;;(print lim)
	;;;(print problem)
	;;;(print "==================================================")
	;;;(print "-----------------------------------------------------")
	;;;(print lista)
	;;;(print "-----------------------------------------------------")
	;;;(if (> lim 0)
	;;;(loop for x in lista
		;;;do( (lambda()
			;;;(print x)
			;;;(cond ((equal (state-cost x) -100) (print "Encontrei"))
				;;;	((not(equal ret nil))(if (equal (state-cost (nth 0 ret)) -100) ret))
					;;;((not(equal (state-cost x) 20)) ((lambda()
							;;;(if (not(equal x (nth 0 ret))) (remove (nth 0 ret) ret))
						;;;	(push x ret)
							;;;(limdepthfirstsearch_aux x (- lim 1))))))
;;;			))))	
	;;;ret
	;;;))

	;;;push (limdepthfirstsearch_aux x (- lim 1)) ret
	
;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
	(list (make-node :state (problem-initial-state problem))) )