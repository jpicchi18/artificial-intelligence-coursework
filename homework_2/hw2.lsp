; Name: Joseph Picchi
; UID: 605-124-511

; OVERALL EXPLANATIONS OF SOLUTIONS:

; For BFS --> if the fringe is empty, return nil. else if the first element of the fringe is an  atom, then
; perform BFS on the rest of the list and add the first element back to the front. Else the first element is
; a list, so we should append it to the end of the list to flatten out the top-level list of the first element,
; and we perform BFS on the result.

; FINAL-STATE directly compares to see if S is equal to the exact goal state (T T T T). Using an if statement,
; it returns t if the states are exactly the same and nil otherwise.

; NEXT-STATE binds the values in the current state to local variables, then checks to make sure the move is possible,
; then checks if the move results in an invalid state, then returns the move that results if everything is
; possible/valid.

; SUCC-FN applies each of the 4 possible operators to the current state S by calling NEXT-STATE and appends together all of the 4 results
; into a single list that it then returns.

; ON-PATH returns nil if STACK is empty because current state S could not possibly be a member of an empty list.
; else it checks if S is the first element of STACKS or if S appears in the cdr of STACKS.

; MULT-DFS returns nil if STATES is empty because there are no successor states that could lead to the goal state.
; It else it calls DFS on the first successor state STATES and pass along the current path, or'ing the result with
; a recursive call of MULT-DFS on the same PATH and the cdr of STATES, since either of these calls may find the goal
; state.

; DFS returns PATH with S appended if S is the goal state because this represents the path from the initial state to S.
; It checks if we already visited S by calling ON-PATH, returning nil if so because that means that we've reached a 
; dead-end in our search. Else, it performs DFS on each successor of S, in turn, by calling MULT-DFS and using
; SUCC-FN to generate the successor states of S.


;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; INPUTS: FRINGE represents a list of trees
; OUTPUTS: A top-level list of leaf nodes from the FRINGE tree in the order that they are visited by
;          left-to-right breadth-first search on FRINGE
(defun BFS (FRINGE)
  (cond ((not FRINGE) nil)
        ; if first element is an atom, then perform BFS on the rest of the list
        ((atom (first FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
        ; if first element is a list, then append it to the end of the list and perform BFS on result
        ((listp (first FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE))))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.

; INPUTS: S is the current state
; OUPUTS: t if the S is the goal state (T T T T), else nil
(defun FINAL-STATE (S)
  (if (equal S (list T T T T))
      t
      nil))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes 2 arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).


; INPUTS: S is the current state, A is the entity/entities that we desire to move
; OUTPUTS: a list containing the state that results from applying move A on state S, or
;          nil if the action is impossible or results in an invalid state
(defun NEXT-STATE (S A)
  ; bind the 4 values in the current state to named local variables
  (let ((HOMER (first S)) (BABY (second S)) (DOG (third S)) (POISON (fourth S)))
    ; return nil for impossible action (ie homer is not on the same side as the entity he should take)
    (if (or (and (eql A 'b) (not (eql HOMER BABY)))
            (and (eql A 'd) (not (eql HOMER DOG)))
            (and (eql A 'p) (not (eql HOMER POISON))))
        nil
        ; else calculate the transition state, returning nil if the resulting state is invalid
        (cond ((eql A 'h)
               (if (or (eql BABY DOG) (eql BABY POISON))
                   nil
                   (list (list (not HOMER) BABY DOG POISON))))
              ((eql A 'b) (list (list (not HOMER) (not BABY) DOG POISON)))
              ((eql A 'd)
               (if (eql BABY POISON)
                   nil
                   (list (list (not HOMER) BABY (not DOG) POISON))))
              ((eql A 'p)
               (if (eql BABY DOG)
                   nil
                   (list (list (not HOMER) BABY DOG (not POISON)))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; INPUTS: S is the current state
; OUTPUTS: a list of each state that can be reached by applying legal operators to the current state,
;          returning nil if no valid states can be reached
(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd)
          (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

; INPUTS: S is the current state, STATES is the stack of states visited by DFS
; OUTPUTS: t if S is a member of STATES and nil otherwise
(defun ON-PATH (S STATES)
  (if (not STATES)
      nil
      (or (equal S (car STATES)) (ON-PATH S (cdr STATES)))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.

; INPUTS: STATES a list of legal successor states to the last state in PATH.
;         PATH is a list of states from the initial state to the current state, inclusive
; OUTPUTS: if any of the states in STATES finds the goal state through DFS, then it returns
;          the complete path from the initial state to the goal state. else returns nil
(defun MULT-DFS (STATES PATH)
  (cond ((not STATES) nil)
        (t (or (DFS (car STATES) PATH) (MULT-DFS (cdr STATES) PATH)))))

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.

; INPUTS: S is the initial state in the DFS search. PATH is the path form the initial state to S,
;         not including S
; OUTPUTS; if a path exists from S to the goal state, it returns that path. else it returns nil.
(defun DFS (S PATH)
  ; check if S is the goal state and return PATH+S if so
  (cond ((FINAL-STATE S) (append PATH (list S)))
        ; if we already visited S before, return nil because we reached a dead-end
        ((ON-PATH S PATH) nil)
        ; else perform DFS on the successors of S
        (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))))
    
