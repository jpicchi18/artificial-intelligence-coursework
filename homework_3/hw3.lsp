; NAME: Joseph Picchi                                        ;
; UID: 605124511

; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;

(defun sokoban (s h)
  (printstates (a* s #'goal-test #'next-states h) 0.2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (left) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list row x)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; INPUT: a state s
; OUTPUT: t iff s is a goal state, else nil
; DESCRIPTION: the goal test to check if state s is a goal state
; LOGIC/DESIGN: the function uses a cond statement to check the following. If the 
;  state is nil, then there are no unsolved boxes, so the state is a goal state. 
;  Otherwise, the function checks if there exist unsolved boxes in the first row of
;  s or in the cdr of s by recursively calling itself. If there exist unsolved boxes
;  in either one, the function returns false. else the function returns true.
; INPUTS: a state s
; OUTPUTS: t if s is a goal state, else nil
(defun goal-test (s)
  ; empty list --> contains no unsolved boxes, so it satisfies goal test
  (cond ((not s) t)
        ; else if no unsolved boxes exist return true, else nil
        (t (and
            (= (count 2 (car s)) 0)
            (goal-test (cdr s))))))

; DESCRIPTION: helper function for "next-states" that returns the int
;  value of the square at (r, c) or the value of a wall if the
;  the square is outside the scope of the problem. Returns wall if c<0 or r<0
; LOGIC/DESIGN: 
; INPUTS: a state S, a row number r, and a column number c
; OUTPUTS: the int value of the square in S at (r, c), or value of a wall
;  if the square is outside the scope of the problem
(defun get-square (s r c)
  ; return wall if square outside scope
  (cond ((not s) wall)
	((or (< r 0) (< c 0)) wall)
	; check boundaries
	(t (let* ( (row_limit (length s)) (col_limit (length (car s))))
	     ; return wall if index exceeds limit
	     (cond ((>= r row_limit) wall)
		   ((>= c col_limit) wall)
		   ; else get the element we want
		   (t (car (nthcdr c (car (nthcdr r s))))))))))

; DESCRIPTION: helper function of "try-move" that returns the state resulting from
;  setting the square at row r and column c to int value v.
; LOGIC/DESIGN: the function uses an outer cond statement that returns s if s is nil
;  because we cannot update any element, in this case. else, the function uses a let
;  statement to record the number of rows and the number of columns in local 
;  variables. If r or c exceeds the upper bound of row or column count, then we return
;  the unchanged state s. else we update the state by appending together all rows
;  before r, the updated row r, and all rows after r. We update row r by appending
;  together all elements before c in row r, the updated element c, and all elements
;  following c in row r.
; INPUTS: a state s, a row number r, a column number c, an int value v
; OUTPUTS: the state that results from setting the int at (r, c) to value v in state s
(defun set-square (s r c v)
  ; return nil for invalid s, r, c, v values
  (cond ((not s) s)
	(t (let* ( (row_limit (length s)) (col_limit (length (car s))))
	     ; return nil if (r, c) exceeds the boundaries
	     (cond ((or (>= r row_limit) (>= c col_limit) (< r 0) (< c 0)) s)
		   ; navigate to the row
		   (t (append (butlast s (- row_limit r))
			      (let ( (row (car (nthcdr r s))) )
				(list (append (butlast row (- col_limit c))
					      (list v)
					      (nthcdr (+ c 1) row))))
			      (nthcdr (+ r 1) s))))))))
			

; DESCRIPTION: helper of "next-states" that takes a state and move direction and
;  returns the state that results from moving the keeper at (krow, kcol) in s in a
;  speicified direction
; LOGIC/DESIGN: The implementation uses let* with cond statements in the variable
;  binding to record the (row1, col1) coordinate of the new keeper location and the
;  location (row2, col2) just after the new keeper location in the move direction.
;  The let* statement then calls the "get-square" helper function to record the int
;  values in the keeper square, (row1, col1) square (ie "square1"), and (row2, col2)
;  square (ie "square2"). Next, a cond statement returns nil for invalid moves (eg
;  square after the keeper is wall, 2 boxes in a row, etc). If the move is valid based
;  on the current values in s, then the function uses a let call with nested cond
;  cond statements to record the new values of the squares being updated based on each
;  of the squares' current values. Lastly, the function creates the new state by
;  calling set-square multiple times with the new values for the squares.
; INPUTS: current state s, a direction "dir" that is one of {'U, 'D, 'L, 'R}, the row
;  "krow" of the keeper in s, and the column "kcol" of the keeper in s
; OUTPUTS: the state resulting from moving the keeper at (krow, kcol) in s in
;  direction "dir", or nil if the move is illegal/invalid.
(defun try-move (s dir krow kcol)
  ; get coords (row1, col1) of new keeper location coords (row2, col2) just after that
  ; in the move direction
  (let* ( (row1 (cond ((eql dir 'U) (- krow 1))
		      ((eql dir 'D) (+ krow 1))
		      (t krow)))
	  (col1 (cond ((eql dir 'L) (- kcol 1))
		      ((eql dir 'R) (+ kcol 1))
		      (t kcol)))
	  (row2 (cond ((eql dir 'U) (- krow 2))
		      ((eql dir 'D) (+ krow 2))
		      (t krow)))
	  (col2 (cond ((eql dir 'L) (- kcol 2))
		      ((eql dir 'R) (+ kcol 2))
		      (t kcol)))
	  ; extract int contents of squares being updated
	  (ksquare (get-square s krow kcol))
	  (square1 (get-square s row1 col1))
	  (square2 (get-square s row2 col2)))
    ; return nil for invalid moves
    (cond ((isWall square1) nil)
	  ((and (or (isBox square1) (isBoxStar square1))
		(or (isWall square2) (isBox square2) (isBoxStar square2)))
	   nil)
	  ; else update to new state based on current state values
	  ; get new value for ksquare
	  (t (let ( (ksquare_new
		     ; update ksquare to blank if it's just a keeper
		     (cond ((isKeeper ksquare) 0)
			   ; update ksquare to goal if it's keeper+goal
			   ((isKeeperStar ksquare) 4)))
		    ; update square after keeper
		    (square1_new
		     ; to keeper if it's a blank or box
		     (cond ((or (isBlank square1) (isBox square1)) 3)
			   ; to keeper+goal if it's a goal or box+goal
			   ((or (isStar square1) (isBoxStar square1)) 6)))
		    ; update square that's 2 places after keeper in direction dir
		    (square2_new
		     ; to wall if it's a wall
		     (cond ((isWall square2) 1)
			   ; to box if it's a box
			   ((isBox square2) 2)
			   ; to box+goal if it's a box+goal
			   ((isBoxStar square2) 5)
			   ; if it's a blank then new value depends on square1
			   ((isBlank square2)
			    ; update to box if square1 has a box
			    (cond ((or (isBox square1) (isBoxStar square1)) 2)
				  ; else keep it blank
				  (t 0)))
			   ; if it's a goal then new value depends on square1
			   ((isStar square2)
			    ; update to box+goal if square1 is box or box+goal
			    (cond ((or (isBox square1) (isBoxStar square1)) 5)
				  ; else keep it as a goal
				  (t 4))))))
	       ; apply the moves and return
	       (set-square
		(set-square (set-square s row2 col2 square2_new) row1 col1 square1_new)
		krow kcol ksquare_new))))))
	       
; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 

; DESCRIPTION: successor function for A* search that takes a state s as input and
;  returns a list of all states reachable from s in 1 move
; LOGIC/DESIGN: the function determines the keeper position (krow, kcol) in s by
;  calling helper function "getKeeperPosition". It then tries each move up, down,
;  left, right using "try-move" with the keeper position as input, placing the result
;  of each call into a list and returning that list with nils removed from cleanUpList.
; INPUTS: a state s
; OUTPUTS: a list of all states reachable from s in 1 move
(defun next-states (s)
  ; extract keeper coordinates
  (let* ( (pos (getKeeperPosition s 0))
	  (krow (car pos))
	  (kcol (cadr pos))
	  (result (list (try-move s 'U krow kcol) (try-move s 'D krow kcol)
			(try-move s 'L krow kcol) (try-move s 'R krow kcol))))
    (cleanUpList result)))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.

; DESCRIPTION: a trivial admissible heuristic function
; LOGIC/DESIGN: returns the int 0 unconditionally.
; INPUTS: a state s
; OUTPUTS: the constant 0
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; IS THIS HEURISTIC ADMISSIBLE? Yes. The true cost from s to the goal is always at
;  least the number of misplaced boxes because we can only move 1 box per move of the 
;  keeper. Thus, it takes cost 1 to move a box once. We need to move each misplaced
;  box at least once to put it on a goal location. Thus, the cost from s to the goal
;  state is at least the number of misplaced boxes, so h1 <= h* = cost from s to goal
;  state. Ergo, h1 is admissible.
; DESCRIPTION: a heuristic function that returns the number of boxes that are not on
;  goal positions in s
; LOGIC/DESIGN: uses a cond statement that returns 0 when s is nil, else the sum of
;  the number of misplaced boxes in the car of s (using "count") and the number of 
;  misplaced boxes in the cdr of s (using a recursive call of itself).
; INPUTS: a state s
; OUTPUTS: the number of boxes that are not on goal positions in s.
(defun h1 (s)
  ; 0 misplaced boxes for a nil state
  (cond ((not s) 0)
	; else return the sum of boxes in the car and cdr of s
	(t (+ (count 2 (car s)) (h1 (cdr s))))))


; DESCRIPTION: Helper function to "box-goal-coordinates"
; LOGIC/DESIGN: takes in a row r that is a list of integers representing a row of s,
;  an integer r representating the row number of s, and the number of rows in s.
;  Using a cond statement, it returns (nil nil) for an empty row because an empty
;  row contains no boxes or goals. It then performs a recursive call on the cdr of row
;  and concatenates the coordinates of boxes or goals in the current row with those of
;  the boxes/goals in the cdr to produce a final result of the form
;  ((list of box coords) (list of goal coords))
; INPUTS: a row "row" of s, the index "r" of that row in s, and the total number of
;  rows "row_length" in s.
; OUTPUTS: a list containing 2 lists: the first nested list contains coordinates of 
;  boxes in the row, and the second contains coordinates of the goals.
(defun box-goal-coords-in-row (row r row_length)
  (cond ((not row) (list nil nil))
	(t (let* ( (cdr_result (box-goal-coords-in-row (cdr row) r row_length))
		   (boxes (car cdr_result))
		   (goals (car (cdr cdr_result))))
	     (cond ((isBox (car row)) (list (cons (list r (- row_length
							     (length row)))
						  boxes)
					    goals))
		   ((isStar (car row)) (list boxes
					     (cons (list r (- row_length
							      (length row)))
						   goals)))
		   (t cdr_result))))))

; DESCRIPTION: a helper function for "h605124511"
; LOGIC/DESIGN: returns (nil nil) for a nil state s because an empty state contains no
;  boxes or goals. Else it gets the coordinates of the boxes and goals in the cdr of
;  s, then gets the boxes/goals in the current row using "box-goal-coords-in-row".
;  It appends together all of the boxes/goals and returns the final result.
; INPUTS: a state s, the number of rows "n_rows" in s
; OUTPUTS: a list containing 2 lists: the first nested list contains coordinates of 
;  boxes in s, and the second contains coordinates of the goals.
(defun box-goal-coords (s n_rows)
  ; no boxes or goals for an empty state
  (cond ((not s) (list nil nil))
	; else get results for the cdr and car of s
	(t (let* ( (cdr_result (box-goal-coords (cdr s) n_rows))
		   (boxes (car cdr_result))
		   (goals (car (cdr cdr_result)))
		   (this_row (box-goal-coords-in-row (car s) (- n_rows (length s))
						     (length (car s))))
		   (this_row_boxes (car this_row))
		   (this_row_goals (car (cdr this_row))))
	     ; concatenate the results for the car and cdr
	     (list (append this_row_boxes boxes) (append this_row_goals goals))))))

; DESCRIPTION: helper for "closest-dist"
; LOGIC/DESIGN: adds together the absolute value of the difference between the
;  coordinates and the y coordinates in coord1 and coord2.
; INPUTS: 2 sets of coordinates "coord1" and "coord2" described by (row, col)
; OUTPUTS: the l1 distance between coord1 and coord2
(defun l1-dist (coord1 coord2)
  ; x-coord difference
  (+ (abs (- (car coord1) (car coord2)))
     ; y-coord difference
     (abs (- (car (cdr coord1)) (car (cdr coord2))))))

; DESCRIPTION: helper function for "sum-min-box-dists".
; LOGIC/DESIGN: returns 0 if there are no coordinates. Returns the l1 distance
;  between p and the coordinate if there is only 1 coordinate. Else it computes
;  the minimum l1 distance between p and any coordinate in the cdr, returning the min
;  of that and the l1 distance between p and the car of coord_list.
; INPUTS: a point p described by (row, col), a list of coordinates "coord_list"
; OUTPUTS: the minimum l1 distance between p and any coordinate in coord_list
(defun closest-dist (p coord_list)
  ; empty coord-list means there are no options, so min distance is arbitrarily 0
  (cond ((not coord_list) 0)
	; 1 option means that element must have the closest distance
	((= (length coord_list) 1) (l1-dist p (car coord_list)))
	; else compute l1-dist with car, then compare that with min of the cdr
	(t (let ( (car_dist (l1-dist p (car coord_list)))
		  (cdr_min (closest-dist p (cdr coord_list))) )
	     ; return l1 dist with car if that's the new min, else return the cdr min
	     (cond ((< car_dist cdr_min) car_dist)
		   (t cdr_min))))))

; DESCIPTION: helper function for "h605124511"
; LOGIC/DESIGN: returns 0 if there are no boxes because we have a goal state.
;  Else, it computes the closest distance between the car of box_list and all of the
;  boxes in goal_list using "closest-dist", then adds that to the sum of closest
;  distances between the boxes in the cdr of box_list and the goals in goal_list
; INPUT: a list of box coordinates "box_list", a list of goal coordinates "goal_list"
; OUPTUT: the sum of the minimum distance from each box in box_list to the goals in
;  goal_list
(defun sum-min-box-dists (box_list goal_list)
  ; no boxes means we have 0 distances between goals and boxes
  (cond ((not box_list) 0)
	; else add min distance from car box to a goal to distances in the cdr
	(t (+ (closest-dist (car box_list) goal_list)
	      (sum-min-box-dists (cdr box_list) goal_list)))))

; DESCRIPTION: helper for "any-boxes-stuck"
; LOGIC/DESIGN: returns nil if s or the box coords is empty because we have
;  have a goal state. Else it retrieves the content of each square in the four
;  directions surrounding box_coord. If 2 adjacent directions (eg left and up) are
;  walls, it returns t because the box is stuck. else it returns nil.
; INPTUS: a state s, a box coordinate "box_coord" described by (row, col)
; OUTPUTS: t of nil, whether the box at box_coord is stuck
(defun is-box-stuck (s box_coord)
  (cond ((or (not s) (not box_coord)) nil)
	(t (let* ( (r (car box_coord))
		   (c (car (cdr box_coord)))
		   (up (get-square s (- r 1) c))
		   (down (get-square s (+ r 1) c))
		   (left (get-square s r (- c 1)))
		   (right (get-square s r (+ c 1))))
	     (cond ((or (and (isBox up) (isBox right))
			(and (isBox up) (isBox left))
			(and (isBox down) (isBox left))
			(and (isBox down) (isBox right)))
		    t)
		   (t nil))))))

; DESCIPTION: helper for "h605124511"
; LOGIC/DESIGN: returns nil if s or the list of boxes is empty because no box is 
;  stuck. Eles it checks if the box in the car of boxes is stuck, or whether any 
;  boxes in the cdr of boxes is stuck, returning true if either returns true.
; INPUTS: a state s, a list of box coordinates "boxes"
; OUTPUTS: t or false, indicating whether any boxes in "boxes" is stuck against a wall
(defun any-boxes-stuck (s boxes)
  (cond ((or (not s) (not boxes)) nil)
	(t (or (is-box-stuck s (car boxes)) (any-boxes-stuck s (cdr boxes))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h605124511 (s)
  ; get coords of keeper, misplaced boxes, and unfilled targets
  (let* ( (keeper (getKeeperPosition s 0))
	  (boxes_and_goals (box-goal-coords s (length s)))
	  (boxes (car boxes_and_goals))
	  (goals (car (cdr boxes_and_goals)))
	  (result (sum-min-box-dists boxes goals)))
    (cond ((not boxes) 0)
	  ((> result 4999) 1000)
	  (t result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
