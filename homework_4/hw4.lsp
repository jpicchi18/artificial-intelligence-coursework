; Name: Joseph Picchi
; UID: 605-124-511

;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;

; INPUTS: node index n, color index c, max color index k
; OUTPUTS: index of the propositional variable representing "node n recieves color c"
; LOGIC: directly applies the conversion equation "var_index = (n-1)*k + c"
(defun node2var (n c k)
  ; directly apply the conversion equation
  (+ (* (- n 1) k) c))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; INPUTS: node index n, color index c, max color index k
; OUPUTS: a clause representing the constraint "node n must be colored with at least
;  one color whose index comes from the set {c,c+1,...,k}"
; LOGIC: the constraint is the same as "node2var(c) v node2var(c+1) v ... v node2var(k)"
(defun at-least-one-color (n c k)
  ; nil list for invalid c
  (cond ((> c k) nil)
	; list of just the constraint on c if c == k
	((= c k) (list (node2var n c k)))
	; else cons the constraint on c with the constraint on values after c
	(t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))


; INPUTS: node index n, a left color index c, a right color index c2,  max color index k
; OUTPUTS: a list of clauses representing the constraint "node n must be colored with
;  at most one color whose index comes from the set {c, c+1, ..., k}
; LOGIC: the constraint is equivalent to a list containing all clauses
;  '(-node2var(x) -node2var(y)) for each x=c,c+1,...,k and y=c,c+1,...,k and x != y
(defun at-most-one-color (n c k)
  ; return nil if c is invalid value
  (cond ((> c (- k 1)) nil)
	; elses call the helper on current c and append to recursive call with c+=1
	(t (append (at-most-one-helper n c k k) (at-most-one-color n (+ c 1) k)))))


; DESCRIPTION: helper for at-most-one-color
; INPUTS: node index n, a left color index c, a right color index c2,  max color index k
; OUTPUTS: a clause '(-node2var(c) -node2var(z)) for each z = c+1,c+1,...,k
(defun at-most-one-helper (n c c2 k)
  ; return nil if c2 is invalid value
  (cond ((< c2 (+ 1 c)) nil)
	; else recursively call itself and append '((-node2var(c) -node2var(c2)))
	(t (append (at-most-one-helper n c (- c2 1) k)
		   (list (list (* -1 (node2var n c k)) (* -1 (node2var n c2 k))))))))


; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; INPUTS: a node index n, max color index k
; OUTPUTS: list of clauses that constrain n to be colored with exactly one color
;  whose index is in the set {1,2,...,k}
; LOGIC: n is exactly one color if it is at-least-one-color and at-most-one-color
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))

  
; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
; INPUTS: an edge e='(x y), max color index k
; OUTPUTS: a list of clauses that prohibit ndoes x and y from having the same color
;  in the set {1,2,...,k}
; LOGIC: just calls the helper function "gec-helper"
(defun generate-edge-clauses (e k)
  (gec-helper e k k))


; INPUTS: an edge e='(x y), max color index k
; OUTPUTS: a list of clauses that prohibit nodes x and y from having the same color
;  in the set {1,2,...,k}
; LOGIC: the constraint is equivalent to a list of all possible 
;  '((-node2var(x, z) -node2var(y, z))) where z=1,...,k
(defun gec-helper (e k c)
  ; get node indices from the edge
  (let ( (x (car e))
	 (y (car (cdr e))))
    ; recursively call itself and append '((-node2var(x, c) -node2var(y, c)))
    (cond ((< c 1) nil)
	  (t (append (gec-helper e k (- c 1))
		     (list (list (* -1 (node2var x c k)) (* -1 (node2var y c k)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
