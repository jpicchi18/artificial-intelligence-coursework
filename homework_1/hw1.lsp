; NAME: Joseph Picchi
; UID : 605124511

; OVERVIEW OF SOLUTIONS:
; All 3 functions implemented below use recursion with 2 or 3 base cases to calculate the solution to the problem at hand.
;
; The PAD function simply returns 1 if its input argument N is 0, 1, or 2 because, by definition of the Padovan sequence, 
; PAD(0)=PAD(1)=PAD(2)=1. Otherwise, PAD(N) recursively computes PAD(N-2) and PAD(N-3) and sums them together.
;
; For the SUMS function, the number of sums to calculate a base case of PAD is 0 because these are pre-defined. Thus,
; SUMS(0)=SUMS(1)=SUMS(2)=0. Otherwise, the number of additions to calculate PAD is equivalent to the number of sums to 
; calculate PAD(N-2) plus the number of sums to calculate PAD(N-3) plus 1 additional sum to add together PAD(N-2) and PAD(N-3).
; 
; For the ANON function, an empty tree input leads to an empty tree output because we need not replace any leaves with "?".
; A tree input that consists of just one leaf element returns "?" because only replaced one element with one "?" symbol.
; Any other tree input is transformed by anonymizing the first element of the tree list, then anonymizing the rest of the
; elements of the tree list, then putting those results together using "cons".



; ARGUMENT: a single integer N>0
; RETURNS: the Nth number in the Padovan sequence
(defun PAD (N)
  (if (< N 3)
      1
      (+ (PAD (- N 2)) (PAD (- N 3)))))

; ARGUMENT: a single integer N>0
; RETURNS: the number of addition operations necessary for the PAD function to calculate the Nth number in the Padovan Sequence.
(defun SUMS (N)
  (if (< N 3)
      0
      (+ 1 (SUMS (- N 2)) (SUMS (- N 3)))))

; ARGUMENT: a list or atom TREE
; RETURNS: An anonymized version of TREE, where each symbol or number in TREE has been replaced with the symbol "?"
(defun ANON (TREE)
  (cond ((not TREE) nil)
        ((atom TREE) '?)
        (t (cons
            (ANON (car TREE))
            (ANON (cdr TREE))))))
