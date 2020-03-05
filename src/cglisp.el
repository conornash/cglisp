(load "src/matrix")

;; relative positions of cells to check for aliveness of cell (0,0)
(defconst cell-check-positions '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
  
(defun cgl-transition (state)
  "Returns a new state (represeted as a matrix) with updated data
  according to CGL's rules"
  (apply #'matrix (seq-map-indexed
		   (lambda (vector idx) (funcall #'cgl--vector-transition vector idx state))
		   state)))

(defun cgl--vector-transition (vector row-index state)
  "Returns the new vector given previous state's vector"
  (vconcat (seq-map-indexed
	    (lambda (cell col-index) (funcall #'cgl--cell-transition state cell row-index col-index))
	    vector)))

(defun cgl--cell-transition (state cell-value row col)
  (let ((alive-cells
	 ;; computes the number of alive cells around cell (row, col)
	 (apply '+ (seq-map (lambda (x) (cgl--mget state (+ row (car x)) (+ col (cadr x))))
			    cell-check-positions))))
    (cond ((= alive-cells 2) cell-value)
	  ((= alive-cells 3) 1)
	  (t 0))))

(defun cgl--mget (state row col)
  (if (and (< -1 row (length state))
	   (< -1 col (length (aref state 0))))
      (mget state row col)
    0))
