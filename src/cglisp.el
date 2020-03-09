;;;;
;;;; Conway's Game of Life implementation 
;;;; ====================================

(require 'subr-x)
(load "src/matrix")


(setf cgl-game-size 50)

;;;
;;; Main commands
;;;

(defconst cgl-buffer-name "* CGL *")

(defun cgl-start ()
  "Opens a buffer to let the user describe an initial state
   Clears the buffer if previously used by a game."
  (interactive)
  (if (get-buffer cgl-buffer-name) (kill-buffer cgl-buffer-name))
  (switch-to-buffer (get-buffer-create cgl-buffer-name)))

(defun cgl-step ()
  "Runs a step of the game from the current state string in
   the CGL buffer"
  (interactive)
  (if (get-buffer cgl-buffer-name)
      (switch-to-buffer cgl-buffer-name)
    (error "Please create a game state with cgl-start"))
  (setq buffer-read-only nil)
  (let ((next-string
	 (cgl--state-to-string (cgl-transition (cgl--string-to-state (buffer-string))))))
    (erase-buffer)
    (insert next-string))
  (setq buffer-read-only t))

;;;
;;; Transitioning from a game state to the next
;;;

;; relative positions of cells to check for aliveness of cell (0,0)
(defconst cgl--cell-check-positions '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))

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
			    cgl--cell-check-positions))))
    (cond ((= alive-cells 2) cell-value)
	  ((= alive-cells 3) 1)
	  (t 0))))

(defun cgl--mget (state row col)
  (if (and (< -1 row (length state))
	   (< -1 col (length (aref state 0))))
      (mget state row col)
    0))

;;;
;;; Converting a string to a game state
;;;

(defun cgl--string-to-state (string)
  (cgl--adjust-matrix-to-size
   (vconcat (mapcar #'cgl--line-string-to-vector
		    (split-string string "[\n\f\r]")))
   cgl-game-size))

(defun cgl--line-string-to-vector (line-string)
  (vconcat (mapcar (lambda (char)
		     (cond ((equal char ?\s) 0)
			   ((equal char ?o) 1)
			   (t (error "Please only use 'o' and spaces"))))
		   line-string)))

(defun cgl--adjust-matrix-to-size (matrix size)
  "Given a matrix, resize it as a square matrix to fit size
  size must be bigger than matrix's largest dimension"
  (let ((row (length matrix)) (col (length (aref matrix 0))))
    (if (or (> row size)
	    (> col size))
	(error "Cannot resize matrix with smaller size")
      ;; apply row-resizing function to each row, with added nil rows to match size
      (vconcat (mapcar (lambda (row) (vconcat row (make-vector (- size (length row)) 0)))
		       (vconcat matrix (make-vector (- size row) [])))))))

;;;
;;; Converting a game state to a displayable string
;;;

(defun cgl--state-to-string (state)
  (mapconcat #'cgl--row-to-string
	     state "\n"))

(defun cgl--row-to-string (row)
  (apply 'string (mapcar (lambda (val) (if (= val 1) ?o ?\s)) row)))

;;;
;;; Utils
;;;

(defun cgl--trim-state-string (stringstate)
  "Utility to remove trailing spaces and ending newlines
   for easier string state comparisons"
  (string-trim-right
   (mapconcat
    #'string-trim-right
    (split-string stringstate "[\n\f\r]+")
    "\n")))
