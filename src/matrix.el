;;;; Simple interface to work with numerical matrices.

;;; Matrices are represented by vectors (rows) of vectors of same size
;;; (columns)

(defun matrixp (obj)
  (error "Not implemented"))

(defun matrix (&rest vectors)
  "Creates a matrix given a list of rows represented by vectors"
  (if (sequences-same-size-p vectors)
      (vconcat vectors)
    (signal 'row-sizes-mismatch '(vectors))))

(define-error 'row-sizes-mismatch "Row sizes are not all the same.")

(defun sequences-same-size-p (sequences)
  (equal (length (seq-uniq (seq-map #'length sequences))) 1))

(defun make-matrix (length &optional value)
  (error "Not implemented"))

(defun mset (matrix value row col)
  (error "Not implemented"))

(defun mget (matrix row col)
  (aref (aref matrix row) col))
