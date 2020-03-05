(load "src/cglisp")

(ert-deftest string-to-init-state ()
  ())

(ert-deftest cgl-transition ()
  ;;; Tests correctness of various transitions for CGL
  (let ((init-state (matrix [0 0 0]
			    [0 0 0]
			    [0 0 0])))
    (should (equal init-state (cgl-transition init-state))))
  
  (let ((init-state (matrix [1 0 0]
			    [0 1 0]
			    [0 1 0])))
    (should (equal (cgl-transition init-state)
		   (matrix [0 0 0]
			   [1 1 0]
			   [0 0 0]))))
  
  (let ((init-state (matrix [1 1 0]
			    [1 1 0]
			    [0 0 0])))
    (should (equal (cgl-transition init-state)
		   (matrix [1 1 0]
			   [1 1 0]
			   [0 0 0]))))

  (let ((init-state (matrix [0 0 0]
			    [1 1 1]
			    [0 0 0])))
    (should (equal (cgl-transition init-state)
		   (matrix [0 1 0]
			   [0 1 0]
			   [0 1 0]))))
  
  (let ((init-state (matrix [1 1 0 0]
			    [0 1 0 0]
			    [0 0 1 0]			    
			    [0 0 1 0])))
    (should (equal (cgl-transition init-state)
		   (matrix [1 1 0 0]
			   [1 1 1 0]			   
			   [0 1 1 0]
			   [0 0 0 0])))))

(ert-deftest cgl--mget ()
  ;; should return 0 rather than fail when mget is out of bounds for matrix
  (let ((test-matrix (matrix [1 2 3]
			     [4 0 4]
			     [-1 5.3 1])))
    (should (equal (cgl--mget test-matrix 1 -1) 0))
    (should (equal (cgl--mget test-matrix -1 1) 0))
    (should (equal (cgl--mget test-matrix 3 1) 0))
    (should (equal (cgl--mget test-matrix 1 3) 0))
    (should (equal (cgl--mget test-matrix 1 1) 0))
    (should (equal (cgl--mget test-matrix 2 2) 1))
    (should (equal (cgl--mget test-matrix 0 0) 1))
    (should (equal (cgl--mget test-matrix 2 1) 5.3))))

(ert-deftest test-cgl--cell-transition ()
  (let ((init-state (matrix [1 1 0 0]
			    [0 1 0 0]
			    [0 0 1 0]			    
			    [0 0 1 0])))
    (should (equal (cgl--cell-transition init-state 1 0 0) 1))
    (should (equal (cgl--cell-transition init-state 1 0 1) 1))
    (should (equal (cgl--cell-transition init-state 0 0 2) 0))
    (should (equal (cgl--cell-transition init-state 0 1 0) 1))
    (should (equal (cgl--cell-transition init-state 1 1 1) 1))
    (should (equal (cgl--cell-transition init-state 0 1 2) 1))
    (should (equal (cgl--cell-transition init-state 1 3 2) 0))))
