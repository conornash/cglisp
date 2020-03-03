(load "src/cglisp")

(ert-deftest cgl-transition ()
  ;;; Tests correctness of various transitions for CGL
  (skip-unless nil)
  (let ((init-state (matrix [0 0 0]
			    [0 0 0]
			    [0 0 0])))
    (should (equal init-state (cgl-transition init-state))))
  
  (let ((init-state (matrix [1 0 0]
			    [0 1 0]
			    [0 1 0])))
    (should (equal (cgl-transition init-state)
		   (matrix [0 0 0]
			   [0 0 0]
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
			   [0 0 1 0]
			   [0 0 0 0])))))
