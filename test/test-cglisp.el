(load "src/cglisp")

(ert-deftest cgl-transition--test ()
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

(defconst correct-state-string "\
o o  oo o
    oo
o")

(defconst correct-state-string-2 "\
oo

oo")

(setq cgl-game-size 10)

(ert-deftest cgl--string-to-state--correct-state-1 ()
  (let ((result-matrix (cgl--adjust-matrix-to-size
			(matrix
			 [1 0 1 0 0 1 1 0 1]
			 [0 0 0 0 1 1 0 0 0]
			 [1 0 0 0 0 0 0 0 0])
			cgl-game-size)))
    (should (equal (cgl--string-to-state correct-state-string) result-matrix))))

(ert-deftest cgl--string-to-state--correct-state-2 ()
  (let ((result-matrix (cgl--adjust-matrix-to-size
			(matrix
			 [1 1]
			 [0 0]
			 [1 1])
			cgl-game-size)))
    (should (equal (cgl--string-to-state correct-state-string-2) result-matrix))))


(defconst wrong-state-string "\
o o o  
X o o")

(ert-deftest cgl--string-to-state--wrong-state ()
  :expected-result :failed
  (cgl--string-to-state wrong-state-string))

(defconst cgl--test-result-string-1 "\
ooo o
  o o
o oo 
     
     ")

(ert-deftest cgl--state-to-string--test ()
  (let ((test-matrix (matrix [1 1 1 0 1]
			     [0 0 1 0 1]
			     [1 0 1 1 0]
			     [0 0 0 0 0]
			     [0 0 0 0 0])))
    (should (equal (cgl--state-to-string test-matrix) cgl--test-result-string-1))))  
			    
(ert-deftest cgl--adjust-matrix-to-size--test ()
  (let ((test-matrix
	 (matrix [1 1 1 0 1]
		 [0 0 1 0 1]
		 [1 0 1 1 0])))
    (should (equal (cgl--adjust-matrix-to-size test-matrix 5)
		   (matrix
		    [1 1 1 0 1]
		    [0 0 1 0 1]
		    [1 0 1 1 0]
		    [0 0 0 0 0]
		    [0 0 0 0 0])))
    (should (equal (cgl--adjust-matrix-to-size test-matrix 7)
		   (matrix
		    [1 1 1 0 1 0 0]
		    [0 0 1 0 1 0 0]
		    [1 0 1 1 0 0 0]
		    [0 0 0 0 0 0 0]
		    [0 0 0 0 0 0 0]
		    [0 0 0 0 0 0 0]
		    [0 0 0 0 0 0 0])))))

(ert-deftest cgl--adjust-matrix-to-size--wrong-size ()
  :expected-result :failed
  (let ((test-matrix
	 (matrix [1 1 1 0 1]
		 [0 0 1 0 1]
		 [1 0 1 1 0])))
    (should (equal (cgl--adjust-matrix-to-size test-matrix 4)
		   (matrix
		    [1 1 1 0]
		    [0 0 1 0]
		    [1 0 1 1]
		    [0 0 0 0])))))

;;; First noticed bug
(defconst cgl--string-issue-1-input "oo
o

oo
o")

(defconst cgl--string-issue-1-output "oo
oo

oo
oo")

(defconst cgl--string-issue-1-output-untrimmed "oo     
oo       
  
oo   
oo


")

(ert-deftest cgl--trim-state-string--test ()
  (should (equal (cgl--trim-state-string cgl--string-issue-1-output-untrimmed)
		 cgl--string-issue-1-output)))

(ert-deftest cgl--issue-1 ()
  (should (equal cgl--string-issue-1-output
		 (cgl--trim-state-string
		  (cgl--state-to-string
		   (cgl-transition
		    (cgl--string-to-state
		     cgl--string-issue-1-input)))))))
