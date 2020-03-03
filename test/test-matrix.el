(load "src/matrix.el")

(ert-deftest matrix-creation ()
  (let ((test-matrix (matrix [0 1 0] [1 0 0])))
    (should (equal (mget test-matrix 0 1) 1))
    (should (equal (mget test-matrix 1 1) 0))))

(ert-deftest matrix-creation-row-size ()
  ;; All row vectors should have the same size
  :expected-result :failed
  (matrix [0] [1 2]))

(ert-deftest sequences-same-size-test ()
  (should (sequences-same-size-p '((1 2) ("ah" ha))))
  (should (not (sequences-same-size-p '((1) (3 "4"))))))

(ert-deftest matrix-equality ()
  (should (equal (matrix [0 1 2] [2 1 0]) (matrix [0 1 2] [2 1 0]))))
