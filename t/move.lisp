(in-package :cl-user)
(defpackage cl-othello-test.move
  (:use :cl
        :cl-othello.defines
        :cl-othello.move
        :prove))
(in-package :cl-othello-test.move)

(plan 6)

(subtest
    "Test make-a-move"
  (is (make-a-move 1 2) '(1 . 2)))

(subtest
    "Test set-to-move"
  (let ((move (make-a-move 1 2)))
    (is (set-to-move move 3 4) (make-a-move 3 4))
    (is move (make-a-move 3 4))))

(subtest
    "Test getter (move-x, move-y)"
  (let ((move (make-a-move 1 2)))
    (is (move-x move) 1)
    (is (move-y move) 2)
    
    (is (setf (move-x move) 4) 4)
    (is (move-x move) 4)
    (is (setf (move-y move) 3) 3)
    (is (move-y move) 3)))

(subtest
    "Test move-p"
  (ok (move-p (make-a-move 1 2)))
  (ok (move-p (make-a-move 10 -2))) ; no range check
  (ok (move-p '(1 . 2)))
  (ok (not (move-p 1)))
  (ok (not (move-p '(1 2))))
  (ok (not (move-p 'test)))
  (ok (not (move-p '(not move)))))

(subtest
    "Test clone-move"
  (let* ((move (make-a-move 1 2))
	 (cloned (clone-move move)))
    (is move cloned :test #'equalp)
    (isnt move cloned :test #'eq)))

(subtest
    "Test get-fn-to-replace-by-next"
  (labels ((test-ok (dir expected-x expected-y)
	     (let ((move (make-a-move 4 4))
		   (expected-move (make-a-move expected-x expected-y)))
	       (multiple-value-bind (tmp-move suc)
		   (funcall (get-fn-to-replace-by-next dir) move)
		 (is tmp-move expected-move)
		 (is move expected-move)
		 (ok suc))))
	   (test-ng (dir start-x start-y)
	     (let ((move (make-a-move start-x start-y)))
	       (multiple-value-bind (tmp-move suc)
		   (funcall (get-fn-to-replace-by-next dir) move)
		 (declare (ignore tmp-move))
		 (ok (not suc)))))
	   (test (dir x1 y1 x2 y2)
	     (test-ok dir x1 y1)
	     (test-ng dir x2 y2)))
    (test +dir-up+         4 3  4 0)
    (test +dir-down+       4 5  4 7)
    (test +dir-right+      5 4  7 4)
    (test +dir-left+       3 4  0 4)
    (test +dir-left-up+    3 3  4 0)
    (test +dir-left-down+  3 5  0 4)
    (test +dir-right-up+   5 3  7 4)
    (test +dir-right-down+ 5 5  4 7)))

(finalize)
