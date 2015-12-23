(in-package :cl-user)
(defpackage cl-othello-test.history-record
  (:use :cl
        :cl-othello.move
        :cl-othello.move-store
        :cl-othello.history-record
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.history-record)

(plan 2)

(subtest
    "Test history-record"
  (subtest
      "Test make"
    (let ((record ($:init-history-record)))
      (ok (move-p (history-record-move record)))
      (is-type (history-record-reverse-list record) 'move-store))))

(subtest
    "Test history-record-store"
  (labels ((test-count (store expected)
	     (is (history-record-store-count store) expected))
	   (test-record (records record-idx move-idx expected-x expected-y)
	     (is (get-nth-move move-idx
				     (history-record-reverse-list
				      (aref records record-idx)))
		       (make-a-move expected-x expected-y)
		       :test #'equalp))
	   (make-fn-add-move (x y)
	     #'(lambda (record)
		 (add-to-move-store (history-record-reverse-list record) x y))))
    (subtest
	"Test make"
      (let* ((store (init-history-record-store))
	     (records (history-record-store-records store)))
	(test-count store 0)
	(ok (> (array-total-size records) 0))
	(let ((result t))
	  (loop for i from 0 below (length records)
             do (when (eq (type-of (aref records i)) 'hisotory-record)
                  (setf result nil)
                  (return)))
	  (ok result "All elements are history-record-p"))))
    
    (subtest
	"Test push and pop"
      (let* ((store (init-history-record-store))
	     (records (history-record-store-records store)))
	(subtest
	    "Test not push"
	  (push-history-record store 
				     #'(lambda (record) (declare (ignore record)) nil))
	  (test-count store 0))
	
	(subtest
	    "Test push"
	  (push-history-record store (make-fn-add-move 1 2))
	  (test-count store 1)
	  (test-record records 0 0 1 2)
	  
	  (push-history-record store (make-fn-add-move 3 4))
	  (test-count store 2)
	  (test-record records 0 0 1 2)
	  (test-record records 1 0 3 4))
	
	(subtest
	    "Test pop and re-push"
	  (is-type (pop-history-record store) 'history-record)
	  (test-count store 1)

	  (push-history-record store (make-fn-add-move 5 6))
	  (test-count store 2)
	  (test-record records 0 0 1 2)
	  (test-record records 1 0 5 6)

	  (is-type (pop-history-record store) 'history-record)
	  (is-type (pop-history-record store) 'history-record)
	  (test-count store 0)
	  (ok (not (pop-history-record store)))
	  (test-count store 0))))

    (subtest
	"Test do-history-record-store"
      (let ((store (init-history-record-store))
	    (loop-count 0))
	(push-history-record store (make-fn-add-move 1 2))
	(push-history-record store (make-fn-add-move 3 4))
	(do-history-record-store (record store)
	  (is (get-nth-move 0 (history-record-reverse-list record))
		    (if (= loop-count 0)
			(make-a-move 3 4)
			(make-a-move 1 2))
		    :test #'equalp)
	  (incf loop-count))
	(is loop-count 2)))))

(finalize)
