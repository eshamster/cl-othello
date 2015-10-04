(in-package :cl-user)
(defpackage cl-othello-test.move-store
  (:use :cl
        :cl-othello.move
        :cl-othello.move-store
        :prove))

(in-package :cl-othello-test.move-store)

(plan 7)

(subtest
    "Test init-move-store"
  (let ((store (init-move-store)))
    (is-type store 'move-store)
    (is (move-store-count store) 0)))

(subtest
    "Test add, get and reset move-store"
  (let ((store (init-move-store)))
    (subtest
	"Test adding from init"
      (is-type (add-to-move-store store 1 2) 'move-store)
      (is-type (add-to-move-store store 3 4) 'move-store)
      (is (move-store-count store) 2)
      (is (get-nth-move 0 store) (make-a-move 1 2) :test #'equalp)
      (is (get-nth-move 1 store) (make-a-move 3 4) :test #'equalp))
    (subtest
	"Test error paths of getting"
      (ok (not (get-nth-move 0 nil)))
      (ok (not (get-nth-move -1 store)))
      (ok (not (get-nth-move 2 store))))
    (subtest
	"Test resetting and re-adding"
      (is-type (reset-move-store store) 'move-store)
      (is (move-store-count store) 0)
      (add-to-move-store store 5 6)
      (is (move-store-count store) 1)
      (is (get-nth-move 0 store) (make-a-move 5 6)))))

(subtest
    "Test do-move-store and contains-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    (ok (contains-move store 1 2))
    (ok (contains-move store 3 4))
    (ok (not (contains-move store 1 10)))))

(subtest
    "Test copy and clone move-store"
  (labels ((test-isnt-same (copied org)
	     (let ((len (move-store-count org)))
	       (isnt org copied)
	       (add-to-move-store org 5 6)
	       (is (move-store-count copied) len)
	       (is (move-store-count org) (1+ len)))))
    (let* ((store (init-move-store))
	   (deep-copy (init-move-store))
	   (shallow-copy store))
      (is store shallow-copy)
      (add-to-move-store store 1 2)
      (add-to-move-store store 3 4)
      
      (is-type (copy-move-store deep-copy store) 'move-store)
      (test-isnt-same deep-copy store)

      (is-type (clone-move-store store) 'move-store)
      (test-isnt-same (clone-move-store store) store))))

(defun num-allocated-move-store-stack (stack)
  (cl-othello.move-store::num-allocated-move-store-stack stack))

(defun num-reserved-move-store-stack (stack)
  (cl-othello.move-store::num-reserved-move-store-stack stack))

(subtest
    "Test move-store-stack"
  (labels ((test-stack-num (stack allocated reserved)
	     (is (num-allocated-move-store-stack stack) allocated)
	     (is (num-reserved-move-store-stack stack) reserved)))
    (let ((store (init-move-store))
	  (stack (init-move-store-stack)))
      (test-stack-num stack 0 0)
      (add-to-move-store store 1 2)
      (add-to-move-store store 3 4)
      
      (with-cloned-move-store stack (clone1 store)
	(test-stack-num stack 1 1)
	(add-to-move-store clone1 5 6)
	(with-cloned-move-store stack (clone2 store)
	  (test-stack-num stack 2 2)
	  (add-to-move-store clone2 2 1)
	  (add-to-move-store clone2 4 3)
	  (is (move-store-count clone2) 4))
	(test-stack-num stack 2 1)
	(is (move-store-count clone1) 3))
      
      (test-stack-num stack 2 0)
      (is (move-store-count store) 2)
      
      (with-cloned-move-store stack (clone3 store)
	(test-stack-num stack 2 1)))))

(subtest
    "Test get-nth-move"
  (let ((store (init-move-store)))
    (add-to-move-store store 1 2)
    (add-to-move-store store 3 4)
    
    (ok (not (get-nth-move -1 store)))
    (ok (not (get-nth-move 10 store)))
    (ok (not (get-nth-move 0 nil)))
    (is (get-nth-move 1 store) (make-a-move 3 4))))

(subtest
    "Test mapcar-move-store"
  (let ((store (init-move-store)))
    (add-to-move-store store 5 1)
    (add-to-move-store store 1 3)
    (add-to-move-store store 3 2)
    (is (mapcar-move-store #'(lambda (move) (move-x move)) store)
        '(5 1 3)
        :test #'equal)))

(finalize)
