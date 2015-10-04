(in-package :cl-user)
(defpackage cl-othello-test.tree
  (:use :cl
        :cl-othello.tree
        :prove))
(in-package :cl-othello-test.tree)

(plan 5)

(subtest
    "Test add-child"
  (subtest
      "Test result"
    (defun test (f)
      (is (funcall f '(1) 2) '(1 (2)))
      (is (funcall f '(1) 2 3) '(1 (2) (3)))
      (is (funcall f '(1) '(2)) '(1 (2)))
      (is (funcall f '(1 (2)) '3) '(1 (2) (3)))
      (is (funcall f '(1) (funcall f '(2) 3)) '(1 (2 (3))))
      (is (funcall f '(1) nil) '(1 (nil)))
      (is (funcall f '(1) `(,nil)) '(1 (nil))))
    (test #'add-child)
    (test #'push-child))
  (subtest
      "Test destructiveness"
    (let ((test-tree '(1)))
      (add-child test-tree 2)
      (is test-tree '(1))
      (push-child test-tree 2)
      (is test-tree '(1 (2))))))

(subtest
    "Test do-children"
  (let ((test-tree (add-child '(1) 2 3 4))
	(test-list nil))
    (do-children (node test-tree)
      (setf test-list (cons (+ (get-node-value node) 1) test-list)))
    (is test-list '(5 4 3))))

(subtest
    "Test get-num-child"
  (let ((test-tree '(1)))
    (ok (not (has-children test-tree)))
    (is (get-num-children test-tree) 0)
    
    (setf test-tree (add-child test-tree 2))
    (ok (has-children test-tree))
    (is (get-num-children test-tree) 1)
    
    (setf test-tree (add-child test-tree 3))
    (ok (has-children test-tree))
    (is (get-num-children test-tree) 2)))

(subtest
    "Test tree size and depth"
  (let ((test-tree
	 (add-child '(1) (add-child '(2) 3 4 5) 6)))
    (is (get-tree-size test-tree) 6)
    (is (get-tree-depth test-tree) 2)))

(subtest
    "Test some getter funcs"
  (let ((test-tree (add-child '(1) 2 3 4)))
    (is (get-node-value test-tree) 1)
    
    (is (get-children test-tree) '((2) (3) (4)))
    (is (get-children '(1)) nil)
    
    (is (get-rest-children (get-children test-tree)) '((3) (4)))
    
    (is (get-nth-child 0 test-tree) '(2))
    (is (get-nth-child 1 test-tree) '(3))
    (is (get-nth-child 2 test-tree) '(4))
    (is (get-nth-child 3 test-tree) nil)
    (setf (car (get-nth-child 1 test-tree)) 5)
    (is (get-nth-child 1 test-tree) '(5))
    
    (subtest
	"Test select max funcs"
      (is (select-max-child #'(lambda(val) (* val -1)) test-tree) '(2))
      
      (is (select-max-node #'(lambda(val) (* val -1)) '(1 2 3)) 1))))


(finalize)
