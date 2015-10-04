(in-package :cl-user)
(defpackage cl-othello-test.board
  (:use :cl
        :cl-othello.board
        :cl-othello.defines
        :cl-othello.move-store
        :prove))
(in-package :cl-othello-test.board)

(plan 8)

(subtest
    "Test init-board"
  (is (init-board)
	    #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    :test #'equalp))

(subtest
    "Test is-in-board"
  (ok (is-in-board 0 0))
  (ok (is-in-board 7 7))
  (ok (is-in-board 5 3))
  (ok (not (is-in-board 5 -1)))
  (ok (not (is-in-board -1 3)))
  (ok (not (is-in-board 4 8)))
  (ok (not (is-in-board 8 4)))
  (ok (not (is-in-board nil 4)))
  (ok (not (is-in-board 3 nil))))

(defun get-next-x (x dir)
  (cl-othello.board::get-next-x x dir))

(defun get-next-y (y dir)
  (cl-othello.board::get-next-y y dir))

(subtest
    "Test get-next-x, y"
  (labels ((get-next-cell (x y dir)
	     (cons (get-next-x x dir) (get-next-y y dir))))
    (is (get-next-cell 1 2 +dir-down+)  '(1 . 3))
    (is (get-next-cell 1 2 +dir-up+)    '(1 . 1))
    (is (get-next-cell 1 2 +dir-right+) '(2 . 2))
    (is (get-next-cell 1 2 +dir-left+)  '(0 . 2))
    (is (get-next-cell 1 2 +dir-right-up+)   '(2 . 1))
    (is (get-next-cell 1 2 +dir-right-down+) '(2 . 3))
    (is (get-next-cell 1 2 +dir-left-up+)    '(0 . 1))
    (is (get-next-cell 1 2 +dir-left-down+)  '(0 . 3))))

(defparameter *test-board* (init-board))

(subtest
    "Test get-piece"
  (is (get-piece *test-board* 3 3) +black+)
  (is (get-piece *test-board* 4 3) +white+)
  (ok (is-empty (get-piece *test-board* 0 3)))
  (ok (null (get-piece *test-board* -1 5))))

(subtest
    "Test make-moves-on-board"
  (labels ((test (store expected-moves)
	     (is (move-store-count store) (length expected-moves))
	     (dolist (move expected-moves)
	       (ok (contains-move store (car move) (cdr move))))))
    (test (make-moves-on-board *test-board* +white+)
	  '((4 . 5) (5 . 4) (2 . 3) (3 . 2)))
    (test (make-moves-on-board *test-board* +black+)
	  '((3 . 5) (2 . 4) (5 . 3) (4 . 2)))
    (test (make-moves-on-board *test-board* nil) nil)))

(subtest
    "Test move-on-board"
  (subtest
      "Test error"
    (ok (not (move-on-board *test-board* -1 5 +white+)))
    (ok (not (move-on-board *test-board* 3 5 nil)))
    (ok (not (move-on-board *test-board* 5 5 +white+))))

  (subtest
      "Test move"
    (is *test-board* (init-board) :test #'equalp)
    (labels ((prove-reverse-list (result expected)
	       (is-type result 'move-store)
	       (is (move-store-count result) (length expected))
	       (let ((all t))
		 (dolist (move expected)
		   (unless (contains-move result (car move) (cdr move))
		     (setf all nil)
		     (return)))
		 (ok all "All of moves in the list are contained in the reverse result"))))
      (prove-reverse-list (move-on-board *test-board* 4 5 +white+) '((4 . 4)))
      (is *test-board*
		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		:test #'equalp)
      (move-on-board *test-board* 3 5 +black+)
      (move-on-board *test-board* 2 4 +white+)
      (prove-reverse-list (move-on-board *test-board* 5 5 +black+) '((4 . 4) (4 . 5)))
      (is *test-board*
		#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 -1 1 -1 0 0 0 0 0 1 -1 -1 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
		:test #'equalp))))

(subtest
    "Test count-piece"
  (is (count-piece *test-board* +white+) 3)
  (is (count-piece *test-board* +black+) 5)
  (ok (not (count-piece *test-board* nil)))
  (ok (not (count-piece '(1 2) +white+))))

(subtest
    "Test print-board"
  (is-print (print-board *test-board*)
		  "   01234567
|0 -------- |
|1 -------- |
|2 -------- |
|3 ---XO--- |
|4 --OOX--- |
|5 ---XXX-- |
|6 -------- |
|7 -------- |
"))

(finalize)
