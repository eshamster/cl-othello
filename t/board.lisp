(in-package :cl-user)
(defpackage cl-othello-test.board
  (:use :cl
        :cl-othello.board
        :cl-othello.defines
        :cl-othello.move-store
        :cl-othello-test.test-utils
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
  (ok (not (is-in-board 8 4))))

(subtest
    "Test get-next-x, y"
  (labels ((get-next-cell (x y dir)
	     (cons ($:get-next-x x dir) ($:get-next-y y dir))))
    (is (get-next-cell 1 2 +dir-down+)  '(1 . 3))
    (is (get-next-cell 1 2 +dir-up+)    '(1 . 1))
    (is (get-next-cell 1 2 +dir-right+) '(2 . 2))
    (is (get-next-cell 1 2 +dir-left+)  '(0 . 2))
    (is (get-next-cell 1 2 +dir-right-up+)   '(2 . 1))
    (is (get-next-cell 1 2 +dir-right-down+) '(2 . 3))
    (is (get-next-cell 1 2 +dir-left-up+)    '(0 . 1))
    (is (get-next-cell 1 2 +dir-left-down+)  '(0 . 3))))

(subtest
    "Test get-piece"
  (let ((board (init-board)))
    (is (get-piece board 3 3) +black+)
    (is (get-piece board 4 3) +white+)
    (ok (is-empty (get-piece board 0 3)))
    #+sbcl (is-error (get-piece board -1 5) 'type-error)
    #-sbcl (ok (null (get-piece board -1 5)))
    ))

(subtest
    "Test make-moves-on-board"
  (labels ((test (store expected-moves)
	     (is (move-store-count store) (length expected-moves))
	     (dolist (move expected-moves)
	       (ok (contains-move store (car move) (cdr move))))))
    (let ((board (init-board)))
      (test (make-moves-on-board board +white+)
            '((4 . 5) (5 . 4) (2 . 3) (3 . 2)))
      (test (make-moves-on-board board +black+)
            '((3 . 5) (2 . 4) (5 . 3) (4 . 2)))
      (test (make-moves-on-board board nil) nil))))

(subtest
    "Test move-on-board"
  (subtest
      "Test error"
    (let ((board (init-board)))
      (ok (not (move-on-board board -1 5 +white+)))
      (ok (not (move-on-board board 3 5 nil)))
      (ok (not (move-on-board board 5 5 +white+)))))

  (subtest
      "Test move"
    (let ((board (init-board)))
      (labels ((prove-reverse-list (result expected)
                 (is-type result 'move-store)
                 (is (move-store-count result) (length expected))
                 (let ((all t))
                   (dolist (move expected)
                     (unless (contains-move result (car move) (cdr move))
                       (setf all nil)
                       (return)))
                   (ok all "All of moves in the list are contained in the reverse result"))))
        (prove-reverse-list (move-on-board board 4 5 +white+) '((4 . 4)))
        (is board
            #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 1 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            :test #'equalp)
        (move-on-board board 3 5 +black+)
        (move-on-board board 2 4 +white+)
        (prove-reverse-list (move-on-board board 5 5 +black+) '((4 . 4) (4 . 5)))
        (is board
            #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 -1 1 -1 0 0 0 0 0 1 -1 -1 0 0 0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            :test #'equalp)))))

(defun make-test-board ()
  (let ((board (init-board)))
    (move-on-board board 4 5 +white+)
    (move-on-board board 3 5 +black+)
    (move-on-board board 2 4 +white+)
    (move-on-board board 5 5 +black+)
    board))

(subtest
    "Test count-piece"
  (let ((board (make-test-board)))
    (move-on-board board 4 5 +white+)
    (move-on-board board 3 5 +black+)
    (move-on-board board 2 4 +white+)
    (move-on-board board 5 5 +black+)
    (is (count-piece board +white+) 3)
    (is (count-piece board +black+) 5)
    (ok (not (count-piece board nil)))
    (ok (not (count-piece '(1 2) +white+)))))

(subtest
    "Test print-board"
  (let ((board (make-test-board)))
    (is-print (print-board board)
              "   01234567
|0 -------- |
|1 -------- |
|2 -------- |
|3 ---XO--- |
|4 --OOX--- |
|5 ---XXX-- |
|6 -------- |
|7 -------- |
")))

(finalize)
