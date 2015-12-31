(in-package :cl-user)
(defpackage cl-othello.move
  (:use :cl
        :cl-othello.defines)
  (:export :make-a-move
           :set-to-move
           :move-x
           :move-y
           :move-p
           :clone-move
           :get-fn-to-replace-by-next))
(in-package :cl-othello.move)

(defun make-a-move (x y)
  `(,x . ,y))

(defun set-to-move (move x y)
  (setf (car move) x)
  (setf (cdr move) y)
  move)

(defmacro move-x (move)
  `(car ,move))

(defmacro move-y (move)
  `(cdr ,move))

(defun move-p (obj)
  (handler-case
      (and (numberp (car obj))
	   (numberp (cdr obj)))
    (t (e) (declare (ignore e)) nil)))

(defun clone-move (move)
  (make-a-move (move-x move) (move-y move)))

(declaim (inline move-is-in-board))

(defun move-is-in-board (move)
  (let ((x (car move))
	(y (cdr move)))
    (declare (fixnum x y))
    (and (>= x 0) (< x +board-size+) (>= y 0) (< y +board-size+))))

(defparameter fns-replace-by-next (make-array 8))

(defmacro make-fn-to-replace-by-next-move (x-diff y-diff)
  `(lambda (move)
     ,(when (not (eq x-diff 0))
            `(incf (car move) ,x-diff))
     ,(when (not (eq y-diff 0))
            `(incf (cdr move) ,y-diff))
     (values move (move-is-in-board move))))

(labels ((set-fn (dir x-diff y-diff)
	   (setf (aref fns-replace-by-next dir)
		 (make-fn-to-replace-by-next-move x-diff y-diff))))
  (set-fn +dir-up+          0 -1)
  (set-fn +dir-down+        0  1)
  (set-fn +dir-left+       -1  0)
  (set-fn +dir-right+       1  0)
  (set-fn +dir-left-up+    -1 -1)
  (set-fn +dir-left-down+  -1  1)
  (set-fn +dir-right-up+    1 -1)
  (set-fn +dir-right-down+  1  1))

(declaim (inline get-fn-to-replace-by-next))

(defun get-fn-to-replace-by-next (dir)
  (declare (optimize (speed 3) (safety 2))
           (fixnum dir)
           ((simple-array function (8)) fns-replace-by-next))
  (aref fns-replace-by-next dir))
