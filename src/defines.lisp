(in-package :cl-user)
(defpackage cl-othello.defines
  (:use :cl)
  (:export :+board-size+
           :+white+
           :+black+
           :+empty+
           :+not-game-end+
           :+dir-up+
           :+dir-right-up+
           :+dir-right+
           :+dir-right-down+
           :+dir-down+
           :+dir-left-down+
           :+dir-left+
           :+dir-left-up+
           :is-empty
           :is-reverse
           :reverse-turn
           :is-up-dir
           :is-down-dir
           :is-right-dir
           :is-left-dir))
(in-package cl-othello.defines)

(defconstant +board-size+ 8)
(defconstant +white+ 1)
(defconstant +black+ -1)
(defconstant +empty+ 0)
(defconstant +not-game-end+ -999)

(defconstant +dir-up+		0)
(defconstant +dir-right-up+	1)
(defconstant +dir-right+	2)
(defconstant +dir-right-down+ 	3)
(defconstant +dir-down+		4)
(defconstant +dir-left-down+ 	5)
(defconstant +dir-left+		6)
(defconstant +dir-left-up+ 	7)

(defun is-empty (piece)
  (if (or (eq piece +white+) (eq piece +black+)) nil t))
(defun is-reverse (turn target)
  (and (not (null turn)) (not (null target))
       (eq turn (reverse-turn target))))
(defun reverse-turn (turn)
  (* turn -1))

; (1111) -> (up down right left)
(defparameter *bit-dir-arr* (make-array 8 :element-type 'fixnum))
(setf (aref *bit-dir-arr* +dir-up+        ) #b1000)
(setf (aref *bit-dir-arr* +dir-down+      ) #b0100)
(setf (aref *bit-dir-arr* +dir-right+     ) #b0010)
(setf (aref *bit-dir-arr* +dir-left+      ) #b0001)
(setf (aref *bit-dir-arr* +dir-right-up+  ) #b1010)
(setf (aref *bit-dir-arr* +dir-right-down+) #b0110)
(setf (aref *bit-dir-arr* +dir-left-up+   ) #b1001)
(setf (aref *bit-dir-arr* +dir-left-down+ ) #b0101)

(defmacro is-target-dir (dir target-bit)
  `(not (eq 0 (logand
               (aref *bit-dir-arr* ,dir)
               ,target-bit))))

(defun is-up-dir (dir)
  (is-target-dir dir #b1000))
(defun is-down-dir (dir)
  (is-target-dir dir #b0100))
(defun is-right-dir (dir)
  (is-target-dir dir #b0010))
(defun is-left-dir (dir)
  (is-target-dir dir #b0001))