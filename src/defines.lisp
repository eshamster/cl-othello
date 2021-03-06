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

(annot:enable-annot-syntax)

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

@inline
(defun is-empty (piece)
  (not (or (eq piece +white+) (eq piece +black+))))

@inline
(defun is-reverse (turn target)
  (and (not (null turn)) (not (null target))
       (eq turn (reverse-turn target))))

@inline
(defun reverse-turn (turn)
  (* turn -1))

; (1111) -> (up down right left)
(defparameter *bit-dir-arr* (make-array 8 :element-type 'fixnum))
(dolist (pair `((,+dir-up+         . #b1000)
                (,+dir-down+       . #b0100)
                (,+dir-right+      . #b0010)
                (,+dir-left+       . #b0001)
                (,+dir-right-up+   . #b1010)
                (,+dir-right-down+ . #b0110)
                (,+dir-left-up+    . #b1001)
                (,+dir-left-down+  . #b0101)))
  (setf (aref *bit-dir-arr* (car pair))
        (cdr pair)))

(defmacro def-dir-checker (name target-bit)
  `(defun ,name (dir)
     (declare (optimize (speed 3) (safety 2))
              (fixnum dir)
              ((simple-array fixnum (8)) *bit-dir-arr*))
     (not (eq 0 (logand
                 (aref *bit-dir-arr* dir)
                 ,target-bit)))))

(def-dir-checker is-up-dir    #b1000)
(def-dir-checker is-down-dir  #b0100)
(def-dir-checker is-right-dir #b0010)
(def-dir-checker is-left-dir  #b0001)
