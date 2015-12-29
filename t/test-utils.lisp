(in-package :cl-user)
(defpackage cl-othello-test.test-utils
  (:use :cl)
  (:export :make-nth-test-game
           :within
           :prove-in
           :prove-macro-expand-error)
  (:import-from :cl-othello.game
                :init-game
                :move-game)
  (:import-from :cl-ppcre
                :regex-replace)
  (:import-from :alexandria
                :symbolicate))
(in-package :cl-othello-test.test-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (make-dispatch-macro-character #\$)
  (set-dispatch-macro-character
   #\$ #\:
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (intern (symbol-name (read stream nil))
               (regex-replace "-TEST" (package-name *package*) ""))))
  ;; for prove:macro-expand (in which "$..." is interpreted as gensym)
  (set-dispatch-macro-character
   #\$ #\$
   #'(lambda (stream &rest rest)
       (declare (ignore rest))
       (symbolicate '$ (read stream nil)))))

; this record from http://tsplans.com/i/othello/k.cgi?t=0&m=&k=d3c3c4c5f6f5e6e3f4f3e2f1f2g3h4h3h2g4b4b3d1g6g5h6c6a4b6b5d6a7a5a6d2c2c1f7f8c7c8e7d7b2g7h7a1d8a2h8a3g8e8b8a8b788h588h188g288g188b188e1
(defparameter *test-record*
  '((3 . 2) (2 . 2) (2 . 3) (2 . 4) (5 . 5) (5 . 4) (4 . 5) (4 . 2)
    (5 . 3) (5 . 2) (4 . 1) (5 . 0) (5 . 1) (6 . 2) (7 . 3) (7 . 2)
    (7 . 1) (6 . 3) (1 . 3) (1 . 2) (3 . 0) (6 . 5) (6 . 4) (7 . 5)
    (2 . 5) (0 . 3) (1 . 5) (1 . 4) (3 . 5) (0 . 6) (0 . 4) (0 . 5)
    (3 . 1) (2 . 1) (2 . 0) (5 . 6) (5 . 7) (2 . 6) (2 . 7) (4 . 6)
    (3 . 6) (1 . 1) (6 . 6) (7 . 6) (0 . 0) (3 . 7) (0 . 1) (7 . 7)
    (0 . 2) (6 . 7) (4 . 7) (1 . 7) (0 . 7) (1 . 6) (7 . 4) (7 . 0)
    (6 . 1) (6 . 0) (1 . 0) (4 . 0)))

(defun make-nth-test-game (n)
  (let ((game (init-game)))
    (labels ((f (record n)
	       (if (or (<= n 0) (null record))
		   (return-from f game))
	       (move-game game (caar record) (cdar record))
	       (f (cdr record) (- n 1))))
      (f *test-record* n))))

(defmacro within (val1 val2 epsilon)
  `(prove:ok (<= (abs (- ,val1 ,val2)) ,epsilon)))

(defmacro prove-in (val list)
  `(prove:ok (member ,val ,list)))

(defmacro prove-macro-expand-error (code expected-error)
  `(prove:is-error (macroexpand-1 ',code)
                   ,expected-error))
