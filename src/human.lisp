(in-package :cl-user)
(defpackage cl-othello.human
  (:use :cl)
  (:export :eval-play-command)
  (:import-from :cl-othello.game
                :print-game
                :reverse-game
                :move-game))
(in-package :cl-othello.human)

(defun exec-move-com (game args)
  (when (< (length args) 2)
    (princ "The Move command requires 2 numbers as args")
    (return-from exec-move-com nil))
  (let ((x (car args))
	(y (cadr args)))
    (when (not (and (numberp x) (numberp y)))
      (princ "The Move command requires 2 numbers as args")
      (return-from exec-move-com nil))
    (move-game game x y)))

(defun eval-play-command (game com-list)
  (string-case:string-case ((string-upcase (car com-list)))
    ("PRINT" (print-game game) nil)
    ("REVERSE" (reverse-game game))
    ("MOVE" (exec-move-com game (cdr com-list)))
    (t (format t "The command \"~A\" is not defined.~%" (car com-list))
       nil)))
