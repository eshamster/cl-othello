(in-package :cl-user)
(defpackage cl-othello.board
  (:use :cl
        :cl-othello.defines)
  (:export :init-board
           :is-in-board
           :get-piece
           :make-moves-on-board
           :set-to-board
           :move-on-board
           :count-piece
           :print-board
           :check-move-valid)
  (:import-from :cl-othello.utils
                :aif-second-true
                :it)
  (:import-from :cl-othello.move
                :make-a-move
                :set-to-move
                :get-fn-to-replace-by-next)
  (:import-from :cl-othello.move-store
                :init-move-store
                :add-to-move-store
                :reset-move-store
                :move-store-count))
(in-package :cl-othello.board)

(defun init-board ()
  (let ((board (make-array (* +board-size+ +board-size+)
					      :element-type 'fixnum)))
    (set-to-board board 3 4 +white+)
    (set-to-board board 4 4 +black+)
    (set-to-board board 3 3 +black+)
    (set-to-board board 4 3 +white+)
    board))

(defun is-in-board (x y)
  (if (and (not (null x)) (not (null y))
       (>= x 0) (< x +board-size+) (>= y 0) (< y +board-size+))
      t
      nil))

(defun get-next-x (x dir)
  (if (is-right-dir dir) (incf x))
  (if (is-left-dir dir) (decf x))
  x)
(defun get-next-y (y dir)
  (if (is-up-dir dir) (decf y))
  (if (is-down-dir dir) (incf y))
  y)

(defun set-to-board(board x y piece)
  (let ((pnt (+ (* x +board-size+) y)))
	(setf (aref board pnt) piece))
  board)

(defun get-piece(board x y)
  (if (not (is-in-board x y))
      (return-from get-piece nil))
  (let ((pnt (+ (* x +board-size+) y)))
	(aref board pnt)))

(defun print-board(board)
  (dotimes (i 3) (princ '\ ))
  (dotimes (x +board-size+) (princ x))
  (fresh-line)
  (dotimes (y +board-size+)
    (princ `\|) (princ y) (princ '\ )
    (dotimes (x +board-size+)
      (let ((cell (get-piece board x y)))
	(cond ((eq cell 1) (princ 'o))
	      ((eq cell -1) (princ 'x))
	      (t (princ '-)))))
    (princ "\ |")
    (fresh-line)))

(let ((tmp-move (make-a-move 1 1)))
  (defun count-reverse-to-same(board x y dir turn)
    (let ((fn-replace-by-next (get-fn-to-replace-by-next dir))
	  (rev-turn (reverse-turn turn))
	  (now-dist 0))
      (set-to-move tmp-move x y)
      (loop while t do
	   (aif-second-true (funcall fn-replace-by-next tmp-move)
			    (let ((next-piece (get-piece board (car it) (cdr it))))
			      (cond ((eq next-piece turn) (return))
				    ((eq next-piece rev-turn) (incf now-dist))
				    (t (return-from count-reverse-to-same 0))))
			    (return-from count-reverse-to-same 0)))
      now-dist)))

(defun is-invalid-pnt-turn (x y turn)
  (or (is-empty turn)
      (not (is-in-board x y))))
	  
; TODO: make a test 
(defun check-move-valid(board x y turn)
  (if (or (is-invalid-pnt-turn x y turn)
	  (not (is-empty (get-piece board x y))))
      (return-from check-move-valid nil))
  (dotimes (dir 8)
    (if (< 0 (count-reverse-to-same board x y dir turn))
	(return-from check-move-valid t)))
  nil)

(defun make-moves-on-board(board turn &key (store (init-move-store)))
  (reset-move-store store)
  (dotimes (y +board-size+)
    (dotimes (x +board-size+)
      (if (check-move-valid board x y turn)
	  (add-to-move-store store x y))))
  store)

(defun move-on-board (board x y turn &key (reverse-store (init-move-store)))
  (if (or (is-invalid-pnt-turn x y turn)
	  (not (check-move-valid board x y turn)))
      (return-from move-on-board nil))
  (set-to-board board x y turn)
  (reset-move-store reverse-store)
  (labels ((reverse-piece (next-x next-y dir)
	     (when (is-reverse turn (get-piece board next-x next-y))
	       (set-to-board board next-x next-y turn)
	       (add-to-move-store reverse-store next-x next-y)
	       (reverse-piece
		(get-next-x next-x dir)
		(get-next-y next-y dir)
		dir))))
    (dotimes (dir 8)
      (if (< 0 (count-reverse-to-same board x y dir turn))
	  (reverse-piece (get-next-x x dir) (get-next-y y dir) dir))))
  (if (> (move-store-count reverse-store) 0)
      reverse-store
      nil))

(defun count-piece (board turn)
  (if (or (is-empty turn)
	  (not (arrayp board)))
      (return-from count-piece nil))
  (loop for i below (* +board-size+ +board-size+)
       count (= (aref board i) turn)))