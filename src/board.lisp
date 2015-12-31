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

(declaim (inline is-in-board))

(defun is-in-board (x y)
  (declare (optimize (speed 3) (safety 2))
           (fixnum x y))
  (and (>= x 0) (< x +board-size+)
       (>= y 0) (< y +board-size+)))

(defun get-next-x (x dir)
  (cond ((is-right-dir dir) (incf x))
        ((is-left-dir dir) (decf x)))
  x)
(defun get-next-y (y dir)
  (cond ((is-up-dir dir) (decf y))
        ((is-down-dir dir) (incf y)))
  y)

(defun set-to-board (board x y piece)
  (let ((pnt (+ (* x +board-size+) y)))
    (setf (aref board pnt) piece))
  board)

(declaim (inline get-piece))

;; Note: If change +board-size+, the expected array size needs to be rewritten
(defun get-piece (board x y)
  (declare (optimize (speed 3) (safety 2))
           ((unsigned-byte 4) x y)
           ((simple-array fixnum (64)) board))
  (when (is-in-board x y)
    (aref board (+ (* x +board-size+) y))))

(defun print-board (board)
  (dotimes (i 3) (princ '\ ))
  (dotimes (x +board-size+) (princ x))
  (fresh-line)
  (dotimes (y +board-size+)
    (princ `\|) (princ y) (princ '\ )
    (dotimes (x +board-size+)
      (let ((piece (get-piece board x y)))
	(cond ((eq piece +white+) (princ 'o))
	      ((eq piece +black+) (princ 'x))
	      (t (princ '-)))))
    (princ "\ |")
    (fresh-line)))

(let ((tmp-move (make-a-move 1 1)))
  (defun count-reverse-to-same (board x y dir turn)
    (let ((fn-replace-by-next (get-fn-to-replace-by-next dir))
	  (rev-turn (reverse-turn turn))
	  (now-dist 0))
      (set-to-move tmp-move x y)
      (loop 
	   (aif-second-true (funcall fn-replace-by-next tmp-move)
			    (let ((next-piece (get-piece board (car it) (cdr it))))
			      (cond ((eq next-piece turn) (return))
				    ((eq next-piece rev-turn) (incf now-dist))
				    (t (return-from count-reverse-to-same 0))))
			    (return-from count-reverse-to-same 0)))
      now-dist)))
	  
(defun check-move-valid (board x y turn)
  (when (and (not (is-empty turn))
             (is-empty (get-piece board x y))) 
    (dotimes (dir 8)
      (when (< 0 (count-reverse-to-same board x y dir turn))
        (return-from check-move-valid t)))
    nil))

(defun make-moves-on-board (board turn &key (store (init-move-store)))
  (reset-move-store store)
  (dotimes (y +board-size+)
    (dotimes (x +board-size+)
      (when (check-move-valid board x y turn)
        (add-to-move-store store x y))))
  store)

(defun is-invalid-pnt-turn (x y turn)
  (or (is-empty turn)
      (not (is-in-board x y))))

(defun move-on-board (board x y turn &key (reverse-store (init-move-store)))
  (when (is-invalid-pnt-turn x y turn)
    (return-from move-on-board nil))
  (set-to-board board x y turn)
  (reset-move-store reverse-store)
  (labels ((reverse-piece (next-x next-y dir rest-piece)
	     (when (> rest-piece 0)
	       (set-to-board board next-x next-y turn)
	       (add-to-move-store reverse-store next-x next-y)
	       (reverse-piece (get-next-x next-x dir) (get-next-y next-y dir) dir (1- rest-piece)))))
    (dotimes (dir 8)
      (reverse-piece (get-next-x x dir) (get-next-y y dir) dir (count-reverse-to-same board x y dir turn))))
  (when (> (move-store-count reverse-store) 0)
    reverse-store))

(defun count-piece (board turn)
  (when (and (not (is-empty turn))
             (arrayp board)) 
    (loop for i below (* +board-size+ +board-size+)
       count (= (aref board i) turn))))
