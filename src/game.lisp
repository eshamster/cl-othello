(in-package :cl-user)
(defpackage cl-othello.game
  (:use :cl
        :cl-othello.defines)
  (:export :game
           :game-board
           :game-turn
           :init-game
           :move-game
           :make-moves
           :reverse-game
           :reverse-game-to-depth
           :get-game-depth
           :is-game-end
           :get-game-result
           :do-in-move-reverse
           :print-game
           :is-game-same-phase)
  (:import-from :cl-othello.move
                :set-to-move
                :move-x
                :move-y)
  (:import-from :cl-othello.move-store
                :init-move-store
                :move-store-count
                :do-move-store)
  (:import-from :cl-othello.board
                :init-board
                :make-moves-on-board
                :move-on-board
                :set-to-board
                :get-piece
                :count-piece
                :check-move-valid
                :print-board)
  (:import-from :cl-othello.history-record
                :history-record-move
                :history-record-turn
                :history-record-reverse-list
                :init-history-record-store
                :push-history-record
                :pop-history-record
                :history-record-store-records
                :history-record-store-count
                :do-history-record-store)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-othello.game)

(defstruct game
  board
  turn
  move-store
  history)

(defun init-game()
  (let ((a-game (make-game :board (init-board)
			   :turn 1
			   :move-store (init-move-store)
			   :history (init-history-record-store))))
    a-game))

(defun is-game-end(game)
  (labels ((neq (x y) (not (eq x y))))
    (let ((turn (game-turn game)))
      (and (neq turn +white+) (neq turn +black+)))))

; TODO: compare by hash value
(defun is-game-same-phase(game1 game2)
  (and
   (eq (game-turn game1) (game-turn game2))
   (equalp (game-board game1) (game-board game2))))

(defun judge-next-turn (moved-game)
  (let* ((board (game-board moved-game))
	 (turn (game-turn moved-game))
	 (rev-turn (reverse-turn turn)))
    (if (< 0 (move-store-count (make-moves-on-board board rev-turn
                                                    :store (game-move-store moved-game))))
	rev-turn
	(if (< 0 (move-store-count (make-moves-on-board board turn
                                                        :store (game-move-store moved-game))))
	    turn
	    0))))

(defun move-game-by-xy (game x y)
  (let ((board (game-board game))
	(turn (game-turn game)))
    (unless (check-move-valid board x y turn)
      (return-from move-game-by-xy nil))
    (push-history-record
     (game-history game)
     #'(lambda (record)
	 (block exit
	   (unless (move-on-board (game-board game) x y turn
				  :reverse-store (history-record-reverse-list record))
	     (return-from exit nil))
	   (set-to-move (history-record-move record) x y)
	   (setf (history-record-turn record) turn)
	   t)))
    (setf (game-turn game) (judge-next-turn game))
    game))

;; TODO: add test
(defmacro move-game (game &rest rest)
  (case (length rest)
    (1 (if (atom (car rest))
           `(move-game-by-xy ,game (move-x ,(car rest)) (move-y ,(car rest)))
           (with-gensyms (move)
             `(let ((,move ,(car rest)))
                (move-game-by-xy ,game (move-x ,move) (move-y ,move))))))
    (2 `(move-game-by-xy ,game ,(car rest) ,(cadr rest)))
    (t (error "move-game takes 2 or 3 arguments (got ~D argument[s])" (length rest)))))

(defun reverse-game (game)
  (if (<= (get-game-depth game) 0) (return-from reverse-game nil))
  (let* ((record (pop-history-record (game-history game)))
	 (move (history-record-move record)))
    (set-to-board (game-board game) (car move) (cdr move) +empty+)
    (do-move-store (move (history-record-reverse-list record))
      (let ((x (move-x move))
	    (y (move-y move)))
	(set-to-board (game-board game) x y
		      (reverse-turn (get-piece (game-board game) x y)))))
    (setf (game-turn game) (history-record-turn record))
    game))

(defun reverse-game-to-depth (game depth)
  (if (<= (get-game-depth game) (max 0 depth))
      (return-from reverse-game-to-depth game))
  (reverse-game game)
  (reverse-game-to-depth game depth))

(defun get-game-depth (game)
  (history-record-store-count (game-history game)))

(defun make-moves (game)
  (make-moves-on-board (game-board game) (game-turn game) :store (game-move-store game)))

(defmacro do-in-move-reverse (game move &body body)
  (let ((result (gensym))
	(g-game (gensym))
	(g-move (gensym)))
    `(let ((,g-game ,game)
	   (,g-move ,move))
       (unless (check-move-valid
		(game-board ,g-game) (car ,g-move) (cdr ,g-move) (game-turn ,g-game))
	   (error (format nil "ERROR: An Invalid Move! (~A)" ,g-move)))
       (move-game ,g-game ,g-move)
       (let ((,result (progn ,@body)))
	 (reverse-game ,g-game)
	 ,result))))

(defun print-turn (turn)
  (cond ((eq turn +white+) (princ "White turn"))
	((eq turn +black+) (princ "Black turn"))
	(t (princ "Game Finished")))
  (fresh-line))

(defun get-game-result (game)
  (if (not (is-game-end game)) (return-from get-game-result +not-game-end+))
  (let ((white (count-piece (game-board game) +white+))
	(black (count-piece (game-board game) +black+)))
    (cond ((> white black) +white+)
	  ((< white black) +black+)
	  (t +empty+))))

(defun print-game (game &optional (prints-history nil))
  (print-board (game-board game))
  (print-turn (game-turn game))
  (format t "~%MOVE-> ")
  (do-move-store (move (make-moves game))
    (format t "~A " move))
  (format t "~%")
  (if prints-history
      (labels ((print-a-history (history)
		 (let ((lst nil))
		   (do-move-store (move (history-record-reverse-list history))
		     (setf lst (cons move lst)))
		   (format t "TURN: ~2D, Move: ~A, REVERSE-LIST: ~D~%"
			   (history-record-turn history)
			   (history-record-move history)
			   lst))))
	(fresh-line)
	(princ "+++++ history start +++++")
	(fresh-line)
	(do-history-record-store (record (game-history game))
	  (print-a-history record))
	(princ "+++++ history end +++++")))
  (fresh-line))
 
