(in-package :cl-user)
(defpackage cl-othello.random-move
  (:use :cl)
  (:export :make-uniform-policy
           :move-by-random-policy
           :prob-store
           :make-prob-store)
  (:import-from :cl-othello.move-store
                :get-nth-move
                :move-store-count
                :do-move-store
                :+max-move-store+)
  (:import-from :cl-othello.game
                :game
                :move-game
                :make-moves
                :is-game-end))
(in-package :cl-othello.random-move)

(defstruct prob-store
  (count 0 :type fixnum)
  (probs (make-array +max-move-store+ :element-type 'single-float)))

(defun reset-prob-store (store)
  (setf (prob-store-count store) 0)
  store)

(defun add-to-prob-store (store prob)
  (setf (aref (prob-store-probs store) (prob-store-count store)) prob)
  (incf (prob-store-count store))
  store)

(defmacro do-prob-store ((name store) &body body)
  (let ((i (gensym))
        (g-store (gensym)))
    `(let ((,g-store ,store))
       (dotimes (,i (prob-store-count ,g-store))
         (let ((,name (aref (prob-store-probs ,g-store) ,i)))
           ,@body)))))

(defmacro get-nth-prob (n store)
  `(if (and (>= ,n 0)
            (< ,n (prob-store-count ,store))) 
       (the single-float (aref (prob-store-probs ,store) ,n))
       (error "Out of the range of prob-store")))

; --------------------- ;

(defun make-uniform-policy (game move-store prob-store)
  (declare (ignore game))
  (reset-prob-store prob-store)
  (let ((len (move-store-count move-store)))
    (dotimes (i len)
      (add-to-prob-store prob-store (float (/ 1 len)))))
  prob-store)

(defun decide-according-to-prob (store &optional (rand-val (random 1.0)) (sum (get-nth-prob 0 store)) (count 0))
  (declare (optimize (speed 3) (safety 2))
           (fixnum count)
           (single-float rand-val sum))
  (if (>= count (prob-store-count store))
      (the fixnum (1- count))
      (let ((prob (get-nth-prob count store)))
        (declare (real prob))
        (cond ((>= sum rand-val) count)
              (t (decide-according-to-prob store rand-val (+ sum prob) (1+ count)))))))

(defun decide-move-by-random-policy (game fn-make-policy prob-store)
  (unless (is-game-end game)
    (let ((move-store (make-moves game)))
      (funcall fn-make-policy game move-store prob-store)
      (get-nth-move (decide-according-to-prob prob-store) move-store))))

(defun move-by-random-policy (game fn-make-policy &key (prob-store (make-prob-store)))
  (move-game game
             (decide-move-by-random-policy game fn-make-policy prob-store)))

