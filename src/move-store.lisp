(in-package :cl-user)
(defpackage cl-othello.move-store
  (:use :cl)
  (:export :move-store
           :init-move-store
           :init-move-store-stack
           :add-to-move-store
           :move-store-count
           :reset-move-store
           :do-move-store
           :get-nth-move
           :clone-move-store
           :with-cloned-move-store
           :+max-move-store+
           :contains-move
           :mapcar-move-store)
  (:import-from :cl-othello.utils
                :make-list-of-same-value)
  (:import-from :cl-othello.move
                :make-a-move
                :set-to-move
                :move-x
                :move-y)
  (:import-from :cl-othello.defines
                :+board-size+)
  (:import-from :alexandria
                :once-only))
(in-package :cl-othello.move-store)

(defconstant +max-move-store+ (- (* +board-size+ +board-size+) 4))

(defstruct move-store
  count
  moves)

(defun init-move-store (&key (num-moves +max-move-store+))
  (make-move-store :count 0
		   :moves (make-array num-moves
				      :initial-contents (make-list-of-same-value
                                                         num-moves
                                                         (make-a-move 0 0)))))

(defun reset-move-store (store)
  (setf (move-store-count store) 0)
  store)

; skip the range check
(defun add-to-move-store (store x y)
  (set-to-move (aref (move-store-moves store) (move-store-count store)) x y)
  (incf (move-store-count store))
  store)

(defmacro do-move-store ((move store) &body body)
  (let ((i (gensym)))
    (once-only (store)
      `(dotimes (,i (move-store-count ,store))
        (let ((,move (aref (move-store-moves ,store) ,i)))
          ,@body)))))

(defun contains-move (store x y)
  (do-move-store (move store)
    (when (and (eq x (move-x move))
	       (eq y (move-y move)))
      (return-from contains-move t)))
  nil)

(defun copy-to-move-store (dst src)
  (reset-move-store dst)
  (do-move-store (move src)
    (add-to-move-store dst (car move) (cdr move)))
  dst)
(defun clone-move-store (src)
  (let ((dst (init-move-store)))
    (copy-to-move-store dst src)
    dst))

(defstruct move-store-stack
  (unused-store (make-array 0 :fill-pointer 0 :adjustable t))
  (used-store (make-array 0 :fill-pointer 0 :adjustable t)))

(defun init-move-store-stack ()
  (make-move-store-stack))

(macrolet ((unused (stack) `(move-store-stack-unused-store ,stack))
           (used (stack) `(move-store-stack-used-store ,stack)))

  (defun num-allocated-move-store-stack (stack)
    (+ (length (used stack))
       (length (unused stack))))

  (defun num-reserved-move-store-stack (stack)
    (length (used stack)))
  
  (defun reserve-move-store-from-stack (stack)
    (labels ((push-and-return (store stack)
               (vector-push-extend store stack)
               store))
      (if (= (length (unused stack)) 0)
          (push-and-return (init-move-store) (used stack))
          (push-and-return (vector-pop (unused stack)) (used stack)))))
  
  (defun free-move-store-to-stack (stack)
    (vector-push-extend (vector-pop (used stack))
                        (unused stack))))

(defmacro with-cloned-move-store (stack (cloned store) &body body)
  `(let ((,cloned (reserve-move-store-from-stack ,stack)))
     (copy-to-move-store ,cloned ,store)
     ,@body
     (free-move-store-to-stack ,stack)))

(defmacro get-nth-move (n store)
  `(cond ((null ,store) nil)
	 ((< ,n 0) nil)
	 ((>= ,n (move-store-count ,store)) nil)
	 (t (aref (move-store-moves ,store) ,n))))

(defun mapcar-move-store (fn store)
  (let ((lst nil))
    (do-move-store (move store)
      (setf lst (cons (funcall fn move) lst)))
    (reverse lst)))
