(in-package :cl-user)
(defpackage cl-othello.history-record
  (:use :cl)
  (:export :history-record
           :history-record-turn
           :history-record-move
           :history-record-reverse-list
           :history-record-store
           :history-record-store-count
           :history-record-store-records
           :init-history-record-store
           :push-history-record
           :pop-history-record
           :do-history-record-store)
  (:import-from :cl-othello.defines
                :+empty+
                :+board-size+)
  (:import-from :cl-othello.move
                :make-a-move)
  (:import-from :cl-othello.move-store
                :move-store
                :init-move-store
                :reset-move-store))
(in-package :cl-othello.history-record)

(defparameter *max-reverse-list* (* (- +board-size+ 2) 3) )
(defparameter *max-history-record* (- (* +board-size+ +board-size+) 4))

(defstruct history-record
  (turn +empty+)
  (move (make-a-move -1 -1))
  (reverse-list (init-move-store :num-moves *max-reverse-list*)))

(defun init-history-record ()
  (make-history-record))

(defstruct history-record-store
  (count 0)
  (records (make-array *max-history-record*
		       :initial-contents (let ((lst nil))
					   (dotimes (i *max-history-record*)
					     (setf lst (cons (make-history-record) lst)))
					   lst))))

(defun init-history-record-store ()
  (make-history-record-store))

(defun push-history-record (store fn-process-record)
  (let ((record (aref (history-record-store-records store)
		      (history-record-store-count store))))
    (reset-move-store (history-record-reverse-list record))
    (when (funcall fn-process-record record)
      (incf (history-record-store-count store)))))

(defun pop-history-record (store)
  (if (<= (history-record-store-count store) 0)
      (return-from pop-history-record nil))
  (decf (history-record-store-count store))
  (aref (history-record-store-records store)
	(history-record-store-count store)))

(defmacro do-history-record-store (name<>store &body body)
  (let ((i (gensym))
	(len (gensym))
	(store (gensym)))
    `(let* ((,store ,(cadr name<>store))
	    (,len (history-record-store-count ,store)))
       (dotimes (,i ,len)
	 (let ((,(car name<>store) (aref (history-record-store-records ,store) (1- (- ,len ,i)))))
	   ,@body)))))
