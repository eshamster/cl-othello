(in-package :cl-user)
(defpackage cl-othello.utils
  (:use :cl)
  (:export :string-to-list
           :stream-to-list
           :to-string
           :concat-symbol
           :push-without-dup
           :read-line-while
           :aif-second-true
           :make-list-of-same-value
           :it)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-othello.utils)

(defun string-to-list (line)
  (let ((result nil))
    (with-input-from-string (s line)
      (labels ((add-to-list ()
		 (let ((value (read s nil)))
		   (if (null value) (return-from add-to-list))
		   (setf result (cons value result))
		   (add-to-list))))
	(add-to-list)))
    (reverse result)))

(defun stream-to-list (&optional (stream *standard-input*))
  (string-to-list (read-line stream)))

(defgeneric to-string (target))
(defmethod to-string (target)
  (format nil "~A" target))
(defmethod to-string ((target symbol))
  (symbol-name-with-package target))
(defmethod to-string ((target function))
  (error 'simple-error))

(defun concat-symbol (&rest symbols)
  (if (= (length symbols) 0)
      (return-from concat-symbol nil))
  (let ((str ""))
    (dolist (x symbols)
      (setf str (concatenate 'string str (symbol-name x))))
    (intern str)))

(defun symbol-name-with-package (symbol)
  (format nil "~A:~A"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defun push-without-dup (target lst fn-equal)
  (if (null target)
      (return-from push-without-dup))
  (labels ((f (item old-lst new-lst)
	     (if (null (car old-lst))
		 (return-from f 
		   (if (null item)
		       new-lst
		       (cons item new-lst))))
	     (if (or (null item)
		     (not (funcall fn-equal item (car old-lst))))
		 (setf new-lst (cons (car old-lst) new-lst))
		 (progn (setf new-lst (cons item new-lst)) (setf item nil)))
	     (f item (cdr old-lst) new-lst)))
    (reverse (f target lst nil))))

(defun read-line-while (description fn-loop-cond &optional (stream *standard-input*))
  (labels ((my-read-line ()
	     (string-trim " " (read-line stream)))
	   (print-prefix ()
	     (format t "~D> " description)))
    (print-prefix)
    (do ((str (my-read-line) (my-read-line)))
	((not (funcall fn-loop-cond str)) str)
      (print-prefix))))

(defmacro aif-second-true (test if-ex else-ex)
  (let ((second (gensym)))
    `(multiple-value-bind (it ,second) ,test
       (if ,second ,if-ex ,else-ex))))

(defmacro make-list-of-same-value (length value)
  "Make a list by repeating 'value' in the 'length' times. The 'value' is intentionally evaluated in multiple times."
  (with-gensyms (i lst)
    `(let ((,lst nil))
       (dotimes (,i ,length)
         (setf ,lst (cons ,value ,lst)))
       ,lst)))
