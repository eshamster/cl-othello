(in-package :cl-user)
(defpackage cl-othello.tree
  (:use :cl)
  (:export :add-child
           :push-child
           :do-children
           :has-children
           :get-num-children
           :get-node-value
           :get-children
           :get-rest-children
           :get-nth-child
           :get-tree-size
           :get-tree-depth
           :select-max-child
           :select-max-node
           :print-tree))
(in-package :cl-othello.tree)

(defmacro get-node-value (node)
  `(car ,node))

(defmacro get-children (node)
  `(cdr ,node))

(defmacro get-rest-children (children)
  `(cdr ,children))

(defmacro get-nth-child (n node)
  `(nth ,n (get-children ,node)))

(defmacro do-children ((var tree) &body body)
  `(dolist (,var (get-children ,tree))
     ,@body))

(defun has-children (node)
  (get-children node))

(defun add-or-push-child (parent children destructive)
  (labels ((add-or-push (lst1 lst2)
	     (if destructive (nconc lst1 lst2) (append lst1 lst2))))
    (add-or-push
     parent
     (mapcar (lambda (child)
	       (if (and (not (null child)) (listp child) (listp (cdr child)))
		   `,child
		   `(,child)))
             children))))
 
; not destructive 
(defun add-child (parent &rest children)
  (add-or-push-child parent children nil))

; destructive
(defun push-child (parent &rest children)
  (add-or-push-child parent children t))

(defun get-num-children (node)
  (- (length node) 1))

(defun get-tree-size (tree)
  (labels ((f (node)
 	     (if node 
                 (let ((sum 1))
                   (do-children (child node)
                     (incf sum (f child)))
                   sum)
                 0)))
    (f tree)))

(defun get-tree-depth (tree)
  (labels ((f (node)
	     (if node
                 (let ((depth 0))
                   (do-children (child node)
                     (setf depth (max depth (+ (f child) 1))))
                   depth)
                 0)))
    (f tree)))

(defun print-tree (tree &key (max-depth -1) (fn-proc-value #'(lambda (v) v)))
  (labels ((print-str-seq (s n)
	     (dotimes (i n) (princ s)))
	   (f (node depth)
	     (when (and node
                        (or (< max-depth 0) (<= depth max-depth))) 
               (print-str-seq "| " depth)
               (princ (funcall fn-proc-value (get-node-value node)))
               (fresh-line)
               (do-children (child node)
                 (f child (+ depth 1))))))
    (f tree 0)))
	     

(defun select-max-child (fn-calc-value parent)
  (select-max-node #'(lambda (node)
                       (funcall fn-calc-value (get-node-value node)))
                   (get-children parent)))

(defun select-max-node (fn-calc-value nodes)
  (let* ((max-node (car nodes))
	 (max-value (funcall fn-calc-value max-node)))
    (dolist (node (cdr nodes))
      (let ((value (funcall fn-calc-value node)))
	(when (< max-value value)
          (setf max-node node)
          (setf max-value value))))
    max-node))
  
