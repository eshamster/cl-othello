#|
  This file is a part of cl-othello project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-othello-asd
  (:use :cl :asdf))
(in-package :cl-othello-asd)

(defsystem cl-othello
  :version "0.1"
  :author "eshamster"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "defines")
                 (:file "utils")
                 (:file "tree")
                 (:file "move")
                 (:file "move-store")
                 (:file "history-record")
                 (:file "board")
                 (:file "game")
                 (:file "eval-board")
                 (:file "cl-othello"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-othello-test))))
