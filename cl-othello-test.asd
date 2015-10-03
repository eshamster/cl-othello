#|
  This file is a part of cl-othello project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-othello-test-asd
  (:use :cl :asdf))
(in-package :cl-othello-test-asd)

(defsystem cl-othello-test
  :author "eshamster"
  :license ""
  :depends-on (:cl-othello
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-othello"))))
  :description "Test system for cl-othello"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
