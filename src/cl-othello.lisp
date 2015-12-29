(in-package :cl-user)
(defpackage cl-othello
  ;; TODO: Decide what should we export
  (:export :main)
  (:import-from :cl-othello.main
                :main))
