(in-package :cl-user)
(defpackage cl-othello-test.random-move
  (:use :cl
        :cl-othello.random-move
        :cl-othello.move-store
        :cl-othello.game
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.random-move)

(plan 4)

(subtest
    "Test make-uniform-policy"
  (labels ((test (depth expected)
             (let* ((game (make-nth-test-game depth))
                    (move-store (make-moves game))
                    (prob-store (make-prob-store))
                    (lst nil))
               (make-uniform-policy game move-store prob-store)
               ($:do-prob-store (prob prob-store)
                 (setf lst (cons prob lst)))
               (setf lst (reverse lst))
               (is lst expected :test #'equalp))))
    (test 2 '(0.25 0.25 0.25 0.25))
    (test 100 nil)))

(subtest
    "Test decide-according-to-prob"
  (let ((store (make-prob-store)))
    (dotimes (i 4)
      ($:add-to-prob-store store 0.25))
    (labels ((test-decision (rand-val answer-idx)
               (is ($:decide-according-to-prob store rand-val)
                   answer-idx)))
      (test-decision -2.0 0)
      (test-decision 0.0 0)
      (test-decision 0.1 0)
      (test-decision 0.25 0)
      (test-decision 0.26 1)
      (test-decision 1.0 3)
      (test-decision 2.0 3))))

(subtest
    "Test move-by-random-policy"
  (let ((game (init-game)))
    (dotimes (n 10)
      (move-by-random-policy game #'make-uniform-policy))
    (is (get-game-depth game) 10)))

(subtest
    "Test prob-store"
  (let ((store (make-prob-store)))
    (subtest
        "Test initialization"
      (is ($:prob-store-count store) 0))
    
    (subtest
        "Test add ,reset and loop"
      (is-type ($:add-to-prob-store store 0.1) 'prob-store)
      (is-type ($:add-to-prob-store store 0.3) 'prob-store)
      (let ((lst nil))
        ($:do-prob-store (prob store)
          (setf lst (cons prob lst)))
        (is lst '(0.3 0.1) :test #'equalp))
      
      (is-type ($:reset-prob-store store) 'prob-store)
      (is ($:prob-store-count store) 0))
    
    (subtest
        "Test get-nth-prob"
      ($:reset-prob-store store)
      ($:add-to-prob-store store 0.5)
      ($:add-to-prob-store store 0.5)
      (is-error ($:get-nth-prob -1 store) 'simple-error)
      (is-error ($:get-nth-prob 2 store) 'simple-error)
      (is ($:get-nth-prob 0 store) 0.5))))

(finalize)
