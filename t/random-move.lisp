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
    (test 2 '(1/4 1/4 1/4 1/4))
    (test 5 '(1/7 1/7 1/7 1/7 1/7 1/7 1/7))
    (test 100 nil)))

(subtest
    "Test decide-move-by-random"
  (let* ((game (make-nth-test-game 2))
         (moves (make-moves game))
         (prob-store (make-prob-store)))
    (labels ((test-decision (rand-val answer-idx)
               (is ($:decide-move-by-random-policy game #'make-uniform-policy
                                                   rand-val prob-store)
                         (get-nth-move answer-idx moves))))
      (test-decision -2 0)
      (test-decision 0 0)
      (test-decision 0.1 0)
      (test-decision 0.25 0)
      (test-decision 0.26 1)
      (test-decision 1 3)
      (test-decision 2 3))))

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
      (is-type ($:add-to-prob-store store 1/7) 'prob-store)
      (let ((lst nil))
        ($:do-prob-store (prob store)
          (setf lst (cons prob lst)))
        (is lst '(1/7 0.1) :test #'equalp))
      
      (is-type ($:reset-prob-store store) 'prob-store)
      (is ($:prob-store-count store) 0))
    
    (subtest
        "Test get-nth-prob"
      ($:reset-prob-store store)
      ($:add-to-prob-store store 0.5)
      (ok (not ($:get-nth-prob nil store)))
      (ok (not ($:get-nth-prob -1 store)))
      (ok (not ($:get-nth-prob 1 store)))
      (is ($:get-nth-prob 0 store) 0.5))))

(finalize)
