(in-package :cl-user)
(defpackage cl-othello-test.mc
  (:use :cl
        :cl-othello.mc
        :cl-othello.board
        :cl-othello.game
        :cl-othello.random-move
        :cl-othello.defines
        :cl-othello.move-store
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.mc)

(defun sim-to-game-end (game fn-make-policy prob-store)
  (cl-othello.mc::sim-to-game-end game fn-make-policy prob-store))
(defmacro mc-node-num (node)
  `(cl-othello.mc::mc-node-num ,node))
(defmacro mc-node-sum (node)
  `(cl-othello.mc::mc-node-sum ,node))

(plan 7)

(subtest
    "Test calc-ucb"
  (ok (= (calc-ucb 2  0 10) +ucb-max+))
  (ok (= (calc-ucb 2 -2 10) +ucb-max+))
  (ok (= (calc-ucb 2 10  0) +ucb-max+))
  (ok (= (calc-ucb 2 10 -2) +ucb-max+))
  (ok (= (calc-ucb 2 -2 -2) +ucb-max+))
  (ok (< (calc-ucb -2 10 10) +ucb-max+))

  (ok (< (calc-ucb 2 2 2)
               (calc-ucb 4 2 2)))
  (ok (< (calc-ucb 2 2 2)
               (calc-ucb 2 1 2)))
  (ok (< (calc-ucb 2 2 2)
               (calc-ucb 2 2 4)))
  
  (is (calc-ucb -2 10 10 :turn +white+)
            (calc-ucb 2 10 10 :turn +black+))
  (is (calc-ucb 2 10 10 :turn +white+)
            (calc-ucb -2 10 10 :turn +black+))
  (is (calc-ucb -2 0 10 :turn +white+)
            (calc-ucb -2 0 10 :turn +black+))

  (within (calc-ucb 2 2 8) 2.4420 0.0001)
  (within (calc-ucb -2 2 8) 0.4420 0.0001))


(defparameter *end-status-list* `(,+white+ ,+black+ ,+empty+))
(defun prove-game-is-end (game)
  (prove-in (get-game-result game) *end-status-list*))
(subtest
    "Test sim-to-game-end"
  (let ((prob-store (make-prob-store)))
    (prove-game-is-end (sim-to-game-end (init-game) #'make-uniform-policy prob-store))
    (prove-game-is-end (sim-to-game-end (make-nth-test-game 3) #'make-uniform-policy prob-store))
    (prove-game-is-end (sim-to-game-end (make-nth-test-game 100)  #'make-uniform-policy prob-store))))

(subtest
    "Test mc-simulate-once"
  (defun prove-mc-sim-once (start repeat)
    (let ((game (make-nth-test-game start)))
      (dotimes (x repeat)
        (prove-in (mc-simulate-once game #'make-uniform-policy) *end-status-list*))
      (ok (is-game-same-phase game (make-nth-test-game start)))))
  (prove-mc-sim-once 3 5)
  (prove-mc-sim-once 4 5))

(subtest
    "Test init-mc-nodes"
  (is (init-mc-nodes (make-nth-test-game 3))
      '(#S(CL-OTHELLO.MC::MC-NODE :MOVE (4 . 2) :SUM 0 :NUM 0)
        #S(CL-OTHELLO.MC::MC-NODE :MOVE (2 . 4) :SUM 0 :NUM 0)) :test #'equalp)
  (defun prove-mc-node-len (start)
    (let ((game (make-nth-test-game start)))
      (is (length (init-mc-nodes game)) (move-store-count (make-moves game)))))
  (prove-mc-node-len 5)
  (prove-mc-node-len 34)
  (prove-mc-node-len 100))

(subtest
    "Test select-mc-node-by-ucb"
  (defun prove-all-node-selected-in-ucb (start)
    (let* ((game (make-nth-test-game start))
           (mc-node (init-mc-nodes game))
           (len (length mc-node)))
      (when (<= len 0) (return-from prove-all-node-selected-in-ucb t))
      (dotimes (x len)
        (incf (mc-node-num (select-mc-node-by-ucb mc-node x)) 1))
      (ok (every #'(lambda(node) (= (mc-node-num node) 1)) mc-node))))
  (prove-all-node-selected-in-ucb 2)
  (prove-all-node-selected-in-ucb 3)

  (pass "I have no plan to validate the selection by select-mc-node-by-ucb"))


(defmacro t-nth-mc-num (n nodes)
  `(mc-node-num (nth ,n ,nodes)))
(defmacro t-nth-mc-sum (n nodes)
  `(mc-node-sum (nth ,n ,nodes)))
(subtest
    "Test select-mc-node-by-ave"
  (let ((test-mc-nodes (init-mc-nodes (make-nth-test-game 57))))
    (assert (= (length test-mc-nodes) 3))
    
    (setf (t-nth-mc-sum 0 test-mc-nodes) -2)
    (setf (t-nth-mc-num 0 test-mc-nodes) 5)
    (setf (t-nth-mc-sum 2 test-mc-nodes) -7)
    (setf (t-nth-mc-num 2 test-mc-nodes) 10)
    (is (mc-node-sum (select-mc-node-by-ave test-mc-nodes)) -2)
    
    (setf (t-nth-mc-sum 2 test-mc-nodes) 3)
    (is (mc-node-sum (select-mc-node-by-ave test-mc-nodes)) 3)))
  
(subtest
    "Test mc-simulate"
  (ok (not (mc-simulate (make-nth-test-game 100) #'make-uniform-policy 5)))

  (defun prove-mc-simulate (start)
    (let* ((game (make-nth-test-game start))
           (move (mc-simulate game #'make-uniform-policy 20)))
      (ok (is-game-same-phase game (make-nth-test-game start)))
      (ok (check-move-valid (game-board game) (car move) (cdr move) (game-turn game)))))

  (prove-mc-simulate 13)
  (prove-mc-simulate 24))

(finalize)
