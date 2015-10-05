(in-package :cl-user)
(defpackage cl-othello-test.eval-board
  (:use :cl
        :cl-othello.eval-board
        :cl-othello.defines
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.eval-board)

(defparameter *test-eval-param* (make-hash-table))
(setf (gethash :corner *test-eval-param*) 400)
(setf (gethash :num-move *test-eval-param*) 100)

(plan 2)

(subtest
    "Test eval without corner"
  (is (eval-game-static (make-nth-test-game 3) +white+) -2)
  (is (eval-game-static (make-nth-test-game 3) +black+) 2)

  (is (eval-game-static (make-nth-test-game 4) +white+) 9)
  (is (eval-game-static (make-nth-test-game 4) +black+) -9))

(subtest
    "Test eval including corner"
  (is (eval-game-static (make-nth-test-game 55) +white+) -1)
  (is (eval-game-static (make-nth-test-game 55) +black+) 1)
  (is (eval-game-static (make-nth-test-game 55) +white+
                        *test-eval-param*) -100))

(finalize)
