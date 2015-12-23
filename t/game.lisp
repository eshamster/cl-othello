(in-package :cl-user)
(defpackage cl-othello-test.game
  (:use :cl
        :cl-othello.game
        :cl-othello.defines
        :cl-othello.move
        :cl-othello.move-store
        :cl-othello.board
        :cl-othello.history-record
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.game)

(plan 8)

(subtest
    "Test init-game"
  (let ((game (init-game)))
    (is (get-game-depth game) 0)
    (ok (not (reverse-game game)))))

(subtest
    "Test move-game"
  (let ((game (init-game)))
    (ok (not (move-game game -1 3)))
    (ok (not (move-game game 5 5)))
    (let* ((game (make-nth-test-game 2))
           (depth (get-game-depth game)))
      (assert (move-game game 4 5))
      (is (get-game-depth game) (1+ depth))))
  (let ((game (init-game)))
    (ok (move-game game (make-a-move 4 5))))
  (let ((game (init-game)))
    (ok (let ((move (make-a-move 4 5)))
          (move-game game move))))
  (subtest
      "Test macroexpand"
    (is-expand (move-game game 4 5) ($:move-game-by-xy game 4 5))
    (is-expand (move-game game move) ($:move-game-by-xy game (move-x move) (move-y move)))
    (is-expand (move-game game (make-a-move 4 5))
               (let (($$move (make-a-move 4 5)))
                 ($:move-game-by-xy game (move-x $$move) (move-y $$move)))))
  (subtest
      "Test error"
    (let ((game (init-game)))
      (is-error (move-game game) 'error)
      (is-error (move-game game 1 2 3) 'error))))

(subtest
    "Test is-game-same-phase"
  (ok (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 3)))
  (ok (is-game-same-phase (make-nth-test-game 0) (init-game)))
  (ok (not (is-game-same-phase (make-nth-test-game 3) (make-nth-test-game 4))))
  (ok (not (is-game-same-phase
            (move-game (make-nth-test-game 2) 4 5)
            (move-game (make-nth-test-game 2) 1 2)))))

(subtest
    "Test utils for test (make-nth-test-game)"
  (let ((game  (make-nth-test-game 3)))
    (is (get-game-depth game) 3)
    (let ((store (make-moves game)))
      (is (move-store-count store) 2)
      (ok (contains-move store 2 4))
      (ok (contains-move store 4 2)))))

(subtest
    "Test reverse-game"
  (let ((game (make-nth-test-game 5)))
    (reverse-game game)
    (reverse-game game)

    (ok (is-game-same-phase game (make-nth-test-game 3)))

    (ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 3)
                            (make-nth-test-game 3)))
    (ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 0)
                            (init-game)))
    (ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) 8)
                            (make-nth-test-game 5)))
    (ok (is-game-same-phase (reverse-game-to-depth (make-nth-test-game 5) -2)
                            (init-game)))))

(subtest
    "Test funcs about the game end"
  (ok (not (is-game-end (init-game))))
  (ok (not (is-game-end (make-nth-test-game 3))))
  (ok (is-game-end (make-nth-test-game 100)))

  (is (get-game-result (init-game)) +not-game-end+)
  (is (get-game-result (make-nth-test-game 3)) +not-game-end+)

  (is (get-game-result (make-nth-test-game 100)) +black+)
  (let ((num-cell (* +board-size+ +board-size+)))
    (labels ((test (white black expected)
               (let ((game (init-game)))
                 (dotimes (i num-cell)
                   (set-to-board (game-board game)
                                 (mod i +board-size+)
                                 (floor (/ i +board-size+))
                                 (if (< i white) +white+
                                     (if (< i (+ white black)) +black+ +empty+))))
                 (setf (game-turn game) +empty+)
                 (assert (is-game-end game))
                 (is (get-game-result game) expected))))
      (test 33 31 +white+)
      (test 31 25 +white+)
      (test 31 33 +black+)
      (test 25 31 +black+)
      (test 30 30 +empty+)
      (test 32 32 +empty+))))

(subtest
    "Test do-in-move-reverse"
  (let ((game (make-nth-test-game 3)))
    (is (do-in-move-reverse game (make-a-move 2 4)
          (let ((test-par 2))
            (* (get-game-depth game) test-par))) 8)
    (is (do-in-move-reverse game (make-a-move 2 4)
          (do-in-move-reverse game (make-a-move 5 5)
            (get-game-depth game)))
        5)
    (is-error (do-in-move-reverse game (make-a-move 5 5)
                (print 'empty))
              'simple-error)
    (is (get-game-depth game) 3)))

(subtest
    "Test print-game"
  (is-print (print-game (make-nth-test-game 4) t)
          "   01234567
|0 -------- |
|1 -------- |
|2 --XO---- |
|3 --XOO--- |
|4 --XXX--- |
|5 -------- |
|6 -------- |
|7 -------- |
White turn

MOVE-> (1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (2 . 5) (3 . 5) (4 . 5) (5 . 5) 
+++++ history start +++++
TURN: -1, Move: (2 . 4), REVERSE-LIST: ((3 . 4) (2 . 3))
TURN:  1, Move: (2 . 3), REVERSE-LIST: ((3 . 3))
TURN: -1, Move: (2 . 2), REVERSE-LIST: ((3 . 3))
TURN:  1, Move: (3 . 2), REVERSE-LIST: ((3 . 3))
+++++ history end +++++
"))

(finalize)
