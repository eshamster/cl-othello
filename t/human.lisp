(in-package :cl-user)
(defpackage cl-othello-test.human
  (:use :cl
        :cl-othello.human
        :cl-othello.board
        :cl-othello.move-store
        :cl-othello.game
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.human)

(plan 1)

(subtest
    "Test eval-play-command"
  (labels ((test-ok (com expected &optional game)
	     (when (null game) (setf game (make-nth-test-game 6)))
	     (let ((result (eval-play-command game com)))
	       (if expected (ok result) (ok (not result))))))
    (subtest
	"Test print command"
      (test-ok '("print") nil))
    (subtest
	"Test move command"
      (test-ok '("move") nil)
      (test-ok '("move" 1) nil)
      (test-ok '("move" -1 1) nil)
      (test-ok '("move" "ab" 1) nil)
      (let* ((game (make-nth-test-game 11))
	     (move (get-nth-move 0 (make-moves game))))
	(test-ok (list "move" (car move) (cdr move)) t game)
	(is (get-game-depth game) 12)
	; We doesn't elaborate the search for an invalid move
	(assert (not (check-move-valid (game-board game) 0 0 (game-turn game))))
	(test-ok '("move" 0 0) nil)))
    (subtest
	"Test reverse command"
      (let ((game (make-nth-test-game 6)))
	(test-ok '("reverse") t game)
	(is (get-game-depth game) 5))
      (test-ok '("reverse") nil (init-game)))
    (subtest
	"Test not-defined command"
      (test-ok '("not-defined" com) nil))))

(finalize)
