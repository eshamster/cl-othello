(in-package :cl-user)
(defpackage cl-othello-test.player
  (:use :cl
        :cl-othello.player
        :cl-othello.move-store
        :cl-othello.game
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.player)

(plan 5)

(subtest
    "Test fit-type-to"
  (subtest
      "number"
    (is ($:fit-type-to 12 '111) 111)
    (is-error ($:fit-type-to 12 'not-number) 'simple-error))
  (subtest
      "string"
    (is ($:fit-type-to "str" 'test) "TEST"))
  (subtest
      "others"
    (is ($:fit-type-to 'abc 'test) 'test)
    (is-error ($:fit-type-to 'abc "test") 'error)))

(defparameter *all-player-kind* '(:human :minimax :random :mc :uct))

(subtest
    "Test consturct-player"
  (subtest
      "Test for existing player"
    (dolist (kind *all-player-kind*)
      (ok (subtypep (type-of (construct-player "test" kind)) 'player))))
  (subtest
      "Test for not-existing player"
    (is (construct-player "test" :not-exist) nil)))

(subtest
    "Test serialize and deserialize player"
  (labels ((test (kind)
             (let* ((name "test-name")
                    (before-player (construct-player name kind))
                    (before-player-dump (player-serialize before-player))
                    (after-player (player-deserialize before-player-dump)))
               (subtest
                   (format nil "Test ~D" kind)
                 (ok (stringp before-player-dump))
                 (is before-player-dump (player-serialize after-player) :test #'equalp)
                 (maphash #'(lambda (k v)
                              (is (gethash k (player-params after-player)) v :test #'equalp))
                          (player-params before-player))))))
    (dolist (kind *all-player-kind*)
      (print kind)
      (test kind))))

(subtest
    "Test player-make-mover"
  (let ((start-depth 5))
    (labels ((test (kind &optional (opt nil))
               (let* ((name "test-name")
                      (plyr (construct-player name kind))
                      (mover (player-make-mover plyr))
                      (game (make-nth-test-game start-depth)))
                 (if (null opt)
                     (ok (funcall mover game))
                     (ok (funcall mover game opt)))
                 (is (get-game-depth game) (1+ start-depth)))))
      (let ((move (get-nth-move 0 (make-moves (make-nth-test-game start-depth)))))
        (test :human (make-string-input-stream
                       (format nil "print~%move ~D ~D" (car move) (cdr move)))))
      (dolist (kind (remove :human *all-player-kind* :test #'equal))
        (test kind)))))

(subtest
    "Test find-player-by-name"
  (let* ((found (construct-player "test" :mc))
         (lst (list (construct-player "abcd" :human)
                    found
                    (construct-player "xyz" :uct))))
    (is (find-player-by-name "test" lst) found :test #'equalp)
    (isnt (find-player-by-name "abcd" lst) found :test #'equalp)
    (ok (null (find-player-by-name "not-found" lst)))))

(finalize)
