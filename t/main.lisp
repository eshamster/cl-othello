(in-package :cl-user)
(defpackage cl-othello-test.main
  (:use :cl
        :cl-othello.main
        :cl-othello.game
        :cl-othello.player
        :cl-othello-test.test-utils
        :prove))
(in-package :cl-othello-test.main)

(defun game-p (obj)
  (cl-othello.game::game-p obj))

(plan nil)

(subtest
    "Test game-loop"
  (labels ((test (game)
             (let ((ret
                    ($:game-loop game
                                 (player-make-mover (construct-player "test-white" :random))
                                 (player-make-mover (construct-player "test-black" :random)))))
               (ok (game-p ret))
               (ok (is-game-end game)))))
    (test (init-game))
    (test (make-nth-test-game 15))))

(defun t-make-player-list ()
  (list (construct-player "test1" :mc)
        (construct-player "test2" :random)
        (construct-player "test3" :uct)))

(subtest
    "Test com-start-game"
  (let* ((plyr-list (t-make-player-list))
         (game (init-game))
         (depth (get-game-depth game)))
    (setf plyr-list (cons (construct-player "test-csg" :random)
                          plyr-list))
    (subtest
        "Test make-mover-by-name-sym"
      (ok (null ($:make-mover-by-name-sym plyr-list :not-exist)))
      (ok (null ($:make-mover-by-name-sym nil :test1)))
    
      (ok (not (null
                (funcall ($:make-mover-by-name-sym plyr-list 'test2)
                         game))))
      (is (get-game-depth game) (1+ depth)))

      (subtest
          "Test com-start-game"
        (macrolet ((test (com-list suc &body body)
                     `(multiple-value-bind (game result)
                          ($:com-start-game (init-game) ,com-list plyr-list)
                        (is result ,suc)
                        (ok (game-p game))
                        ,@body)))
          (test nil nil)
          (test '(test1) nil)
          (test '(test1 not-exist) nil)
          (test '(test2 test-csg) t
                (ok (is-game-end game)))))))

(subtest
    "Test commands about player"
  (subtest
      "Test remove"
    (let ((lst (t-make-player-list)))
      (multiple-value-bind (result test) ($:com-remove-player "not-found" lst)
        (ok (not test))
        (is (length result) (length lst)))
      (multiple-value-bind (result test) ($:com-remove-player "" lst)
        (ok (not test))
        (is (length result) (length lst)))
      (multiple-value-bind (result test) ($:com-remove-player "test2" lst)
        (ok test)
        (is (length result) (1- (length lst)))
        (ok (not (find-if #'(lambda (plyr)
                                    (equalp (player-name plyr) "test2"))
                                result))))))
      
  (subtest
      "Test save and load"
    (let ((save-list (t-make-player-list))
          (load-list nil)
          (file-name (merge-pathnames "t/temp-players"
                                      (directory-namestring
                                       (asdf:system-source-file
                                        (asdf:find-system :cl-othello))))))
      (if (probe-file file-name)
          (delete-file file-name))
      (assert (not (probe-file file-name)))

      (ok (null ($:com-load-player file-name)))

      (labels ((get-result ()
                 ($:com-save-player file-name save-list)
                 (setf load-list ($:com-load-player file-name))
                 
                 (dolist (plyr save-list)
                   (let ((found (find-if #'(lambda (target)
                                             (equalp (player-name plyr)
                                                     (player-name target)))
                                         load-list)))
                     (when (or (null found)
                               (not (eq (type-of plyr) (type-of found))))
                         (return-from get-result nil))))
                 t))
        (ok (get-result))
        ; Test overwriting
        (setf save-list ($:com-remove-player "test2" save-list))
        (assert (= (length save-list) 2))
        (ok (get-result)))))

  (subtest
      "Test show"
    (let ((lst (t-make-player-list))
          (out1 (make-string-output-stream))
          (out2 (make-string-output-stream)))
      ($:com-show-player lst out1)
      (setf lst (cons (construct-player "test4" :human) lst))
      ($:com-show-player lst out2)
      (ok (< (length (get-output-stream-string out1))
                   (length (get-output-stream-string out2))))))

  (subtest
      "Test init-player"
    (let ((plyr ($:com-init-player
                 (make-string-input-stream
                  (format nil "~%  ~%  test  ~%~%  ~%non-player~%  mc ")))))
      (is (player-name plyr) "test")
      (is (type-of plyr) 'cl-othello.player::mc-player)))

  (subtest
      "Test add-player"
    (let* ((plyr-list (t-make-player-list))
           (first-len (length plyr-list)))
      (labels ((test (test-title plyr format-str expected-len expected-match)
                 (subtest
                     test-title
                   (let ((name (player-name plyr))
                         (new-lst ($:com-add-player plyr plyr-list
                                                  (make-string-input-stream
                                                   (format nil format-str)))))
                     (is (length new-lst) expected-len)
                     (ok (not (null (find-player-by-name name new-lst))))
                     (is (equalp (find-player-by-name name new-lst)
                                       (find-player-by-name name plyr-list))
                               expected-match))
                   (is (length plyr-list) first-len))))
        (test "Add new player"
              (construct-player "new-name" :human)
              "" (1+ first-len) nil)
        (test "Overwrite an existing player"
              (construct-player "test2" :human)
              "y" first-len nil)
        (test "Don't overwrite an existeng player"
              (construct-player "test2" :human)
              "n" first-len t))))

  (subtest
      "Test modify-player (test only default value)"
    (labels ((test (kind)
               (let ((plyr (construct-player "test" kind))
                     (str ""))
                 (maphash #'(lambda (k v)
                              (declare (ignore k v))
                              (setf str (concatenate 'string "~%" str)))
                          (player-params plyr))
                 (ok
                  ($:com-modify-player plyr (make-string-input-stream
                                           (format nil str)))))))
      (dolist (kind '(:human :random :mc :uct))
        (test kind))))
  (subtest
      "Test com-player (not comprehensive)"
    (labels ((test-success (com-list expected)
               (let ((plyr-list (t-make-player-list)))
                 (multiple-value-bind (lst suc)
                     ($:com-player com-list plyr-list)
                   (declare (ignore lst))
                   (is suc expected)))))
      ; add, remove are not tested
      (test-success '(show) t)
      (test-success '(help) t)
      (test-success nil t)
      (test-success '(not-defined) nil))))

(subtest
    "Test the main function"
  (let ((test-file "TMP_PLAYER_INFO"))
    (labels ((test (com-str)
               (format t "~%----------<start main>---------~%")
               (main :plyr-file test-file
                     :in (make-string-input-stream
                          (format nil
                                  (concatenate 'string com-str "~%quit~%"))))))
      (if (probe-file test-file)
          (delete-file test-file))
      (pass "Because the auto test is difficult, we only print some information")
      
      (subtest
          "Test empty, help and a not-existed command"
        (test "")
        (test "help~%not-exist"))
      ($:com-save-player test-file (t-make-player-list))
      (subtest
          "Test player save and load"
        (test "player show~%player remove test3~%player show")
        (test "player show"))
      (subtest
          "Test playe game and init game"
        (test "show~%start test2 test2~%show~%init")))))

(finalize)
