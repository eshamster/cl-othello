(in-package :cl-user)
(defpackage cl-othello-test.defines
  (:use :cl
        :cl-othello.defines
        :prove))
(in-package :cl-othello-test.defines)

(plan 8)

(isnt +white+ +black+)

(subtest
    "Test is-empty"
  (ok (is-empty nil))
  (ok (is-empty 0))
  (ok (not (is-empty +white+)))
  (ok (not (is-empty +black+))))

(subtest
    "Test is-reverse"
  (ok (is-reverse +white+ +black+))
  (ok (is-reverse +black+ +white+))
  (ok (not (is-reverse +white+ +white+)))
  (ok (not (is-reverse +black+ +black+)))
  (ok (not (is-reverse +black+ nil)))
  (ok (not (is-reverse nil +black+)))
  (ok (not (is-reverse nil nil))))

(subtest
    "Test reverse-turn"
  (is (reverse-turn +white+) +black+)
  (is (reverse-turn +black+) +white+))

(subtest
    "Test is-up-dir"
  (ok (is-up-dir +dir-up+))
  (ok (is-up-dir +dir-right-up+))
  (ok (is-up-dir +dir-left-up+))
  (ok (not (is-up-dir +dir-left-down+))))

(subtest
    "Test is-down-dir"
  (ok (is-down-dir +dir-down+))
  (ok (is-down-dir +dir-right-down+))
  (ok (is-down-dir +dir-left-down+))
  (ok (not (is-down-dir +dir-right-up+))))

(subtest
    "Test is-right-dir"
  (ok (is-right-dir +dir-right+))
  (ok (is-right-dir +dir-right-down+))
  (ok (is-right-dir +dir-right-up+))
  (ok (not (is-right-dir +dir-left-up+))))

(subtest
    "Test is-left-dir"
  (ok (is-left-dir +dir-left+))
  (ok (is-left-dir +dir-left-down+))
  (ok (is-left-dir +dir-left-up+))
  (ok (not (is-left-dir +dir-right-down+))))

(finalize)
