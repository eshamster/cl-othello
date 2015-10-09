(in-package :cl-user)
(defpackage cl-othello-test.utils
  (:use :cl
        :cl-othello.utils
        :prove))
(in-package :cl-othello-test.utils)

(plan 8)

(subtest
    "Test string-to-list"
  (is (string-to-list "test") '(TEST))
  (is (string-to-list "ThiS is a test") '(THIS IS A TEST)))

(subtest
    "Test stream-to-list"
  (labels ((test (com list)
	     (is (stream-to-list (make-string-input-stream com)) list)))
    (test "test" '(TEST))
    (test "thIs is a Test" '(THIS IS A TEST))))

(subtest
    "Test to-string"
  (is (to-string "test") "test")
  (is (to-string 123) "123")
  (is-error (to-string #'+) 'simple-error)
  (is (to-string 'car) "COMMON-LISP:CAR")
  (is (to-string 'test) "CL-OTHELLO-TEST.UTILS:TEST"))

(subtest
    "Test concat-symbol"
  (is (concat-symbol) nil)
  (is (concat-symbol 'abc) 'abc)
  (is (concat-symbol 'abc- 'def) 'abc-def)
  (is (concat-symbol 'abc- 'def 'gh) 'abc-defgh))

(subtest
    "Test processing of symbol-name"
  (subtest
      "Test symbol-name-with-package"
    (is (symbol-name-with-package 'car) "COMMON-LISP:CAR" :test #'equal)
    (is (symbol-name-with-package 'symbol-name-with-package)
        "CL-OTHELLO.UTILS:SYMBOL-NAME-WITH-PACKAGE" :test #'equal)
    (is (symbol-name-with-package 'cl-othello.utils:symbol-name-with-package)
        "CL-OTHELLO.UTILS:SYMBOL-NAME-WITH-PACKAGE" :test #'equal))
  (subtest
      "Test intern-with-package"
    (is (intern-with-package "CAR") 'car :test #'equal)
    (is (intern-with-package "INTERN-WITH-PACKAGE") 'intern-with-package :test #'equal)
    (is (intern-with-package "CL-OTHELLO.UTILS:INTERN-WITH-PACKAGE")
        'cl-othello.utils:intern-with-package :test #'equal)))

(subtest
    "Test push-without-dup"
  (is (push-without-dup 3 '(1 2) #'=) '(1 2 3))
  (is (push-without-dup 1 '(1 2) #'=) '(1 2))
  (is (push-without-dup '(3 6) '((1 2) (2 4))
			      #'(lambda (a b) (= (car a) (car b))))
	    '((1 2) (2 4) (3 6)))
  (is (push-without-dup '(2 6) '((1 2) (2 4) (2 5) (3 6))
			      #'(lambda (a b) (= (car a) (car b))))
	    '((1 2) (2 6) (2 5) (3 6))))

(subtest
    "Test read-line-while"
  (is (read-line-while "test"
			     #'(lambda (str)
				 (not (equal str "test")))
			     (make-string-input-stream
			      (format nil "   ~%abc~%~%  test "))) "test"))

(subtest
    "Test aif-second-true"
  (is (aif-second-true (values 1 t) (+ it 10) 1234)
	    11)
  (is (aif-second-true (values 1 nil) (+ it 10) 1234)
	    1234))

(finalize)
