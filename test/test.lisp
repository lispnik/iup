(defpackage #:@name@-test
  (:use #:common-lisp
	#:fiveam
	#:@name@)
  (:export #:run!
	   #:@name@-test-suite))

(in-package #:@name@-test)

(def-suite @name@-test-suite
  :description "")

(in-suite @name@-test-suite)

(test @name@-test
      ""
      (is (@name@)))
