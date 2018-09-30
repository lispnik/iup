(defsystem #:@name@-test
  :components
  ((:module "test"
    :serial t
    :components ((:file "tests"))))
  :depends-on (#:fiveam #:@name@)
  :perform (test-op (o s)
		    (symbol-call "FIVEAM" "RUN!"
				 (find-symbol* "@NAME@-TEST-SUITE" "@NAME@-TESTS"))))
