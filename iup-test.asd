(defsystem #:iup-test
  :components
  ((:module "test"
    :serial t
    :components ((:file "tests"))))
  :depends-on (#:fiveam #:iup)
  :perform (test-op (o s)
		    (symbol-call "FIVEAM" "RUN!"
				 (find-symbol* "IUP-TEST-SUITE" "IUP-TESTS"))))
