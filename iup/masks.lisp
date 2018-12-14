(in-package #:iup)

(export '(+mask-float+
	  +mask-ufloat+
	  +mask-efloat+
	  +mask-uefloat+
	  +mask-float-comma+
	  +mask-ufloat-comma+
	  +mask-int+
	  +mask-int+))

(define-constant +mask-float+ "[+/-]?(/d+/.?/d*|/./d+)" :test #'string=)
(define-constant +mask-ufloat+ "(/d+/.?/d*|/./d+)" :test #'string=)
(define-constant +mask-efloat+ "[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?" :test #'string=)
(define-constant +mask-uefloat+ "(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?" :test #'string=)
(define-constant +mask-float-comma+ "[+/-]?(/d+/,?/d*|/,/d+)" :test #'string=)
(define-constant +mask-ufloat-comma+ "(/d+/,?/d*|/,/d+)" :test #'string=)
(define-constant +mask-int+ "[+/-]?/d+" :test #'string=)
(define-constant +mask-uint+ "/d+" :test #'string=)
