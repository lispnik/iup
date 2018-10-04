(defpackage #:iup-scintilla-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-scintilla
  (:use #:common-lisp)
  (:export #:scintilla
	   #:scintilla-send-message))
