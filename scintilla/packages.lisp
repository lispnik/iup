(defpackage #:iup-scintilla-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-scintilla
  (:use #:common-lisp)
  (:export #:scintilla
	   #:scintilla-dialog
	   #:scintilla-send-message
	   #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
