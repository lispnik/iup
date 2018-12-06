(defpackage #:iup-scintilla
  (:use #:common-lisp)
  (:export #:scintilla
	   #:scintilla-dialog
	   #:scintilla-send-message
	   #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-scintilla)

(alias 'open #'iup-scintilla-cffi::%iup-scintilla-open)

(iup::defiupclasses "IUP-SCINTILLA")

(alias 'scintilla-send-message #'iup-scintilla-cffi::%iup-scintilla-send-message)
