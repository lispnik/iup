(defpackage #:iup-scintilla
  (:use #:common-lisp)
  (:export #:scintilla
	   #:scintilla-dialog
	   #:scintilla-send-message
	   #:open)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-scintilla)

(defalias open #'iup-scintilla-cffi::%iup-scintilla-open)

(iup::defiupclasses "IUP-SCINTILLA")

(defalias scintilla-send-message #'iup-scintilla-cffi::%iup-scintilla-send-message)
