(defpackage #:iup-scintilla
  (:use #:common-lisp
        #:serapeum)
  (:export #:scintilla
	   #:scintilla-dialog
	   #:scintilla-send-message
	   #:open)
  (:shadow #:open))

(in-package #:iup-scintilla)

(defalias open #'iup-scintilla-cffi::%iup-scintilla-open)

(iup::defiupclasses "IUP-SCINTILLA")

(defalias scintilla-send-message #'iup-scintilla-cffi::%iup-scintilla-send-message)
