(defpackage #:iup-scintilla-cffi
  (:use #:common-lisp)
  (:import-from #:tecgraf-base #:ihandle))

(in-package #:iup-scintilla-cffi)

(cffi:define-foreign-library iup-scintilla
  (:unix "libiup_scintilla.so")
  (:windows "iup_scintilla.dll")
  (t (:default "iup_scintilla")))

(cffi:use-foreign-library iup-scintilla)

(cffi:defcfun (%iup-scintilla-open "IupScintillaOpen") :void)

(cffi:defcfun (%iup-scintilla-send-message "IupScintillaSendMessage") :pointer
  (handle ihandle)
  (message :unsigned-int)
  (wparam :pointer)
  (lparam :pointer))
