(in-package #:iup-scintilla-cffi)

(define-foreign-library iup-scintilla
  (:unix "libiup_scintilla.so")
  (:windows "iup_scintilla.dll")
  (t (:default "iup_scintilla")))

(use-foreign-library iup-scintilla)

(defcfun (%iup-scintilla-open "IupScintillaOpen") :void)

(defcfun (%iup-scintilla-send-message "IupScintillaSendMessage") :pointer
  (handle iup-cffi::ihandle)
  (message :unsigned-int)
  (wparam :pointer)
  (lparam :pointer))
