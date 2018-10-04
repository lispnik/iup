(in-package #:iup-scintilla-cffi)

(define-foreign-library iupscintilla
  (:unix "libiup_scintilla.so")
  (t (:default "iup_scintilla")))

(use-foreign-library iupscintilla)

(defcfun (%iup-scintilla "IupScintilla") iup-cffi::ihandle)
