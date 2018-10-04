(in-package #:iup-scintilla)

(iup::alias 'scintilla-open #'iup-scintilla-cffi::%iup-scintilla-open)
(iup::defattrfun scintilla () (iup-scintilla-cffi::%iup-scintilla))
