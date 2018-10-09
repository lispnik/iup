(in-package #:iup-scintilla)

(alias 'open #'iup-scintilla-cffi::%iup-scintilla-open)
(alias 'scintilla-send-message #'iup-scintilla-cffi::%iup-scintilla-send-message)

(iup::defattributefun scintilla () (iup-scintilla-cffi::%iup-scintilla))
(iup::defattributefun scintilla-dialog () (iup-scintilla-cffi::%iup-scintilla-dlg))
