(in-package #:asdf-user)

(defsystem #:iup-scintilla-cffi
  :pathname "scintilla-cffi/"
  :components ((:file "scintilla-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
