(defsystem #:iup-scintilla-cffi
  :pathname "scintilla"
  :components ((:file "scintilla-cffi"))
  :depends-on (#:iup-cffi
               #:tecgraf-base
	       #:cffi))
