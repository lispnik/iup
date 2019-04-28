(defsystem #:iup-scintilla
  :serial t
  :pathname "scintilla"
  :components ((:file "scintilla"))
  :depends-on (#:iup-scintilla-cffi
	       #:cffi
               #:iup))
