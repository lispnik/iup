(defsystem #:iup-cd
  :serial t
  :pathname "cd"
  :components ((:file "cd"))
  :depends-on (#:cd
	       #:iup
	       #:iup-cd-cffi
	       #:cffi))
