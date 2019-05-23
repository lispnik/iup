(defsystem #:iup-cd-cffi
  :pathname "cd"
  :components ((:file "cd-cffi"))
  :depends-on (#:cd-cffi
	       #:iup-cffi
               #:tecgraf-base
	       #:cffi))
