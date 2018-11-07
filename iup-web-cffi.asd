(defsystem #:iup-web-cffi
  :pathname "web/"
  :components ((:file "web-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
