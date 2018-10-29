(defsystem #:iup-web-cffi
  :pathname "web-cffi/"
  :components ((:file "web-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
