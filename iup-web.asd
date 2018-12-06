(defsystem #:iup-web
  :serial t
  :pathname "web"
  :components ((:file "web-cffi"))
  :depends-on (#:iup-web-cffi
	       #:iup
	       #:cffi))
