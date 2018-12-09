(defsystem #:iup-web
  :serial t
  :pathname "web"
  :components ((:file "web"))
  :depends-on (#:iup-web-cffi
	       #:iup
	       #:cffi))
