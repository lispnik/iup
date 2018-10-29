(in-package #:asdf-user)

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup/"
  :components ((:file "iup"))
  :depends-on (#:iup-cffi
	       #:iup-utils
	       #:cffi
	       #:alexandria))

;; (defsystem #:iup/all
;;   :depends-on (#:iup
;; 	       #:iup/controls
;; 	       #:iup/plot
;; 	       #:iup/mglplot
;; 	       #+windows #:iup/olecontrol
;; 	       #:iup/gl
;; 	       #:iup/glcontrols
;; 	       #:iup/scintilla
;; 	       #:iup/web))

;; (defsystem #:iup/examples
;;   :serial t
;;   :pathname "examples/"
;;   :components ((:file "packages")
;; 	       (:file "buttons")
;; 	       (:file "simple-notepad")
;; 	       (:file "web-browser")
;; 	       (:file "sample")
;; 	       (:file "cube")
;; 	       (:file "teapot")
;; 	       (:file "plottest")
;; 	       (:file "cells")
;; 	       (:file "cells-numbered")
;; 	       (:file "matrix")
;; 	       (:file "glcontrols")
;; 	       #+windows (:file "windows"))
;;   :depends-on (#:alexandria
;; 	       #:iup
;; 	       #:iup/web
;; 	       #:iup/plot
;; 	       #:iup/gl
;; 	       #:iup/glcontrols
;; 	       #:iup/scintilla
;; 	       #:cd
;; 	       #:cl-opengl
;; 	       #:cl-glut
;; 	       #:cl-glu))

