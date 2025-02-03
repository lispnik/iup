(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :components
  ((:static-file "classesdb.lisp-sexp")
   (:module "iup"
    :serial t
    :components ((:file "packages")
                 (:file "constants")
                 (:file "key")
                 (:file "callback")
                 (:file "global-callback")
                 (:file "classes")
                 (:file "config")
                 (:file "attributes")
                 (:file "status")
                 (:file "masks")
                 (:file "globals")
                 (:file "iup")
                 (:file "misc"))))
  :depends-on (#:iup-cffi
               #:iup-utils
               #:cffi
               #:alexandria
               #:trivial-arguments
               #:split-sequence
               #:parse-number
               #:tecgraf-base))
