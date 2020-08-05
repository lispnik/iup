(defsystem #:iup-threads
  :serial t
  :pathname "threads/"
  :components ((:file "threads"))
  :depends-on (#:iup
               #:bordeaux-threads
               #:lparallel))
