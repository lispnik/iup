(defsystem #:iup-classesdb
  :serial t
  :pathname "classesdb/"
  :components ((:file "classesdb"))
  :depends-on (#:trivial-features
               #:iup-classesdb-cffi
               #:iup-cffi
               #:iup-controls-cffi
               #:iup-plot-cffi
               #:iup-mglplot-cffi
               #:iup-gl-cffi
               #:iup-glcontrols-cffi
               #+(or (and sbcl os-windows) (and ccl windows)) #:iup-olecontrol-cffi
               #:iup-scintilla-cffi
               #:iup-web-cffi
               #:iup-tuio-cffi
               #:local-time)
  :perform (load-op :after (o c) (uiop:symbol-call "IUP-CLASSESDB" "REGENERATE")))
