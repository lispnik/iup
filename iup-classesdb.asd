(defsystem #:iup-classesdb
  :serial t
  :pathname "classesdb/"
  :components ((:file "classesdb"))
  ;; Only the core. The addon libraries and their initializers are named as
  ;; data in *static-metadata* and loaded dynamically, precisely so that an
  ;; addon with no library here is a skipped entry rather than a load failure
  ;; of this whole system -- most of the addons have no CMake target in
  ;; lispnik/tecgraf-iup yet.
  :depends-on (#:trivial-features
               #:iup-classesdb-cffi
               #:iup-cffi
               #:iup-utils
               #:alexandria
               #:cffi
               #:local-time)
  :perform (load-op :after (o c) (uiop:symbol-call "IUP-CLASSESDB" "REGENERATE")))
