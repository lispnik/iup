(defsystem #:iup-classesdb
  :serial t
  :pathname "classesdb/"
  :components ((:file "classesdb"))
  ;; Pure Lisp. The metadata now arrives as classes.sexp files shipped inside
  ;; the tecgraf-iup release archives (written by tools/iup_classdump.c
  ;; there), and this system only transforms them into classesdb.lisp-sexp:
  ;;
  ;;   (iup-classesdb:regenerate-from-dumps #p"classes-linux.sexp" ...)
  ;;
  ;; The previous incarnation introspected a running IUP over FFI, which
  ;; needed a GUI session per platform and internal SDK symbols; see the
  ;; header of classesdb.lisp for why that lost to this.
  :depends-on (#:alexandria
               #:local-time))
