(in-package #:iup-classesdb-cffi)

(autowrap:c-include
 (asdf:system-relative-pathname
  :iup "classesdb/include/iup.h")
 :spec-path '(:iup/classesdb :spec)
 :accessor-package #:iup-classesdb-cffi.accessors
 :function-package #:iup-classesdb-cffi.functions)

