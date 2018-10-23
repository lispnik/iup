(in-package #:iup-classesdb)

(autowrap:c-include
 (asdf:system-relative-pathname
  :iup "classesdb/spec/iup.h")
 :spec-path '(:iup/classesdb :spec))

