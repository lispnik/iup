(defpackage #:iup-utils
  (:use #:common-lisp)
  (:export #:platform))

(in-package #:iup-utils)

(defun platform ()
  "Mapping from trivial-features -provided features to classesdb platform keywords."
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)
