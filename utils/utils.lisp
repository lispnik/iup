(in-package #:iup-utils)

(defmacro alias (target source) `(setf (fdefinition ,target) ,source))

(defun platform ()
  "Mapping from trivial-features -provided features to classesdb platform keywords."
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)
