(in-package #:iup-utils)

(defmacro alias (target source) `(setf (fdefinition ,target) ,source))
