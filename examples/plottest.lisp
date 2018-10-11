(in-package #:iup-plottest)

(cffi:defcallback action-cb :int ((handle iup-cffi::ihandle))
  iup::+default+)

(defun plottest ()
  (iup:with-iup ()
    (iup:show dialog)
    (iup:main-loop)))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (plottest))
