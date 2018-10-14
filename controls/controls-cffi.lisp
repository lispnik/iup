(in-package #:iup-controls-cffi)

(define-foreign-library iupcontrols
  (:unix "libiupcontrols.so")
  (t (:default "iupcontrols")))

(use-foreign-library iupcontrols)

(defcfun (%iup-controls-open "IupControlsOpen") :void)

(defcfun (%iup-cells "IupCells") iup-cffi::ihandle)

(defcfun (%iup-matrix "IupMatrix") iup-cffi::ihandle
  (action :pointer))

(defcfun (%iup-matrix-set-formula "IupMatrixSetFormula") :void
  (handle iup-cffi::ihandle)
  (col :int)
  (formula :string)
  (init :string))

(defcfun (%iup-matrix-set-dynamic "IupMatrixSetDynamic") :void
  (handle iup-cffi::ihandle)
  (init :string))

(defcfun (%iup-matrix-ex "IupMatrixEx") iup-cffi::ihandle)

(defcfun (%iup-matrix-list "IupMatrixList") iup-cffi::ihandle)
