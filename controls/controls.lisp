(in-package #:iup-controls)

(alias 'open #'iup-controls-cffi::%iup-controls-open)

(iup::defattributefun cells () (iup-controls-cffi::%iup-cells))
(iup::defattributefun matrix () (iup-controls-cffi::%iup-matrix (cffi:null-pointer)))
(iup::defattributefun matrix-ex () (iup-controls-cffi::%iup-matrix-ex))
(iup::defattributefun matrix-list () (iup-controls-cffi::%iup-matrix-list))

