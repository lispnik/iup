(in-package #:iup-glcontrols)

(alias 'open   #'iup-glcontrols-cffi::%iup-glcontrols-open)

(iup::defattributefun-children canvas-box iup-glcontrols-cffi::%iup-glcontrols-canvas-box)

(iup::defattributefun sub-canvas () (iup-glcontrols-cffi::%iup-glcontrols-sub-canvas))
(iup::defattributefun separator () (iup-glcontrols-cffi::%iup-glcontrols-separator))
(iup::defattributefun progress-bar () (iup-glcontrols-cffi::%iup-glcontrols-progress-bar))
(iup::defattributefun val () (iup-glcontrols-cffi::%iup-glcontrols-val nil))
(iup::defattributefun text () (iup-glcontrols-cffi::%iup-glcontrols-text))
(iup::defattributefun label () (iup-glcontrols-cffi::%iup-glcontrols-label nil))
(iup::defattributefun button () (iup-glcontrols-cffi::%iup-glcontrols-label nil))
(iup::defattributefun toggle () (iup-glcontrols-cffi::%iup-glcontrols-toggle nil))
(iup::defattributefun link () (iup-glcontrols-cffi::%iup-glcontrols-link nil nil))

(iup::defattributefun frame (child) (iup-glcontrols-cffi::%iup-glcontrols-frame child))
(iup::defattributefun expander (child) (iup-glcontrols-cffi::%iup-glcontrols-expander child))
(iup::defattributefun scroll-box (child) (iup-glcontrols-cffi::%iup-glcontrols-scroll-box child))
(iup::defattributefun size-box (child) (iup-glcontrols-cffi::%iup-glcontrols-size-box child))


;;; TODO draw text, image get text, get image info
