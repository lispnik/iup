;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-gl" "cl-opengl" "cl-glu")))

(defpackage #:iup-examples.cube
  (:use #:common-lisp)
  (:export #:cube))

(in-package #:iup-examples.cube)

(defvar *canvas* nil)
(defvar *tt* 0.0)

(defvar *vertices*
  #((-1 -1 1) (-1 1 1)
    (1 1 1) (1 -1 1)
    (-1 -1 -1) (-1 1 -1)
    (1 1 -1) (1 -1 -1)))

(defun polygon (a b c d)
  (gl:begin :polygon)
  (apply #'gl:vertex (aref *vertices* a))
  (apply #'gl:vertex (aref *vertices* b))
  (apply #'gl:vertex (aref *vertices* c))
  (apply #'gl:vertex (aref *vertices* d))
  (gl:end))

(defun color-cube ()
  (gl:color 1 0 0)
  (gl:normal 1 0 0)
  (polygon 2 3 7 6)
  (gl:color 0 1 0)
  (gl:normal 0 1 0)
  (polygon 1 2 6 5)
  (gl:color 0 0 1)
  (gl:normal 0 0 1)
  (polygon 0 3 2 1)
  (gl:color 1 0 1)
  (gl:normal 0 -1 0)
  (polygon 3 0 4 7)
  (gl:color 1 1 0)
  (gl:normal 0 0 -1)
  (polygon 4 5 6 7)
  (gl:color 0 1 1)
  (gl:normal -1 0 0)
  (polygon 5 4 0 1))

(defun cube ()
  (iup:with-iup ()
    (iup-gl:open)
    (setf *canvas*
          (iup-gl:canvas :rastersize "640x480"
                         :buffer "DOUBLE"
                         :action 'repaint
                         :resize_cb 'resize))
    (let* ((dialog (iup:dialog *canvas* :title "IUP OpenGL")))
      ;; FIXME      (iup-cffi::%iup-set-function :idle_action 'idle)
      (setf (iup:attribute *canvas* :depthsize) "16")
      (iup:show dialog)
      (iup:main-loop))))

(defun repaint (handle posx posy)
  (iup-gl:make-current handle)
  (gl:clear-color 0.3 0.3 0.3 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test)
  (gl:matrix-mode :modelview)
  (gl:with-pushed-matrix
    (gl:translate 0 0 0)
    (gl:scale 1 1 1)
    (gl:rotate *tt* 0 0 1)
    (color-cube))
  (iup-gl:swap-buffers handle)
  iup::+default+)

(defun resize (handle width height)
  (iup-gl:make-current handle)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ 4 3) 1 15)
  (glu:look-at 3 3 3 0 0 0 0 0 1)
  iup::+default+)

;;; FIXME
;; (cffi:defcallback idle-cb :int ()
;;   (incf tt)
;;   (iup-gl:make-current canvas)
;;   (repaint canvas)
;;   iup::+default+)

#-sbcl (cube)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cube))
