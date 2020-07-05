(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd" "cd" "lispqr")))

(defpackage #:iup-examples.qrcode
  (:use #:common-lisp)
  (:export #:qrcode))

(in-package #:iup-examples.qrcode)

(defvar *model*
  (format nil "~A ~A IUP ~A"
          (lisp-implementation-type)
          (lisp-implementation-version)
          (iup:version)))

(defparameter *canvas* nil)

(defun qrcode ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas
                    :rastersize "400x400"
                    :map_cb 'canvas-map
                    :unmap_cb 'canvas-unmap
                    :action 'canvas-redraw))
           (text (iup:text
                  :expand :horizontal
                  :valuechanged_cb (lambda (handle)
                                     (setf *model*
                                           (iup:attribute handle :value))
                                     (canvas-redraw *canvas* 0 0)
                                     iup:+default+)))
           (vbox (iup:vbox (list canvas text) :margin "10x10" :cmargin "10x10"))
           (dialog (iup:dialog vbox :title "QR Code")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

(defun qrcode-draw (canvas model)
  (multiple-value-bind
        (w h)
      (cd:size canvas)
    (cd:clear canvas)
    (when (and model (> (length model) 0))
        (let* ((matrix (lispqr:encode->matrix model :ec-level :q))
               (m (array-dimension matrix 0))
               (n (array-dimension matrix 1))
               (i-step (/ w (1+ m)))
               (j-step (/ h (1+ n))))
          (loop for i below n
                do (loop for j below m
                         do (let* ((x-min (* j j-step))
                                   (x-max (+ x-min j-step))
                                   (y-min (* i i-step))
                                   (y-max (+ y-min i-step))
                                   (cell-color (if (zerop (aref matrix j i))
                                                   cd:+white+
                                                   cd:+black+)))
                              (setf (cd:foreground *canvas*) cell-color)
                              (cd:box canvas x-min x-max y-min y-max))))))))

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (qrcode-draw *canvas* *model*)
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  iup:+default+)

#-sbcl (qrcode)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (qrcode))
