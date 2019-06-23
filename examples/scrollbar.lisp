(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd" "cd")))

;;; from scrollbar.c

(defpackage #:iup-examples.scrollbar
  (:use #:common-lisp)
  (:export #:scrollbar))

(in-package #:iup-examples.scrollbar)

(defvar *canvas* nil)

(defun action (handle posx posy)
  (unless *canvas*
    (return-from action iup:+default+))
  (cd:activate *canvas*)
  (let ((posy (- 399 (iup:attribute handle :dy 'number) posy)))
    (cd:clear *canvas*)
    (setf (cd:foreground *canvas*) cd:+red+)
    (cd:line *canvas* (- posx) (- posy) (- 599 posx) (- 399 posy))
    (cd:line *canvas* (- posx) (- 399 posy) (- 599 posx) (- posy))
    iup:+default+))

(defun scroll-callback (handle op posx posy)
  (action handle posx posy)
  iup:+default+)

(defun resize-callback (handle width height)
  (setf (iup:attribute handle :dx) width
        (iup:attribute handle :dy) height)
  (unless *canvas*
    (return-from resize-callback iup:+default+))
  (cd:activate *canvas*)
  iup:+default+)

(defun scrollbar ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :rastersize "300x200"
                               :scrollbar :yes
                               :xmax 599
                               :ymax 399
                               :scroll_cb 'scroll-callback
                               :resize_cb 'resize-callback
                               :action 'action))
           (dialog (iup:dialog canvas :title "Scrollbar Test")))
      (iup:map dialog)
      (setf *canvas* (cd:create-canvas (iup-cd:context-iup) canvas)
            (iup:attribute canvas :rastersize) nil)
      (iup:show dialog)
      (iup:main-loop)
      (cd:kill *canvas*))))

#-sbcl (scrollbar)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (scrollbar))
