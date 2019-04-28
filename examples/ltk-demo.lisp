(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd")))

(defpackage #:iup-example.ltk-demo
  (:use #:common-lisp))

(in-package #:iup-example.ltk-demo)

(defvar *angle* 0.0f0)
(defvar *angle2* 0.0f0)
(defvar *angle3* 0.0f0)

(declaim (single-float *angle* *angle2* *angle3*))

(defvar *canvas* nil)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  iup:+default+)

(defun canvas-redraw (handle x y)
  (draw *canvas*)
  iup:+default+)

(defun draw (canvas)
  (let ((dx (* 50 (sin *angle2*)))
        (dy (* 50 (cos *angle2*)))
        (wx (sin *angle3*)))
    (cd:activate canvas)
    (cd:clear canvas)
    (setf (cd:foreground canvas) cd:+black+)
    (incf *angle* 0.1f0)
    (incf *angle2* 0.03f0)
    (incf *angle3* 0.01f0)
    (cd:with-vertices (canvas :path-mode-open-lines)
      (dotimes (i 100)
        (let* ((w (+ *angle* (* i 2.8001f0)))
               (x (+ dx 250 (* 150 (sin w) wx)))
               (y (+ dy 200 (* 150 (cos w)))))
          (cd:vertex canvas x y)))))
  (cd:flush canvas)
  iup:+default+)

(defun ltk-demo ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :scrollbar "YES"
                               :map_cb 'canvas-map
                               :unmap_cb 'canvas-unmap
                               :action 'canvas-redraw))
           (timer (iup:timer :run "NO"
                             :time 20   ;ms
                             :action_cb #'(lambda (handle)
                                            (iup:redraw canvas 1)
                                            iup:+default+)))
           (progress (iup:progress-bar :min 0 :max 100))
           (button1 (iup:button :title "&Step"
                                :action (lambda (handle)
                                          (draw *canvas*)
                                          iup:+default+)))
           (vbox (iup:vbox
                  (list canvas
                        (iup:radio
                         (iup:hbox
                          (list
                           (iup:label :title "&Eggs: ")
                           (iup:toggle :title "fried")
                           (iup:toggle :title "stirred")
                           (iup:toggle :title "cooked"))))
                        (iup:hbox (list progress button1))
                        (iup:hbox (list (iup:label :title "&Add: ")
                                        (iup:toggle :title "Pepper")
                                        (iup:toggle :title "Salt")))
                        (iup:hbox (list (iup:label :title "&Rotation: ")
                                        (iup:button :title "Start"
                                                    :action (lambda (handle)
                                                              (setf (iup:attribute timer :run) "YES")
                                                              iup:+default+))
                                        (iup:button :title "Stop"
                                                    :action  (lambda (handle)
                                                               (setf (iup:attribute timer :run) "NO")
                                                               iup:+default+))
                                        (iup:button :title "Hallo")
                                        (iup:button :title "Welt!")
                                        )))))
           (dialog (iup:dialog vbox :title "IUP LTK Demonstration"
                                    :size "500x320")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#-sbcl (ltk-demo)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (ltk-demo))
