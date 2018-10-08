(in-package #:iup-cube)

(defvar canvas nil)
(defvar tt 0)
(defvar vertices
  #((-1 -1 1) (-1 1 1) (1 1 1) (1 -1 1)
    (-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1)))

(defun polygon (a b c d)
  (gl:begin :polygon)
  (apply #'gl:vertex (aref vertices a))
  (apply #'gl:vertex (aref vertices b))
  (apply #'gl:vertex (aref vertices c))
  (apply #'gl:vertex (aref vertices d))
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

(defun repaint (handle)
  (gl:clear-color 0.3 0 0 1)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test)
  (gl:matrix-mode :modelview)
  (gl:with-pushed-matrix
    (gl:translate 0 0 0)
    (gl:scale 1.0 1.0 1.0)
    (gl:rotate tt 0 0 1)
    (color-cube))
  (iup-gl:swap-buffers handle))

(cffi:defcallback repaint-cb :int ((handle iup-cffi::ihandle))
  (iup-gl:make-current handle)
  (repaint handle))

(cffi:defcallback idle-cb :int ()
  (incf tt)
  
  ;; FIXME
  iup::+default+)

(defun cube ()
  (iup:with-iup ()
    (iup-gl:open)
    (setf canvas (iup-gl:canvas :rastersize "640x480" :buffer "DOUBLE" :depth_size "16"))
    (let* ((dialog (iup:dialog
		    canvas
		    :title "IUP 3D OpenGL")))
      (repaint canvas)
      ;; (setf (iup:callback canvas :action) 'repaint-cb
      ;; 	    (iup:callback canvas :idle_action) 'idle-cb)
      (iup:show dialog)
      (iup:main-loop))))
	    
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cube))

