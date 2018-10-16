(in-package #:iup-examples.teapot)

(defun teapot ()
  (iup:with-iup ()
    (iup-gl:open)
    (let* ((canvas (iup-gl:canvas))
	   (dialog (iup:dialog
		    canvas
		    :title "IupGLCanvas"
		    :rastersize "800x600"
		    :margin "5x5"
		    :gap "5")))
      (iup-gl:make-current canvas)
      (gl:load-identity)
      (gl:translate 0 0 -5)
      (gl:rotate 30 1 1 0)
      (gl:light :light0 :position '(0 1 1 0))
      (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
      (gl:clear :color-buffer :depth-buffer)
      (gl:color 1 1 1)
      (gl:front-face :cw)
      (glut:init)
      (glut:solid-teapot 1.3)
      (gl:front-face :ccw)
      (gl:flush)
      (iup-gl:swap-buffers canvas)
      (iup:show dialog)
      (iup:main-loop))))
	    
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (teapot))

