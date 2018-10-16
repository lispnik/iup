(in-package #:iup-examples.glcontrols)

(defvar canvas nil)
(defvar tt 0.0)
(defvar vertices #((-1 -1 1) (-1 1 1) (1 1 1) (1 -1 1) (-1 -1 -1) (-1 1 -1) (1 1 -1) (1 -1 -1)))

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
  (gl:clear-color 0.3 0.3 0.3 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test)
  (gl:matrix-mode :modelview)
  (gl:with-pushed-matrix
    (gl:translate 0 0 0)
    (gl:scale 1 1 1)
    (gl:rotate tt 0 0 1)
    (color-cube))
  (iup-gl:swap-buffers handle)
  iup::+default+)

(cffi:defcallback repaint-cb :int ((handle iup-cffi::ihandle))
  (iup-gl:make-current handle)
  (repaint handle))

(defun resize (handle width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ 4 3) 1 15)
  (glu:look-at 3 3 3 0 0 0 0 0 1)
  iup::+default+)

(cffi:defcallback resize-cb :int ((handle iup-cffi::ihandle) (width :int) (height :int))
  (iup-gl:make-current handle)
  (resize handle width height))

(cffi:defcallback idle-cb :int ()
  (incf tt)
  (iup-gl:make-current canvas)
  (repaint canvas)
  iup::+default+)

(defun glcontrols ()
  (iup:with-iup ()
    (iup-gl:open)
    (iup-glcontrols:open)
    (iup-controls:open)
    (let* ((glabel (iup-glcontrols:label :title "Label"
					 :fgcolor "255 255 255"
					 :font "Arial, 18"
					 :image (load-image-tecgraf)))
	   (gbutton1 (iup-glcontrols:button :title "Button"
					    :padding "5x5"
					    :action 'button-action-cb
					    :name "button1"
					    :tip "Button Tip"))
	   (gbutton2 (iup-glcontrols:button :padding "5x5"
					    :action 'button-action-cb
					    :name "button2"
					    :image (load-image-file-save)))
	   (gtoggle (iup-glcontrols:toggle :title "Toggle"
					   :padding "5x5"
					   :action 'toggle-action-button-cb
					   :name "toggle"))
	   (gtoggle1 (iup-glcontrols:toggle :padding "5x5"
					    :action 'toggle-action-button-cb
					    :name "toggle"
					    :image (load-image-test)))
	   (gtoggle2 (iup-glcontrols:toggle :padding "5x5"
					    :action 'toggle-action-button-cb
					    :name "toggle"
					    :image (load-image-test)))
	   (gsep1 (iup-glcontrols:separator))
	   (glink (iup-glcontrols:link :url "http://www.tecgraf.puc-rio.br/iup"
				       :title "IUP Toolkit"
				       :action 'link-action-cb))
	   (pbar1 (iup-glcontrols:progress-bar :value 0.3 :show_text "YES"))
	   (gval1 (iup-glcontrols:val :value 0.3
				      :valuechanged_cb 'val-action-cb
				      :progressbar pbar1
				      :name "val1"
				      :tip "Val Tip"
				      :show_text "YES"))
	   (hbox (iup:hbox (list glabel gsep1 gbutton1 gtoggle glink pbar1 gval1)
			   :alignment "ACENTER"
			   :margin "5x5"
			   :gap "5"))
	   (pbar2 (iup-glcontrols:progress-bar :value 0.3 :orientation "VERTICAL"))
	   (gval2 (iup-glcontrols:val :value 0.3
				      :orientation "VERTICAL"
				      :valuechanged_cb 'val-action-cb
				      :progressbar pbar2
				      :name "val2"))
	   (gsep2 (iup-glcontrols:separator))
	   (vbox (iup:vbox (list gbutton2
				 gsep2
				 (iup:radio (iup:vbox (list gtoggle1 gtoggle2)) :margin "0x0")
				 pbar2
				 gval2)
			   :alignment "ACENTER"
			   :margin "5x5"
			   :gap "5"))
	   (gtoggle5 (iup-glcontrols:toggle :title "Toggle"
					    :padding "5x5"
					    :action 'toggle-action-cb
					    :name "toggle5"
					    :checkmark "YES"))
	   (gtoggle3 (iup-glcontrols:toggle :title "Radio Toggle"
					    :padding "5x5"
					    :action 'toggle-action-cb
					    :name "toggle3"
					    :checkmark "YES"))
	   (gtoggle4 (iup-glcontrols:toggle :title "Radio Toggle"
					    :padding "5x5"
					    :image (load-image-test)
					    :action 'toggle-action-cb
					    :name "toggle4"
					    :checkmark "YES"))
	   (vbox2 (iup:vbox (list (iup:radio (iup:vbox (list gtoggle3 gtoggle4)) :margin "0x0")
				  gtoggle5)))
	   (gsbox (iup-glcontrols:size-box (iup-glcontrols:scroll-box vbox2 :rastersize "90x90")))
	   (gframe1 (iup-glcontrols:frame hbox :title "Frame1"))
	   (gframe2 (iup-glcontrols:frame vbox :backcolor "250 250 160" :framecolor "250 250 160") )
	   (gframe3 (iup-glcontrols:frame gsbox :title "Frame3"
						:titlebox "YES"
						:moveable "YES"
						:position "550,200"))
	   (gexp1 (iup-glcontrols:expander gframe1 :title "Expander"
						   :horizontalalign "ACENTER"
						   :verticalalign "ATOP"
						   :moveable "YES"))
	   (gexp2 (iup-glcontrols:expander gframe2 :barposition "LEFT"
						   :horizontalalign "ALEFT"
						   :verticalalign "ACENTER"))
	   (text (iup:text :value "Text"))
	   (matrix (iup-controls:matrix))
;;   IupSetAttribute(matrix, "NUMLIN", "3");
;;   IupSetAttribute(matrix, "NUMCOL", "2");
;;   IupSetAttribute(matrix, "NUMLIN_VISIBLE", "3");
;;   IupSetAttribute(matrix, "NUMCOL_VISIBLE", "2");
;;   IupSetAttribute(matrix, "0:0", "Inflation");
;;   IupSetAttribute(matrix, "1:0", "Medicine");
;;   IupSetAttribute(matrix, "2:0", "Food");
;;   IupSetAttribute(matrix, "3:0", "Energy");
;;   IupSetAttribute(matrix, "0:1", "January 2000");
;;   IupSetAttribute(matrix, "0:2", "February 2000");
;;   IupSetAttribute(matrix, "1:1", "5.6");
;;   IupSetAttribute(matrix, "2:1", "2.2");
;;   IupSetAttribute(matrix, "3:1", "4.1");
;;   IupSetAttribute(matrix, "1:2", "10");
;;   IupSetAttribute(matrix, "2:2", "1");
;;   IupSetAttribute(matrix, "3:2", "0.5");
;; //  IupSetAttribute(matrix, "EXPAND", "No");
;;   IupSetAttribute(matrix, "SCROLLBAR", "No");
	   (vbox3 (iup-glcontrols:frame (iup:vbox (list text matrix)) :title "Frame4"
								      :moveable "YES"
								      :position "250,350"))
	   (canvas (iup-glcontrols:canvas-box (list gexp1 gexp2 gframe3 vbox3) :depth_size 16))
	   
  ;; image_open = IupImage(16, 16, img_open);
  ;; image_close = IupImage(16, 16, img_close);
  ;; image_high = IupImage(16, 16, img_close);
  ;; IupSetAttribute(image_open, "0", "BGCOLOR");
  ;; IupSetAttribute(image_open, "1", "192 192 192");
  ;; IupSetAttribute(image_close, "0", "BGCOLOR");
  ;; IupSetAttribute(image_close, "1", "192 192 192");
  ;; IupSetAttribute(image_high, "1", "192 192 192");

  ;; //  IupSetAttribute(gexp1, "BARSIZE", "50");
  ;; //  IupSetAttributeHandle(gexp1, "IMAGE", image_close);
  ;; //  IupSetAttributeHandle(gexp1, "IMOPEN", image_open);
  ;; //  IupSetAttribute(gexp1, "IMAGE", "img1");
  ;; IupSetCallback(gexp1, "ACTION", (Icallback)expand_cb);
  ;; IupSetAttribute(gexp1, "EXTRABUTTONS", "3");
  ;; IupSetCallback(gexp1, "EXTRABUTTON_CB", (Icallback)extrabutton_cb);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRA1", image_close);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAPRESS1", image_open);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAHIGHLIGHT1", image_high);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRA2", image_close);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAPRESS2", image_open);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAHIGHLIGHT2", image_high);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRA3", image_close);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAPRESS3", image_open);
  ;; IupSetAttributeHandle(gexp1, "IMAGEEXTRAHIGHLIGHT3", image_high);
  ;; //IupSetAttribute(gexp1, "REDRAWALL", "No");

  ;; IupSetCallback(canvas, "ACTION", action);
  ;; IupSetCallback(canvas, "BUTTON_CB", (Icallback)button_cb);
  ;; IupSetCallback(canvas, "MOTION_CB", (Icallback)motion_cb);
  ;; IupSetAttribute(canvas, "BUFFER", "DOUBLE");
  ;; IupSetAttribute(canvas, "MARGIN", "10x10");

  ;; box = IupVbox(canvas, NULL);
  ;; IupSetAttribute(box, "MARGIN", "5x5");

  ;; dlg = IupDialog(box);
  ;; IupSetAttribute(dlg, "TITLE", "IupGLCanvas Test");
  ;; IupSetAttribute(dlg, "RASTERSIZE", "800x600");

  ;; IupMap(dlg);

  ;; IupGLMakeCurrent(canvas);
  ;; printf("Vendor: %s\n", glGetString(GL_VENDOR));
  ;; printf("Renderer: %s\n", glGetString(GL_RENDERER));
  ;; printf("Version: %s\n", glGetString(GL_VERSION));

  ;; IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
  ;; IupSetAttribute(dlg, "RASTERSIZE", NULL);
	   (dialog (iup:dialog
		    (iup:vbox (list (iup-glcontrols:canvas-box canvas :depth-size 16)))
		    :title "IUPGLControls Test")))
      (setf (iup:callback canvas :action) 'repaint-cb
	    (iup:callback canvas :resize_cb) 'resize-cb)
      (iup-cffi::%iup-set-function :idle_action (cffi:get-callback 'idle-cb))
      (iup:show dialog)
      (iup:main-loop))))
	    
#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (glcontrols))
