(in-package #:iup)

(defun simple-notepad-3-1 ()
  (with-iup
    (let* ((text (multi-line :expand "YES"))
	   (vbox (vbox text))
	   (dlg (dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER")))
      (show-xy dlg +center+ +center+)
      (setf (attr dlg :usersize) nil)
      (main-loop))))

(cffi:defcallback exit-cb :int ((handle iup-cffi::ihandle))
  (declare (ignore handle))
  -3)

(defun simple-notepad-3-2 ()
  (with-iup
    (let* ((text (multi-line :expand "YES"))
	   (item-open (item :title "Open"))
	   (item-save-as (item :title "Save As"))
	   (item-exit (item :title "Exit"))
	   (file-menu (menu item-open item-save-as (separator) item-exit))
	   (submenu (submenu file-menu :title "File"))
	   (menu (menu submenu))
	   (vbox (vbox text))
	   (dlg (dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER" :menu menu)))
;;      (iup-cffi::%iup-set-callback item-exit :action (callback exit-cb))
      (iup-cffi::%iup-set-callback item-exit :action (cffi:callback exit-cb))
      (show-xy dlg +center+ +center+)
      (setf (attr dlg :usersize) nil)
      (main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-notepad-3-2))


;;; simple_notepad.c

(defun create-main-dialog (config-handle)
  ;; FIXME how to handle callbacks?
  (let* ((multitext (multi-line :expand "YES" :name "MULTITEXT" :dirty "NO"))
	 (lbl-statusbar (label :name "STATUSBAR" :expand "HORIZONTAL" :padding "10x5"))
	 (item-new (item :title "&New	Ctrl+N" :image "IUP_FileNew"))
	 (btn-new (button :image "IUP_FileNew" :flat "YES" :tip "New (Ctrl+N)" :canfocus "NO"))
	 (item-open (item :title "&Open...	Ctrl+O" :image "IUP_FileOpen"))
	 (btn-open (button :image "IUP_FileOpen" :flat "YES" :tip "Open (Ctrl+O)" :canfocus "NO"))

	 (file-menu (menu item-new item-open))
	 (submenu-file (submenu file-menu :title "&File"))
	 (menu (menu submenu-file))

	 (toolbar-hb (hbox (list btn-new btn-open) :margin "5x5" :gap "2"))
	 (vbox (vbox (list toolbar-hb multitext lbl-statusbar)))
	 (dlg (dialog vbox :size "HALFxHALF" :menu menu)))
    dlg))

(defun new-file (dialog-handle))
(defun open-file (dialog-handle file))

(defun simple-notepad (&rest files)
  (with-iup
    (image-lib-open)
    (let* ((config (config :app_name "simple_notepad")))
      (config-load config)
      (let* ((dialog (create-main-dialog config)))
	(config-dialog-show config dialog "MainWindow")
	(new-file dialog)
	(dolist (file files)
	  (open-file dialog file))
	(main-loop)))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-notepad))
