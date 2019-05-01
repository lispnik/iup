(in-package #:iup-examples.simple-notepad)

;;; simple_notepad.c

;;; utilities

(defun write-file (filename string count)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-sequence string stream :end count)))

(defun save-file (handle)
  (let ((filename (iup:attribute handle :filename))
	(str (iup:attribute handle :value))
	(count (iup:attribute handle :count :int)))
    (when (write-file filename str count)
      (setf (iup:attribute handle :dirty)  "NO"))))

(defun save-check (handle)
  (let ((multitext (iup:get-dialog-child handle :multitext)))
    (unless (zerop (iup:attribute multitext :dirty :int))
      (ecase (iup:alarm "Warning" "File is not saved! Save it now?" "Yes" "No" "Cancel")
	(1 (save-file multitext))
	(2 (return-from save-check t))
	(3 (return-from save-check nil)))))
  t)
;;; callbacks

(cffi:defcallback item-new-action-cb :int ((item-new iup-cffi::ihandle))
  (when (save-check item-new)
    (new-file item-new))
  iup::+default+)

(defun open-file (dialog-handle file))

(defun create-main-dialog (config-handle)
  ;; FIXME how to handle callbacks?
  (let* ((multitext (iup:multi-line :expand "YES" :name "MULTITEXT" :dirty "NO" :caret_cb nil :valuechanged_cb nil :dropfiles_cb nil))
	 (lbl-statusbar (iup:label :name "STATUSBAR" :expand "HORIZONTAL" :padding "10x5"))
	 (item-new (iup:item :title "&New	Ctrl+N" :image "IUP_FileNew" :action 'item-new-action-cb))
	 (btn-new (iup:button :image "IUP_FileNew" :flat "YES" :tip "New (Ctrl+N)" :canfocus "NO" :action nil))
	 (item-open (iup:item :title "&Open...	Ctrl+O" :image "IUP_FileOpen" :action nil))
	 (btn-open (iup:button :image "IUP_FileOpen" :flat "YES" :tip "Open (Ctrl+O)" :canfocus "NO" :action nil))
	 (item-save (iup:item :title "&Save	Ctrl+S" :name "SAVE" :image "IUP_FileSave"))
	 (btn-save (iup:button :image "IUP_FileSave" :name "ITEM_SAVE" :flat "YES" :tip "Save (Ctrl+S)" :canfocus "NO" :action nil))
	 (item-saveas (iup:item :title "Save &As..." :name "ITEM_SAVEAS" :action nil))
	 (item-revert (iup:item :title "&Revert" :name "ITEM_REVERT" :action nil))
	 (item-exit (iup:item :title "E&xit" :action nil))
	 (item-find (iup:item :title "&Find...	Ctrl+F" :image "IUP_EditFind" :action nil))
	 (btn-find (iup:button :image "IUP_EditFind" :flat "YES" :tip "Find (Ctrl+F)" :canfocus "NO" :action nil))
	 (item-find-next (iup:item :title "Find &Next	F3" :name "ITEM_FINDNEXT" :action nil))
	 (item-replace (iup:item :title "&Replace...	Ctrl+H" :action nil))
	 (item-cut (iup:item :title "Cu&t	Ctrl+X" :name "ITEM_CUT" :image "IUP_EditCut" :action nil))
	 (btn-cut (iup:button :image "IUP_EditCut" :flat "YES" :action nil :tip "Cut (Ctrl+X)" :canfocus "NO"))
	 (item-copy (iup:item :title "&Copy	Ctrl+C" :name "ITEM_COPY" :image "IUP_EditCopy" :action nil))
	 (btn-copy (iup:button :image "IUP_EditCopy" :flat "YES" :action nil :tip "Copy (Ctrl+C)" :canfocus "NO"))
	 (item-paste (iup:item :title "&Paste	Ctrl+V" :name "ITEM_PASTE" :image "IUP_EditPaste" :action nil))
	 (btn-paste (iup:button :image "IUP_EditPaste" :flat "YES" :action nil :tip "Paste (Ctrl+V)" :canfocus "NO"))
	 (item-delete (iup:item :title "&Delete	Del" :name "ITEM_DELETE" :image "IUP_EditErase" :action nil))
	 (item-select-all (iup:item :title "Select &All	Ctrl+A" :action nil))
	 (item-goto (iup:item :title "&Go To	Ctrl+G" :action nil))
	 (item-toolbar (iup:item :title "&Toolbar" :value "ON" :action nil))
	 (item-statusbar (iup:item :title "&Statusbar" :value "ON" :action nil))
	 (item-font (iup:item :title "&Font..." :action nil))
	 (item-help (iup:item :title "&Help..." :action nil))
	 (item-about (iup:item :title "&About..." :action nil))
	 (recent-menu (iup:menu nil))
	 (file-menu (iup:menu (list item-new
				    item-open
				    item-save
				    item-saveas
				    item-revert
				    (iup:separator)
				    (iup:submenu recent-menu :title "Recent &Files")
				    item-exit)
			      :open_cb nil))
	 (edit-menu (iup:menu (list item-cut
				    item-copy
				    item-paste
				    item-delete
				    (iup:separator)
				    item-find
				    item-find-next
				    item-replace
				    (iup:separator)
				    item-goto
				    (iup:separator)
				    item-select-all)
			      :open_cb nil))
	 (format-menu (iup:menu (list item-font)))
	 (view-menu (iup:menu (list item-toolbar item-statusbar)))
	 (help-menu (iup:menu (list item-help item-about)))
	 (sub-menu-file (iup:submenu file-menu :title "&File"))
	 (sub-menu-edit (iup:submenu edit-menu :title "&Edit"))
	 (sub-menu-format (iup:submenu format-menu :title "F&ormat"))
	 (sub-menu-view (iup:submenu view-menu :title "&View"))
	 (sub-menu-help (iup:submenu help-menu :title "&Help"))
	 (menu (iup:menu (list sub-menu-file
			       sub-menu-edit
			       sub-menu-format
			       sub-menu-view
			       sub-menu-help)))
	 (toolbar-hb (iup:hbox (list btn-new
				     btn-open
				     btn-save
				     (iup:label :separator "VERTICAL")
				     btn-cut
				     btn-copy
				     btn-paste
				     (iup:label :separator "VERTICAL")
				     btn-find)
			       :margin "5x5" :gap "2"))
	 (vbox (iup:vbox (list toolbar-hb multitext lbl-statusbar)))
	 (dlg (iup:dialog vbox :size "HALFxHALF"
			       :menu menu
			       :close_cb nil
			       :dropfiles_cb nil
			       :|K_cN| nil
			       :|K_cO| nil
			       :|K_cS| nil
			       :|K_cF| nil
			       :|K_cH| nil
			       :|K_cG| nil
			       :|K_F3| nil
			       :|K_cF3| nil
			       :|K_cV| nil)))
    (setf (iup:attribute-handle nil :parentdialog) dlg)
    ;; TODO configy stuff
    dlg))

(defun simple-notepad (&rest files)
  (iup:with-iup ()
    (iup:image-lib-open)
    (let* ((config (iup:config :app_name "simple_notepad")))
      (iup:config-load config)
      (let* ((dialog (create-main-dialog config)))
	(iup:config-dialog-show config dialog "MainWindow")
	(new-file dialog)
	(dolist (file files)
	  (open-file dialog file))
	(iup:main-loop)))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (simple-notepad))

