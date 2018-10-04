(in-package #:cl-user)

(defun simple-notepad-3-1 ()
  (iup:with-iup
    (let* ((text (print (iup:multi-line :expand "YES")))
	   (vbox (iup:vbox (list text)))
	   (dlg (iup:dialog vbox :title "Simple Notepad" :size "QUARTERxQUARTER")))
      (iup:show-xy dlg iup:+center+ iup:+center+)
      (setf (iup:attr dlg :usersize) nil)
      (iup:main-loop))))


#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero)
  (simple-notepad-3-1))

;;; simple_notepad.c

;; (defun create-main-dialog (config-handle)
;;   ;; FIXME how to handle callbacks?
;;   (let* ((multitext (multi-line :expand "YES" :name "MULTITEXT" :dirty "NO"))
;; 	 (lbl-statusbar (label :name "STATUSBAR" :expand "HORIZONTAL" :padding "10x5"))
;; 	 (item-new (item :title "&New	Ctrl+N" :image "IUP_FileNew"))
;; 	 (btn-new (button :image "IUP_FileNew" :flat "YES" :tip "New (Ctrl+N)" :canfocus "NO"))
;; 	 (item-open (item :title "&Open...	Ctrl+O" :image "IUP_FileOpen"))
;; 	 (btn-open (button :image "IUP_FileOpen" :flat "YES" :tip "Open (Ctrl+O)" :canfocus "NO"))

;; 	 (file-menu (menu (list item-new item-open)))
;; 	 (submenu-file (submenu file-menu :title "&File"))
;; 	 (menu (menu submenu-file))

;; 	 (toolbar-hb (hbox (list btn-new btn-open) :margin "5x5" :gap "2"))
;; 	 (vbox (vbox (list toolbar-hb multitext lbl-statusbar)))
;; 	 (dlg (dialog vbox :size "HALFxHALF" :menu menu)))
;;     dlg))

;; (defun new-file (dialog-handle))
;; (defun open-file (dialog-handle file))

;; (defun simple-notepad (&rest files)
;;   (with-iup
;;     (image-lib-open)
;;     (let* ((config (config :app_name "simple_notepad")))
;;       (config-load config)
;;       (let* ((dialog (create-main-dialog config)))
;; 	(config-dialog-show config dialog "MainWindow")
;; 	(new-file dialog)
;; 	(dolist (file files)
;; 	  (open-file dialog file))
;; 	(main-loop)))))

;; (defun simple-web-browser ()
;;   (with-iup
;;     (image-lib-open)
;;     (iup-web:web-browser-open)
;;     (let* ((web-browser (iup-web:web-browser))
;; 	   (vbox (vbox (list web-browser)))
;; 	   (dlg (dialog vbox :size "HALFxHALF")))
;;       (show-xy dlg +center+ +center+)
;;       (main-loop))))

;; #+nil
;; (sb-int:with-float-traps-masked
;;     (:divide-by-zero)
;;   (simple-web-browser))
