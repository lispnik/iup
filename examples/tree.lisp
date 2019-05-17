(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.tree
  (:use #:common-lisp)
  (:export #:tree))

(in-package #:iup-examples.tree)

(defun tree ()
  (iup:with-iup ()
    (let* ((tree (iup:tree :executeleaf_cb 'executeleaf-callback
			   :rename_cb 'rename-callback
			   :branchclose_cb 'branchclose-callback
			   :branchopen_cb 'branchopen-callback
			   :dragdrop_cb 'dragdrop-callback
			   :rightclick_cb 'rightclick-callback
			   :k_any 'key-callback
			   :showrename :yes))
	   (button (iup:button :title "Test"))
	   (vbox (iup:vbox (list (iup:hbox (list tree button)
					   :cgap 5))
			   :margin "5x5"))
           (dialog (iup:dialog vbox :title "Tree Example")))
      (iup:show dialog)
      (setf (iup:attribute tree :title) "Figures"
	    (iup:attribute tree :addbranch) "3D"
	    (iup:attribute tree :addbranch) "2D"
	    (iup:attribute tree :addleaf) "Test"
	    (iup:attribute tree :addbranch1) "Parallelogram"
	    (iup:attribute tree :addleaf2) "Diamond"
	    (iup:attribute tree :addleaf2) "Square"
	    (iup:attribute tree :addbranch1) "Triangle"
	    (iup:attribute tree :addleaf2) "Scalenus"
	    (iup:attribute tree :addleaf2) "Isoceles"
	    (iup:attribute tree :addleaf2) "Equilateral"
	    (iup:attribute tree :value) 6)
      (iup:main-loop))))

(defun addleaf (handle)
  iup:+default+)

(defun addbranch (handle)
  iup:+default+)

(defun removenode (handle)
  iup:+default+)

(defun renamenode (handle)
  iup:+default+)

(defun selectnode (handle)
  iup:+default+)

(defun executeleaf-callback (&rest args)
  (format t "executeleaf ~S~%" args)
  (force-output)
  iup:+default+)

(defun rename-callback (handle id name)
  (format t "rename ~S~%" (list handle id name))
  (force-output)
  (if (string= name "fool")
      iup:+ignore+
      iup:+default+))

(defun branchclose-callback (handle id)
  (format t "branchclose ~S~%" (list handle id))
  (force-output)
  iup:+default+)

(defun branchopen-callback (handle id)
  (format t "branchopen ~S~%" (list handle id))
  (force-output)
  iup:+default+)

(defun dragdrop-callback (handle drag-id drop-id shift-p control-p)
  (format t "dragdropcallback ~S~%" (list handle drag-id drop-id shift-p control-p))
  (force-output)
  iup:+default+)

(defun key-callback (handle key)
  (format t "key ~S~%" (list handle key))
  (force-output)
  ;; FIXME complete this
  iup:+default+)

(defun rightclick-callback (handle id)
  (format t "rightclick ~S~%" (list handle id))
  (force-output)
  (let ((popup (iup:menu
		(list (iup:item :title "Add Leaf" :action 'addleaf)
		      (iup:item :title "Add Branch" :action 'addbranch)
		      (iup:item :title "Rename Node" :action 'renamenode)
		      (iup:item :title "Remove Node" :action 'removenode)
		      (iup:sub-menu (iup:menu
				     (list (iup:item :title "Root" :action 'selectnode)
					   (iup:item :title "Last" :action 'selectnode)
					   (iup:item :title "Page Up" :action 'selectnode)
					   (iup:item :title "Page Down" :action 'selectnode)
					   (iup:item :title "Next" :action 'selectnode)
					   (iup:item :title "Previous" :action 'selectnode)
					   (iup:separator)
					   (iup:item :title "Invert" :action 'selectnode)
					   (iup:item :title "Block" :action 'selectnode)
					   (iup:item :title "Clear All" :action 'selectnode)
					   (iup:item :title "Mark All" :action 'selectnode)
					   (iup:item :title "Invert All" :action 'selectnode)))
				    :title "Selection")))))
    (setf (iup:attribute handle :value) id)
    (iup:popup popup iup:+mousepos+ iup:+mousepos+)
    (iup:destroy popup))
  iup:+default+)

#-sbcl (tree)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (tree))
