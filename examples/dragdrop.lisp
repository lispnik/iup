(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-imglib")))

(defpackage #:iup-examples.dragdrop
  (:use #:common-lisp)
  (:export #:dragdrop))

(in-package #:iup-examples.dragdrop)

;;; from https://www.tecgraf.puc-rio.br/iup/examples/C/list2.c

(defun dragdrop ()
  (iup:with-iup ()
    (let* ((button1 (iup:button :title "(1) Drop + Edit"
				:action 'button-callback))
	   (button2 (iup:button :title "(2) Drop"
				:action 'button-callback))
	   (button3 (iup:button :title "(3) List + Edit"
				:action 'button-callback))
	   (button4 (iup:button :title "(4) List"
				:action 'button-callback))
	   (list1 (iup:list :action 'list-callback
			    :expand :horizontal
			    :editbox :yes
			    :dropdown :yes
			    :visibleitems 5
			    :showimage :yes
			    :edit_cb 'edit-callback
			    :droptarget :yes
			    :droptypes "TEXT,STRING"
			    :dropdata_cb 'test-drop-data-callback
			    :dropmotion_cb 'test-drop-motion-callback
			    :name "list1"))
	   (list2 (iup:list :action 'list-callback
			    :expand :horizontal
			    :dropdown :yes
			    :visibleitems 5
			    :showimage :yes))
	   (list3 (iup:list :action 'list-callback
			    :expand :yes
			    :editbox :yes
			    :edit_cb 'edit-callback))
	   (list4 (iup:list :action 'list-callback
			    :expand :yes))
	   (text1 (iup:text :multiline :yes
			    :rastersize "150x90"
			    :value "Drop Text Here"
			    :droptarget :yes
			    :droptypes "TEXT"
			    :dropdata_cb 'test-drop-data-callback
			    :dropmotion_cb 'test-drop-motion-callback
			    :name "txt1"))
	   (text2 (iup:text :multiline :yes
			    :rastersize "150x90"
			    :value "Drop Text From Here"
			    :dragsource :yes
			    :dragtypes "TEXT,STRING"
			    :dragsourcemove :yes
			    :dragbegin_cb 'test-drag-begin-callback
			    :dragdatasize_cb 'test-drag-data-size-callback
			    :dragdata_cb 'test-drag-data-callback
			    :dragend_cb 'test-drag-data-end-callback
			    :name "txt2"))

	   (box1 (iup:vbox (list list1 button1 text1 text2)))
	   (box2 (iup:vbox (list list2 button2)))
	   (box3 (iup:vbox (list list3 button3)))
	   (box4 (iup:vbox (list list4 button4)))

	   (label (iup:label :title ""
			     :expand :horizontal))
	   (button-ok (iup:button :title "OK"
				  :action 'button-close-callback
				  :handlename "btok"))
	   (button-cancel (iup:button :title "Cancel"
				      :action 'button-close-callback
				      :handlename "btcancel"))




	   (dialog (iup:dialog (iup:vbox (list (iup:hbox (list box1 box2 box3 box4))
					       label button-ok button-cancel)
					 :margin "10x10" :gap 10)
			       :title "Drag & Drop List Example"
			       :defaultenter "btok"
			       :defaultesc "btcancel")))
      (loop for item1 in '("US$ 1000" "US$ 2000" "US$ 30000000" "US$ 4000" "US$ 5000" "US$ 6000" "US$ 7000")
	    for item2 in '("R$ 1000" "R$ 2000" "R$ 3000" "R$ 4000" "R$ 5000" "R$ 6000" "R$ 7000")
	    for item3 in '("Char A" "Char B" "Char CCCCC" "Char D" "Char F" "Char G" "Char H")
	    for i from 1
	    do (setf (iup:attribute list1 i) item1
		     (iup:attribute list2 i) item2
		     (iup:attribute list3 i) item3
		     (iup:attribute list4 i) (format nil "Number ~A" i)))
      (loop for button in (list button1 button2 button3 button4)
	    for list in (list list1 list2 list3 list4)
	    for i from 1
	    do (setf (iup:attribute button "_LIST") list
		     (iup:attribute button-ok (format nil "_LIST~A" i)) list
		     (iup:attribute button :bgcolor) "192 192 192"
		     (iup:callback list :getfocus_cb) 'get-focus-callback
		     (iup:callback list :killfocus_cb) 'kill-focus-callback))
      (iup:show dialog)
      (iup:main-loop))))

(defun list-callback (handle text i v)
  (let ((label (iup:attribute handle "_LABEL")))
    (setf (iup:attribute label :title) text))
  iup:+default+)

(defun edit-callback (handle c after)
  (format t "edit ~S~%" (list :ih (iup:attribute handle :name)
			      :c c
			      :after after))
  (let ((label (iup:attribute handle "_LABEL")))
    (unless (zerop c)
      (setf (iup:attribute label :title) after)))
  iup:+default+)

(defun button-close-callback (handle)
  (format t "button-close ~S~%"
	  (list :ih (iup:attribute handle :name)))
  iup:+close+)

(defun button-callback (handle)
  (format t "button ~S~%" (list :ih (iup:attribute handle :name)))
  (let ((list (iup:attribute handle "_LIST" 'cffi:foreign-pointer)))
    (iup:message "List" (format nil "Value=~A" (iup:attribute list :value))))
  iup:+default+)

(defun get-focus-callback (handle)
  (format t "get-focus ~S~%" (list :ih (iup:attribute handle :name)))
  (let ((button (iup:attribute handle "_BUTTON")))
    (setf (iup:attribute button :bgcolor) "255 0 128"))
  iup:+default+)

(defun kill-focus-callback (handle)
  (let ((button (iup:attribute handle "_BUTTON")))
    (setf (iup:attribute button :bgcolor) nil))
  iup:+default+)

(defun test-drop-data-callback (handle type data len x y)
  (format t "drop-data ~S~%" (list :ih (iup:attribute handle :name)
				   :type type
				   :data data
				   :len len
				   :x x
				   :y y))
  iup:+default+)

(defun test-drop-motion-callback (handle x y status)
  (format t "drop-motion ~S~%" (list :ih (iup:attribute handle :name)
				     :x x
				     :y y
				     :status status))
  iup:+default+)

(defun test-drag-data-end-callback (handle del)
  (format t "drag-data-end ~S~%" (list :ih (iup:attribute handle :name)
				       :del del))
  iup:+default+)

(defun test-drag-data-callback (handle type data length)
  (format t "drag-data ~S~%" (list :ih (iup:attribute handle :name)
				   :type type
				   :data data
				   :length length))
  ;; FIXME this is brutal
  (loop for i from 0 below length
	do (setf (cffi:mem-aref data :unsigned-char i)
		 (char-int (char "Drag Text Test Sample Drag Text Test Sample" i))))
  iup:+default+)

(defun test-drag-data-size-callback (handle type)
  (format t "drag-data-size ~S~%" (list :ih (iup:attribute handle :name) :type type))
  22)

(defun test-drag-begin-callback (handle x y)
  (format t "drag-begin ~S~%" (list :ih (iup:attribute handle :name) :x x :y y))
  iup:+default+)

#-sbcl (dragdrop)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (dragdrop))
