(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-imglib")))

(defpackage #:iup-examples.imagelib
  (:use #:common-lisp)
  (:export #:imagelib))

(in-package #:iup-examples.imagelib)

(declaim (declaration *image-names*))

(defun imagelib ()
  (iup:with-iup ()
    (iup-imglib:open)
    (let* ((width 400)
	   (height 400)
	   (cbox (iup:cbox (loop for image-name in *image-names*
				 for r from 0 by (/ width 100)
				 for a from 0 by 2
				 for label = (iup:label :image image-name :tip image-name)
				 do (setf (iup:attribute label :cx) (+ (/ width 2) (* r (cos a)))
					  (iup:attribute label :cy) (+ (/ height 2) (* r (sin a))))
				 collect label)
			   :expand :yes))
           (dialog (iup:dialog cbox :title "IUP Image Library Example")))
      (iup:show dialog)
      (iup:main-loop))))

(defvar *image-names* '("IUP_ActionCancel" 			
			"IUP_ActionOk" 			
			"IUP_ArrowDown" 			
			"IUP_ArrowLeft" 			
			"IUP_ArrowRight" 			
			"IUP_ArrowUp" 			
			"IUP_EditCopy" 			
			"IUP_EditCut" 			
			"IUP_EditErase" 			
			"IUP_EditFind" 			
			"IUP_EditPaste" 			
			"IUP_EditRedo" 			
			"IUP_EditUndo" 			
			"IUP_FileClose" 			
			"IUP_FileNew" 			
			"IUP_FileOpen" 			
			"IUP_FileProperties" 			
			"IUP_FileSave" 			
			"IUP_MediaForward" 			
			"IUP_MediaGoToBegin" 			
			"IUP_MediaGoToEnd" 			
			"IUP_MediaPause" 			
			"IUP_MediaPlay" 			
			"IUP_MediaRecord" 			
			"IUP_MediaReverse" 			
			"IUP_MediaRewind" 			
			"IUP_MediaStop" 			
			"IUP_MessageError" 			
			"IUP_MessageHelp" 			
			"IUP_MessageInfo" 			
			"IUP_NavigateHome" 			
			"IUP_NavigateRefresh" 			
			"IUP_Print" 			
			"IUP_PrintPreview" 			
			"IUP_ToolsColor" 			
			"IUP_ToolsSettings" 			
			"IUP_ToolsSortAscend" 			
			"IUP_ToolsSortDescend" 			
			"IUP_ViewFullScreen" 			
			"IUP_Webcam" 			
			"IUP_ZoomActualSize" 			
			"IUP_ZoomIn" 			
			"IUP_ZoomOut" 			
			"IUP_ZoomSelection"))

#-sbcl (imagelib)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (imagelib))
