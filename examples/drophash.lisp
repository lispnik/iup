(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "ironclad")))

(defpackage #:iup-examples.drophash
  (:use #:common-lisp)
  (:export #:drophash))

(in-package #:iup-examples.drophash)

(defun drophash ()
  (iup:with-iup ()
    (let* ((list (iup:list :dropdown :yes
			   :expand :horizontal
			   :handlename "list"))
	   (label (iup:label :title "Drop files for hash"
			     :alignment "ACENTER:ACENTER"
			     :dropfilestarget :yes
			     :dropfiles_cb 'drop-files-callback
			     :expand :yes))
	   (frame (iup:frame label))
	   (results (iup:multi-line :visiblelines 5
				    :expand :horizontal
				    :readonly :yes
				    :handlename "results"))
	   (vbox (iup:vbox (list list frame results)
			   :margin "10x10"
			   :cgap 10))
           (dialog (iup:dialog vbox
			       :title "Drop Hash"
			       :size "HALFxHALF")))
      (loop for digest in (ironclad:list-all-digests)
	    for i from 1
	    do (setf (iup:attribute list i) digest)
	    finally (setf (iup:attribute list :valuestring) 'ironclad:sha256))
      (iup:show dialog)
      (iup:main-loop))))

(defun drop-files-callback (handle filename num x y)
  (let ((digest-hex 
	  (ironclad:byte-array-to-hex-string 
	   (ironclad:digest-file
	    (read-from-string (iup:attribute (iup:handle "list") :valuestring))
	    filename))))
    (setf (iup:attribute (iup:handle "results") :append)
	  (format nil "~A	~A" filename digest-hex)))
  (iup:flush)
  iup:+default+)

#-sbcl (drophash)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (drophash))

