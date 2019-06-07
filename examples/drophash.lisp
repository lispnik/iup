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
           (label (iup:flat-label :title "Drop files for hash"
                                  :alignment "ACENTER:ACENTER"
                                  :font "Helvetica, 24"
                                  :dropfilestarget :yes
                                  :dropfiles_cb 'drop-files-callback
                                  :expand :yes))
           (frame (iup:frame label))
           (results (iup:multi-line :expand :yes
                                    :readonly :yes
                                    :visiblelines 7
                                    :handlename "results"))
           (vbox (iup:vbox (list list
                                 frame
                                 (iup:sbox results :direction :north))
                           :margin "10x10"
                           :cgap 5))
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
  (let* ((digest
          (intern (iup:attribute (iup:handle "list") :valuestring) "IRONCLAD"))
        (digest-hex 
          (ironclad:byte-array-to-hex-string 
           (ironclad:digest-file digest
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
