(ql:quickload '("iup" "im" "uiop" "lparallel"))

(defun main ()
  (iup:with-iup ()
    (let* ((about-dialog
             (iup:dialog (iup:flat-label :title "About Halfclone")
                         :title "About"))
           (file-menu
             (iup:sub-menu
              (iup:menu (list (iup:item :title "&Open")
                              (iup:item :title "&About"
                                        :action (lambda (handle)
                                                  (iup:popup about-dialog
                                                             iup:+mousepos+
                                                             iup:+mousepos+)
                                                  iup:+default+))
                              (iup:separator)
                              (iup:item :title "E&xit"
                                        :action (lambda (handle)
                                                  iup:+close+))))
              :title "Hal&fclone"))
           (menu
             (iup:menu (list file-menu)))
           (image-view
             (iup:flat-label :title "image view here"
                             :size "320x160"))
           (thumbnails
             (iup:flat-label :title "thumbnails here"
                             :size "320x40"))
           (split
             (iup:split image-view thumbnails :orientation :horizontal))
           (status-bar
             (iup:label :title "status here" :expand :horizontal))
           (vbox
             (iup:vbox (list split status-bar)))
           (dialog
             (iup:dialog vbox :title "Halfclone: Halftone-like Image Viewer")))
      (setf (iup:attribute-handle dialog :menu) menu)
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (main)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (main))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (iup:with-iup ()
    (let* ((channel (lparallel:make-channel))
           (pathnames (uiop:directory-files
                       "/usr/share/backgrounds/"
                       (make-pathname :name :wild :type "jpg"))))
      (let* ((progress (iup:progress-dialog
                        :count (length pathnames)
                        :cancel_cb (lambda (handle)
                                     iup:+close+)))
             (progress-timer (iup:timer
                              :time 500
                              :action_cb (lambda (handle)
                                           (multiple-value-bind
                                                 (result result-p)
                                               (lparallel:try-receive-result channel :timeout 0.1)
                                             (when result-p
                                               (setf (iup:attribute progress :inc) nil)))
                                           iup:+default+)))))
      (dolist (pathname pathnames)
        (lparallel:submit-task
         channel
         #'(lambda (pathname)
             (with-open-file (stream pathname)
               (file-length stream)))
         pathname))
      (loop :repeat (length pathnames)
            :do (print "getting a reuslt")
            :collect (lparallel:receive-result channel)))))


