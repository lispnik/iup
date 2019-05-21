(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "torrents")))

(defpackage #:iup-examples.torrents
  (:use #:common-lisp)
  (:export #:torrents))

(in-package #:iup-examples.torrents)

(defun torrents ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((search-text
             (iup:text :expand :horizontal))
           (results-matrix
             (iup-controls:matrix-ex :expand :yes
                                     :numcol 4
                                     :resizematrix :yes
                                     :readonly :yes
                                     :map_cb (lambda (handle)
                                               (setf (iup:attribute handle "0:1") "Title"
                                                     (iup:attribute handle "0:2") "Seeders"
                                                     (iup:attribute handle "0:3") "Leechers"
                                                     (iup:attribute handle "0:4") "Size")
                                               (setf (iup:attribute-id handle :width 1) 200)
                                               iup:+default+)))
           (search-button
             (iup:button :title "Search"
                         :action (lambda (handle)
                                   (let ((torrents (torrents:search-torrents (iup:attribute search-text :value))))
                                     (setf (iup:attribute results-matrix :addlin) (format nil "1-~A" (length torrents)))
                                     (loop for torrent in torrents
                                           for i from 1
                                           do (loop for accessor in '(torrents:title torrents:seeders torrents:leechers torrents:size)
                                                    for j from 1
                                                    for cell = (format nil "~A:~A" i j)
                                                    do (setf (iup:attribute results-matrix cell)
                                                             (funcall accessor torrent)))))
                                   iup:+default+)))
           (search-box (iup:hbox (list search-text search-button)))
           (open-button (iup:button :title "Open"
                                    :action (lambda (handle)
                                              iup:+default+)))
           (download-button (iup:button :title "Download"
                                        :action (lambda (handle)
                                                  iup:+default+)))
           (bottom-box (iup:hbox (list open-button download-button)))
           (vbox (iup:vbox (list search-box results-matrix bottom-box)))
           (dialog (iup:dialog vbox :title "Torrents" :shrink :yes)))
      (iup:show dialog)

      (iup:main-loop))))

#-sbcl (torrents)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (torrents))
