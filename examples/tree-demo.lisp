;; translated from http://webserver2.tecgraf.puc-rio.br/iup/examples/C/tree.c

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup")))

(defpackage #:iup-examples.tree-demo
  (:use #:common-lisp)
  (:export #:tree-demo))

(in-package #:iup-examples.tree-demo)

(defun addleaf (handle)
  (declare (ignore handle))
  (let* ((tree (iup:handle "tree"))
         (id (iup:attribute tree :value 'number)))
    (setf (iup:attribute-id tree :addleaf id) ""))
  iup:+default+)

(defun addbranch (handle)
  (declare (ignore handle))
  (let* ((tree (iup:handle "tree"))
         (id (iup:attribute tree :value 'number)))
    (setf (iup:attribute-id tree :addbranch id) ""))
  iup:+default+)

(defun removenode (handle)
  (declare (ignore handle))
  (let ((tree (iup:handle "tree")))
    (setf (iup:attribute tree :delnode) :marked))
  iup:+default+)

(defun renamenode (handle)
  (declare (ignore handle))
  iup:+default+)

(defun executeleaf-callback (handle id)
  (declare (ignore handle))
  (format t "executeleaf-callback id ~A~%" id)
  iup:+default+)

(defun rename-callback (handle id name)
  (declare (ignore handle))
  (format t "rename-callback id ~A name ~A~%" id name)
  (if (string= "fool" name)
      iup:+ignore+
      iup:+default+))

(defun branchopen-callback (handle id)
  (declare (ignore handle))
  (format t "branchopen-callback id ~A~%" id)
  iup:+default+)

(defun branchclose-callback (handle id)
   (declare (ignore handle))
  (format t "branchclose-callback id ~A~%" id)
 iup:+default+)

(defun dragdrop-callback (handle drag-id drop-id is-shift is-control)
  (declare (ignore handle))
  (format t "dragdrop-callback drag-id ~A drop-id ~A shift ~A control ~A~%"
          drag-id drop-id is-shift is-control)
  iup:+default+)

(defun k-any-callback (handle c)
  (when (= c iup:+k_del+)
    (setf (iup:attribute handle :delnode) :marked))
  iup:+default+)

(defun selectnode (handle)
  (let ((tree (iup:handle "tree")))
    (setf (iup:attribute tree :value) (iup:attribute handle :title)))
  iup:+default+)
    
(defun rightclick-callback (handle id)
  (let ((menu
          (iup:menu (list (iup:item :title "Add Leaf" :action 'addleaf)
                          (iup:item :title "Add Branch" :action 'addbranch)
                          (iup:item :title "Rename Node" :action 'renamenode)
                          (iup:item :title "Remove Node" :action 'removenode)
                          (iup:sub-menu
                           (iup:menu (list (iup:item :title "ROOT" :action 'selectnode)
                                           (iup:item :title "LAST" :action 'selectnode)
                                           (iup:item :title "PGUP" :action 'selectnode)
                                           (iup:item :title "PGDN" :action 'selectnode)
                                           (iup:item :title "NEXT" :action 'selectnode)
                                           (iup:item :title "PREVIOUS" :action 'selectnode)
                                           (iup:separator)
                                           (iup:item :title "INVERT" :action 'selectnode)
                                           (iup:item :title "BLOCK" :action 'selectnode)
                                           (iup:item :title "CLEAR" :action 'selectnode)
                                           (iup:item :title "MARKALL" :action 'selectnode)
                                           (iup:item :title "INVERTALL" :action 'selectnode)))
                           :title "Selection")))))
    (setf (iup:attribute handle :value) id)
    (iup:popup menu iup:+mousepos+ iup:+mousepos+)
    (iup:destroy menu))
    iup:+default+)

(defun tree-demo ()
  (iup:with-iup ()
    (let* ((tree (iup:tree 
                  :executeleaf_cb 'executeleaf-callback
                  :rename_cb 'rename-callback
                  :branchclose_cb 'branchclose-callback
                  :branchopen_cb 'branchopen-callback
                  :dragdrop_cb 'dragdrop-callback
                  :rightclick_cb 'rightclick-callback
                  :k_any 'k-any-callback
                  ;; :ctrl :yes
                  ;; :shift :yes
                  ;; :addexpanded :no
                  ;; :showdragdrop :yes
                  :showrename :yes))
           (box (iup:vbox (list tree)
                          :margin "20x20"))
           (dialog (iup:dialog box :title "Tree Demo")))
      (setf (iup:handle "tree") tree)
      (iup:show dialog)
      (setf (iup:attribute tree :title) "Figures"
            (iup:attribute-id tree :addbranch 0) "3D"
            (iup:attribute-id tree :addbranch 0) "2D"
            (iup:attribute-id tree :addbranch 1) "parallelogram"
            (iup:attribute-id tree :addleaf 2) "diamond"
            (iup:attribute-id tree :addleaf 2) "square"
            (iup:attribute-id tree :addbranch 1) "triangle"
            (iup:attribute-id tree :addleaf 2) "scalene"
            (iup:attribute-id tree :addleaf 2) "isosceles"
            (iup:attribute-id tree :addleaf 2) "equilateral"
            (iup:attribute tree :value) 6)

      (iup:main-loop))))

#-sbcl (tree-demo)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (tree-demo))
