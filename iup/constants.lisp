(in-package #:iup)

(export '(+error+
	  +noerror+
	  +opened+
	  +invalid+
	  +invalid-id+
	  ;; callback returns
	  +ignore+
	  +default+
	  +close+
	  +continue+
	  ;;
	  +center+
	  +left+
	  +right+
	  +mousepos+
	  +current+
	  +centerparent+
	  +top+
	  +bottom+
	  ;;
	  +primary+
	  +secondary+
	  ;;
	  +recbinary+
	  +rectext+
	  ))

(defconstant +error+      1)
(defconstant +noerror+    0)
(defconstant +opened+     -1)
(defconstant +invalid+    -1)
(defconstant +invalid-id+ -10)

(defconstant +ignore+   -1)
(defconstant +default+  -2)
(defconstant +close+    -3)
(defconstant +continue+ -4)

(defconstant +center+       #xffff)
(defconstant +left+         #xfffe)
(defconstant +right+        #xffd)
(defconstant +mousepos+     #xfffc)
(defconstant +current+      #xfffb)
(defconstant +centerparent+ #xfffa)
(defconstant +top+          +left+)
(defconstant +bottom+       +right+)

(defconstant +primary+   -1)
(defconstant +secondary+ -2)

(defconstant +recbinary+ 0)
(defconstant +rectext+   1)
