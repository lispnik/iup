(defpackage #:iup
  (:use #:common-lisp
	#:cffi
	#:alexandria)
  (:export #:+center+
	   #:+left+
	   #:+right+
	   #:+mousepos+
	   #:+current+
	   #:+centerparent+
	   #:+top+
	   #:+bottom+
	   #:open
	   #:close
	   #:image-lib-open
	   #:main-loop
	   #:loop-step
	   #:loop-step-wait
	   #:main-loop-level
	   #:flush
	   #:exit-loop
	   #:record-input
	   #:play-input
	   #:update
	   #:update-children
	   #:redraw
	   #:refresh
	   #:refresh-children
	   #:version
	   #:version-date
	   #:version-number
	   #:destroy
	   #:detach
	   #:append
	   #:insert
	   #:get-child
	   #:get-child-pos
	   #:get-child-count
	   #:get-next-child
	   #:get-brother
	   #:get-parent
	   #:get-dialog
	   #:get-dialog-child
	   #:reparent
	   #:popup
	   #:show
	   #:show-xy
	   #:hide
	   #:map
	   #:unmap
	   #:attribute-handle
	   #:reset-attribute
	   #:set-global
	   #:get-global
	   #:set-focus
	   #:get-focus
	   #:previous-field
	   #:next-field

	   ;; TODO
	   ;; set-callback
	   ;; get-callback
	   ;; set-function
	   ;; get-function
	   ;; get-handle
	   ;; set-handle
	   ;; get-all-names
	   ;; get-all-dialogs
	   ;; get-name
	   
	   #:image
	   #:image-rgb
	   #:image-rgba

	   #:file-dialog
	   #:message-dialog
	   #:color-dialog
	   #:font-dialog
	   #:message
	   #:message-error
	   #:message-alarm
	   #:alarm
	   #:config
	   #:config-load
	   #:config-save
	   #:config-dialog-show
	   #:config-dialog-closed

	   #:all-classes
	   #:class-attributes
	   #:class-callbacks
	   #:class-name
	   #:class-type
	   #:save-class-attributes
	   #:copy-class-attributes
	   
	   #:with-iup
	   #:attribute
	   #:attribute-id
	   #:attribute-id-2
	   #:attributes
	   #:callback
	   #:handle
	   #:platform)
  (:shadow #:open
	   #:close
	   #:map
	   #:append
	   #:list
	   #:fill
	   #:space
	   #:class-name)
  (:import-from #:iup-utils
		#:alias))
