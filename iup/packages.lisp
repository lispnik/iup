(defpackage #:iup
  (:use #:common-lisp #:alexandria)
  (:export #:open
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
           #:version-show
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

	   #:class-attributes
	   #:class-callbacks
	   #:all-classes
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
	   #:platform

           #:image-get-handle
	   #:layout-dialog)
  (:shadow #:open
	   #:close
	   #:map
	   #:append
	   #:list
	   #:fill
	   #:space
	   #:class-name)
  (:import-from #:serapeum
		#:defalias))

