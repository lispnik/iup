(defpackage #:iup-cffi
  (:use #:common-lisp
        #:alexandria)
  (:import-from #:tecgraf-base #:ihandle))

(in-package #:iup-cffi)

(cffi:define-foreign-library iup
  (:unix "libiup.so")
  (:windows "iup.dll")
  (t (:default "iup")))

(cffi:use-foreign-library iup)

(defmethod print-object ((object ihandle) stream)
  (print-unreadable-object (object stream :type t)
    (let ((pointer (pffft:pointer object)))
      (if (cffi:null-pointer-p pointer)
          (write-string "NULL" stream)
          (format stream "~S ~X"
                  (iup-cffi::%iup-get-class-name object)
                  (cffi:pointer-address pointer))))))

(defun attr-name-from-c (value)
  (if (cffi:null-pointer-p value) nil value))

(defun attr-name-to-c (value)
  (if value
      (etypecase value
        (symbol (symbol-name value))
        (string value)
        (integer (write-to-string value)))
      (cffi:null-pointer)))

(cffi:defctype attr-name
    (:wrapper :string
     :from-c attr-name-from-c
     :to-c attr-name-to-c))

(cffI:defcfun (%iup-open "IupOpen") :int
  (argv :pointer)
  (argc :pointer))

(cffi:defcfun (%iup-close "IupClose") :void)
(cffi:defcfun (%iup-main-loop "IupMainLoop") :int)
(cffi:defcfun (%iup-loop-step "IupLoopStep") :int)
(cffi:defcfun (%iup-loop-step-wait "IupLoopStepWait") :int)
(cffi:defcfun (%iup-main-loop-level "IupMainLoopLevel") :int)
(cffi:defcfun (%iup-flush "IupFlush") :void)
(cffi:defcfun (%iup-exit-loop "IupExitLoop") :void)

(cffi:defcfun (%iup-record-input "IupRecordInput") :int
  (filename :string)
  (mode :int))

(cffi:defcfun (%iup-play-input "IupPlayInput") :int
  (filename :string))

(cffi:defcfun (%iup-update "IupUpdate") :void
  (handle ihandle))

(cffi:defcfun (%iup-update-children "IupUpdateChildren") :void
  (handle ihandle))

(cffi:defcfun (%iup-redraw "IupRedraw") :void
  (handle ihandle)
  (children :boolean))

(cffi:defcfun (%iup-refresh "IupRefresh") :void
  (handle ihandle))

(cffi:defcfun (%iup-refresh-children "IupRefreshChildren") :void
  (handle ihandle))

(cffi:defcfun (%iup-version "IupVersion") :string)
(cffi:defcfun (%iup-version-date "IupVersionDate") :string)
(cffi:defcfun (%iup-version-number "IupVersionNumber") :int)

(cffi:defcfun (%iup-destroy "IupDestroy") :void
  (handle ihandle))

(cffi:defcfun (%iup-detach "IupDetach") :void
  (handle ihandle))

(cffi:defcfun (%iup-append "IupAppend") ihandle
  (handle ihandle)
  (child ihandle))

(cffi:defcfun (%iup-insert "IupInsert") ihandle
  (handle ihandle)
  (ref-child ihandle)
  (child ihandle))

(cffi:defcfun (%iup-get-child "IupGetChild") ihandle
  (handle ihandle)
  (pos :int))

(cffi:defcfun (%iup-get-child-pos "IupGetChildPos") :int
  (handle ihandle)
  (child ihandle))

(cffi:defcfun (%iup-get-child-count "IupGetChildCount") :int
  (handle ihandle))

(cffi:defcfun (%iup-get-next-child "IupGetNextChild")  ihandle
  (handle ihandle)
  (child ihandle))

(cffi:defcfun (%iup-get-brother "IupGetBrother") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-get-parent "IupGetParent") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-get-dialog "IupGetDialog") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-get-dialog-child "IupGetDialogChild") ihandle
  (handle ihandle)
  (name :string))

(cffi:defcfun (%iup-reparent "IupReparent") :int
  (handle ihandle)
  (new-parent ihandle)
  (ref-child ihandle))

(cffi:defcfun (%iup-popup "IupPopup") :int
  (handle ihandle)
  (x :int)
  (y :int))

(cffi:defcfun (%iup-show "IupShow") :int
  (handle ihandle))

(cffi:defcfun (%iup-show-xy "IupShowXY") :int
  (handle ihandle)
  (x :int)
  (y :int))

(cffi:defcfun (%iup-hide "IupHide") :int
  (handle ihandle))

(cffi:defcfun (%iup-map "IupMap") :int
  (handle ihandle))

(cffi:defcfun (%iup-unmap "IupUnmap") :void
  (handle ihandle))

(cffi:defcfun (%iup-get-all-attributes "IupGetAllAttributes") :int
  (handle ihandle)
  (names :pointer)
  (n :int))

(cffi:defcfun (%iup-reset-attribute "IupResetAttribute") :void
  (handle ihandle)
  (name attr-name))

;;; attribute

(cffi:defcfun (%iup-set-str-attribute "IupSetStrAttribute") :void
  (handle ihandle)
  (name attr-name)
  (value :string))

(cffi:defcfun (%iup-get-attribute "IupGetAttribute") :string
  (handle ihandle)
  (name attr-name))

;;; attribute-id

(cffi:defcfun (%iup-set-str-attribute-id "IupSetStrAttributeId") :void
  (handle ihandle)
  (name attr-name)
  (id :int)
  (value :string))

(cffi:defcfun (%iup-get-attribute-id "IupGetAttributeId") :void
  (handle ihandle)
  (name attr-name)
  (id :int))

;;; attribute-id-2

(cffi:defcfun (%iup-set-str-attribute-id-2 "IupSetStrAttributeId2") :void
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int)
  (value :string))

(cffi:defcfun (%iup-get-attribute-id-2 "IupGetAttributeId2") :string
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int))


(cffi:defcfun (%iup-set-str-global "IupSetStrGlobal") :void
  (name attr-name)
  (value :pointer))

(cffi:defcfun (%iup-get-global "IupGetGlobal") :pointer
  (name attr-name))

(cffi:defcfun (%iup-set-focus "IupSetFocus") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-get-focus "IupGetFocus") ihandle)

(cffi:defcfun (%iup-previous-field "IupPreviousField") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-next-field "IupNextField") ihandle
  (handle ihandle))

(cffi:defcfun (%iup-set-callback "IupSetCallback") :pointer
  (handle ihandle)
  (name attr-name)
  (func :pointer))

(cffi:defcfun (%iup-get-callback "IupGetCallback") :pointer
  (handle ihandle)
  (name attr-name))


(cffi:defcfun (%iup-get-function "IupGetFunction") :pointer
  (name attr-name))

(cffi:defcfun (%iup-set-function "IupSetFunction") :pointer
  (name attr-name)
  (func :pointer))

(cffi:defcfun (%iup-get-handle "IupGetHandle") ihandle
  (name :string))

(cffi:defcfun (%iup-set-handle "IupSetHandle") ihandle
  (name :string)
  (handle ihandle))

;; int       IupGetAllNames  (char** names, int n);
;; int       IupGetAllDialogs(char** names, int n);
;; char*     IupGetName      (Ihandle* ih);

(cffi:defcfun (%iup-set-attribute-handle "IupSetAttributeHandle") :void
  (handle ihandle)
  (name attr-name)
  (other-handle ihandle))

(cffi:defcfun (%iup-get-attribute-handle "IupGetAttributeHandle") ihandle
  (handle ihandle)
  (name attr-name))

;; Ihandle*  IupGetAttributeHandle(Ihandle* ih, const char* name);
;; void      IupSetAttributeHandleId(Ihandle* ih, const char* name, int id, Ihandle* ih_named);
;; Ihandle*  IupGetAttributeHandleId(Ihandle* ih, const char* name, int id);
;; void      IupSetAttributeHandleId2(Ihandle* ih, const char* name, int lin, int col, Ihandle* ih_named);
;; Ihandle*  IupGetAttributeHandleId2(Ihandle* ih, const char* name, int lin, int col);

(cffi:defcfun (%iup-get-class-name "IupGetClassName") :string
  (handle ihandle))

(cffi:defcfun (%iup-get-class-type "IupGetClassType") :string
  (handle ihandle))

(cffi:defcfun (%iup-get-all-classes "IupGetAllClasses") :int
  (names :pointer)
  (n :int))

(cffi:defcfun (%iup-get-class-attributes "IupGetClassAttributes") :int
  (classname :string)
  (names :pointer)
  (n :int))

(cffi:defcfun (%iup-get-class-callbacks "IupGetClassCallbacks") :int
  (classname :string)
  (names :pointer)
  (n :int))

(cffi:defcfun (%iup-save-class-attributes "IupSaveClassAttributes") :void
  (handle ihandle))

(cffi:defcfun (%iup-copy-class-attributes "IupCopyClassAttributes") :void
  (source-handle ihandle)
  (destination-handle ihandle))

(cffi:defcfun (%iup-set-class-default-attribute "IupSetClassDefaultAttribute") :void
  (classname :string)
  (name attr-name)
  (value :string))

;; int       IupClassMatch(Ihandle* ih, const char* classname);

(cffi:defcfun (%iup-create "IupCreate") ihandle
  (classname :string))

;; /* IupImage utility */
;; int IupSaveImageAsText(Ihandle* ih, const char* file_name, const char* format, const char* name);

;; /* IupText and IupScintilla utilities */
;; void  IupTextConvertLinColToPos(Ihandle* ih, int lin, int col, int *pos);
;; void  IupTextConvertPosToLinCol(Ihandle* ih, int pos, int *lin, int *col);

;; /* IupText, IupList, IupTree, IupMatrix and IupScintilla utility */
;; int   IupConvertXYToPos(Ihandle* ih, int x, int y);

;; /* IupTree utilities */
;; int   IupTreeSetUserId(Ihandle* ih, int id, void* userid);
;; void* IupTreeGetUserId(Ihandle* ih, int id);
;; int   IupTreeGetId(Ihandle* ih, void *userid);
;; void  IupTreeSetAttributeHandle(Ihandle* ih, const char* name, int id, Ihandle* ih_named); /* deprecated, use IupSetA

(cffi:defcfun (%iup-file-dlg "IupFileDlg") ihandle)
(cffi:defcfun (%iup-message-dlg "IupMessageDlg") ihandle)
(cffi:defcfun (%iup-color-dlg "IupColorDlg") ihandle)
(cffi:defcfun (%iup-font-dlg "IupFontDlg") ihandle)
(cffi:defcfun (%iup-progress-dlg "IupProgressDlg") ihandle)

(cffi:defcfun (%iup-message "IupMessage") :void
  (title :string)
  (message :string))

(cffi:defcfun (%iup-message-error "IupMessageError") :void
  (parent ihandle)
  (message :string))

(cffi:defcfun (%iup-message-alarm "IupMessageAlarm") :int
  (parent ihandle)
  (title :string)
  (message :string)
  (buttons :string))

(cffi:defcfun (%iup-alarm "IupAlarm") :int
  (title :string)
  (message :string)
  (button1 :string)
  (button2 :string)
  (button3 :string))

;; int  IupListDialog(int type, const char *title, int size, const char** list,
;;                    int op, int max_col, int max_lin, int* marks);
;; int  IupGetText(const char* title, char* text, int maxsize);
;; int  IupGetColor(int x, int y, unsigned char* r, unsigned char* g, unsigned char* b);

;; typedef int (*Iparamcb)(Ihandle* dialog, int param_index, void* user_data);
;; int IupGetParam(const char* title, Iparamcb action, void* user_data, const char* format, ...);
;; int IupGetParamv(const char* title, Iparamcb action, void* user_data, const char* format, int param_count, int param_extra, void** param_data);
;; Ihandle* IupParam(const char* format);
;; Ihandle*  IupParamBox(Ihandle* param, ...);
;; Ihandle*  IupParamBoxv(Ihandle* *param_array);


;; /************************************************************************/
;; /*               SHOW_CB Callback Values                                */
;; /************************************************************************/
;; enum{IUP_SHOW, IUP_RESTORE, IUP_MINIMIZE, IUP_MAXIMIZE, IUP_HIDE};

;; /************************************************************************/
;; /*               SCROLL_CB Callback Values                              */
;; /************************************************************************/
;; enum{IUP_SBUP,   IUP_SBDN,    IUP_SBPGUP,   IUP_SBPGDN,    IUP_SBPOSV, IUP_SBDRAGV, 
;;      IUP_SBLEFT, IUP_SBRIGHT, IUP_SBPGLEFT, IUP_SBPGRIGHT, IUP_SBPOSH, IUP_SBDRAGH};

;; /************************************************************************/
;; /*               Mouse Button Values and Macros                         */
;; /************************************************************************/
;; #define IUP_BUTTON1   '1'
;; #define IUP_BUTTON2   '2'
;; #define IUP_BUTTON3   '3'
;; #define IUP_BUTTON4   '4'
;; #define IUP_BUTTON5   '5'

;; #define iup_isshift(_s)    (_s[0]=='S')
;; #define iup_iscontrol(_s)  (_s[1]=='C')
;; #define iup_isbutton1(_s)  (_s[2]=='1')
;; #define iup_isbutton2(_s)  (_s[3]=='2')
;; #define iup_isbutton3(_s)  (_s[4]=='3')
;; #define iup_isdouble(_s)   (_s[5]=='D')
;; #define iup_isalt(_s)      (_s[6]=='A')
;; #define iup_issys(_s)      (_s[7]=='Y')
;; #define iup_isbutton4(_s)  (_s[8]=='4')
;; #define iup_isbutton5(_s)  (_s[9]=='5')

;; /************************************************************************/
;; /*                   IupGetParam Callback situations                    */
;; /************************************************************************/
;; #define IUP_GETPARAM_BUTTON1 -1
;; #define IUP_GETPARAM_INIT    -2
;; #define IUP_GETPARAM_BUTTON2 -3
;; #define IUP_GETPARAM_BUTTON3 -4
;; #define IUP_GETPARAM_CLOSE   -5
;; #define IUP_GETPARAM_MAP     -6
;; #define IUP_GETPARAM_OK     IUP_GETPARAM_BUTTON1
;; #define IUP_GETPARAM_CANCEL IUP_GETPARAM_BUTTON2
;; #define IUP_GETPARAM_HELP   IUP_GETPARAM_BUTTON3

(cffi:defcfun (%iup-config "IupConfig") ihandle)

(cffi:defcfun (%iup-config-load "IupConfigLoad") :int
  (handle ihandle))

(cffi:defcfun (%iup-config-save "IupConfigSave") :int
  (handle ihandle))

(cffi:defcfun (%iup-config-set-variable-str "IupConfigSetVariableStr") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (value :string))

(cffi:defcfun (%iup-config-set-variable-str-id "IupConfigSetVariableStrId") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (value :string))

(cffi:defcfun (%iup-config-set-variable-int "IupConfigSetVariableInt") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (value :int))

(cffi:defcfun (%iup-config-set-variable-int-id "IupConfigSetVariableIntId") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (value :int))

(cffi:defcfun (%iup-config-set-variable-double "IupConfigSetVariableDouble") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (value :double))

(cffi:defcfun (%iup-config-set-variable-double-id "IupConfigSetVariableDoubleId") :void
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (value :double))

(cffi:defcfun (%iup-config-get-variable-str "IupConfigGetVariableStr") :string
  (handle ihandle)
  (group :string)
  (key :string))

(cffi:defcfun (%iup-config-get-variable-str-id "IupConfigGetVariableStrId") :string
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int))

(cffi:defcfun (%iup-config-get-variable-int "IupConfigGetVariableInt") :int
  (handle ihandle)
  (group :string)
  (key :string))

(cffi:defcfun (%iup-config-get-variable-int-id "IupConfigGetVariableIntId") :int
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int))

(cffi:defcfun (%iup-config-get-variable-double "IupConfigGetVariableDouble") :double
  (handle ihandle)
  (group :string)
  (key :string))

(cffi:defcfun (%iup-config-get-variable-double-id "IupConfigGetVariableDoubleId") :double
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int))

(cffi:defcfun (%iup-config-get-variable-str-def "IupConfigGetVariableStrDef") :string
  (handle ihandle)
  (group :string)
  (key :string)
  (default :string))

(cffi:defcfun (%iup-config-get-variable-str-id-def "IupConfigGetVariableStrIdDef") :string
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (default :string))

(cffi:defcfun (%iup-config-get-variable-int-def "IupConfigGetVariableIntDef") :int
  (handle ihandle)
  (group :string)
  (key :string)
  (default :int))

(cffi:defcfun (%iup-config-get-variable-int-id-def "IupConfigGetVariableIntIdDef") :int
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (default :int))

(cffi:defcfun (%iup-config-get-variable-double-def "IupConfigGetVariableDoubleDef") :double
  (handle ihandle)
  (group :string)
  (key :string)
  (default :double))

(cffi:defcfun (%iup-config-get-variable-double-id-def "IupConfigGetVariableDoubleIdDef") :double
  (handle ihandle)
  (group :string)
  (key :string)
  (id :int)
  (default :double))

(cffi:defcfun (%iup-config-copy "IupConfigCopy") :void
  (from-handle ihandle)
  (to-handle ihandle)
  (exclude-prefix :pointer))            ;FIXME in wrapper, can be nullable

;; void IupConfigSetListVariable(Ihandle* ih, const char *group, const char* key, const char* value, int add);

;; void IupConfigRecentInit(Ihandle* ih, Ihandle* menu, Icallback recent_cb, int max_recent);

(cffi:defcfun (%iup-config-recent-update "IupConfigRecentUpdate") :void
  (handle ihandle)
  (filename :string))

(cffi:defcfun (%iup-config-dialog-show "IupConfigDialogShow") :void
  (handle ihandle)
  (dialog-handle ihandle)
  (name :string))

(cffi:defcfun (%iup-config-dialog-closed "IupConfigDialogClosed") :void
  (handle ihandle)
  (dialog-handle ihandle)
  (name :string))

(cffi:defcfun (%iup-layout-dialog "IupLayoutDialog") ihandle
  (dialog ihandle))

(cffi:defcfun (%iup-element-properties-dialog "IupElementPropertiesDialog") ihandle
  (element ihandle))

(cffi:defcfun (%iup-image "IupImage") ihandle
  (width :int)
  (height :int)
  (pixels :pointer))

(cffi:defcfun (%iup-image-rgb "IupImageRGB") ihandle
  (width :int)
  (height :int)
  (pixels :pointer))

(cffi:defcfun (%iup-image-rgba "IupImageRGBA") ihandle
  (width :int)
  (height :int)
  (pixels :pointer))

#+windows
(cffi:define-foreign-library iup-win32-user32
  (:windows "user32.dll"))

#+windows
(cffi:use-foreign-library iup-win32-user32)

#+windows
(cffi:defcfun (%set-process-dpi-aware "SetProcessDPIAware") :boolean)
