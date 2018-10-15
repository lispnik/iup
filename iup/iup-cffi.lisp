(in-package #:iup-cffi)

(define-foreign-type ihandle ()
  ()
  (:actual-type :pointer)
  (:simple-parser ihandle))

(defmethod translate-from-foreign (value (type ihandle))
  (unless (null-pointer-p value)
    value))

(defmethod translate-to-foreign (value (type ihandle))
  (or value (null-pointer)))

(defun attr-name-from-c (value)
  (if (null-pointer-p value) nil value))

(defun attr-name-to-c (value)
  (if value
      (etypecase value
	(symbol (symbol-name value))
	(string value))
      (null-pointer)))

(defctype attr-name
    (:wrapper :string
     :from-c attr-name-from-c
     :to-c attr-name-to-c))

(defcfun (%iup-open "IupOpen") :int
  (argv :pointer)
  (argc :pointer))

(defcfun (%iup-close "IupClose") :void)
(defcfun (%iup-image-lib-open "IupImageLibOpen" :library iupimglib) :void)
(defcfun (%iup-main-loop "IupMainLoop") :int)
(defcfun (%iup-loop-step "IupLoopStep") :int)
(defcfun (%iup-loop-step-wait "IupLoopStepWait") :int)
(defcfun (%iup-main-loop-level "IupMainLoopLevel") :int)
(defcfun (%iup-flush "IupFlush") :void)
(defcfun (%iup-exit-loop "IupExitLoop") :void)

(defcfun (%iup-record-input "IupRecordInput") :int
  (filename :string)
  (mode :int))

(defcfun (%iup-play-input "IupPlayInput") :int
  (filename :string))

(defcfun (%iup-update "IupUpdate") :void
  (handle ihandle))

(defcfun (%iup-update-children "IupUpdateChildren") :void
  (handle ihandle))

(defcfun (%iup-redraw "IupRedraw") :void
  (handle ihandle)
  (children :int))

(defcfun (%iup-refresh "IupRefresh") :void
  (handle ihandle))

(defcfun (%iup-refresh-children "IupRefreshChildren") :void
  (handle ihandle))

(defcfun (%iup-version "IupVersion") :string)
(defcfun (%iup-version-date "IupVersionDate") :string)
(defcfun (%iup-version-number "IupVersionNumber") :int)

(defcfun (%iup-destroy "IupDestroy") :void
  (handle ihandle))

(defcfun (%iup-detach "IupDetach") :void
  (handle ihandle))

(defcfun (%iup-append "IupAppend") ihandle
  (handle ihandle)
  (child ihandle))

(defcfun (%iup-insert "IupInsert") ihandle
  (handle ihandle)
  (ref-child ihandle)
  (child ihandle))

(defcfun (%iup-get-child "IupGetChild") ihandle
  (handle ihandle)
  (pos :int))

(defcfun (%iup-get-child-pos "IupGetChildPos") :int
  (handle ihandle)
  (child ihandle))

(defcfun (%iup-get-child-count "IupGetChildCount") :int
  (handle ihandle))

(defcfun (%iup-get-next-child "IupGetNextChild")  ihandle
  (handle ihandle)
  (child ihandle))

(defcfun (%iup-get-brother "IupGetBrother") ihandle
  (handle ihandle))

(defcfun (%iup-get-parent "IupGetParent") ihandle
  (handle ihandle))

(defcfun (%iup-get-dialog "IupGetDialog") ihandle
  (handle ihandle))

(defcfun (%iup-get-dialog-child "IupGetDialogChild") ihandle
  (handle ihandle)
  (name :string))

(defcfun (%iup-reparent "IupReparent") :int
  (handle ihandle)
  (new-parent ihandle)
  (ref-child ihandle))

(defcfun (%iup-popup "IupPopup") :int
  (handle ihandle)
  (x :int)
  (y :int))

(defcfun (%iup-show "IupShow") :int
  (handle ihandle))

(defcfun (%iup-show-xy "IupShowXY") :int
  (handle ihandle)
  (x :int)
  (y :int))

(defcfun (%iup-hide "IupHide") :int
  (handle ihandle))

(defcfun (%iup-map "IupMap") :int
  (handle ihandle))

(defcfun (%iup-unmap "IupUnmap") :void
  (handle ihandle))

(defcfun (%iup-reset-attribute "IupResetAttribute") :void
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-get-all-attributes "IupGetAllAttributes") :int
  (handle ihandle)
  (names :pointer)
  (n :int))

(defcfun (%iup-set-str-attribute "IupSetStrAttribute") :void
  (handle ihandle)
  (name attr-name)
  (value :string))

(defcfun (%iup-get-attribute "IupGetAttribute") :string
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-get-pointer-attribute "IupGetAttribute") :pointer
  ;; same thing, no conversion
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-set-str-attribute-id "IupSetStrAttributeId") :void
  (handle ihandle)
  (name attr-name)
  (id :int)
  (value :string))

(defcfun (%iup-get-attribute-id-2 "IupGetAttributeId2") :string
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int))

(defcfun (%iup-set-str-attribute-id-2 "IupSetStrAttributeId2") :void
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int)
  (value :string))

(defcfun (%iup-set-int-attribute "IupSetInt") :void
  (handle ihandle)
  (name attr-name)
  (value :int))

(defcfun (%iup-set-float-attribute "IupSetFloat") :void
  (handle ihandle)
  (name attr-name)
  (value :float))

(defcfun (%iup-set-double-attribute "IupSetDouble") :void
  (handle ihandle)
  (name attr-name)
  (value :double))

(defcfun (%iup-get-int-attribute "IupGetInt") :int
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-get-float-attribute "IupGetFloat") :float
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-get-double-attribute "IupGetDouble") :double
  (handle ihandle)
  (name attr-name))

(defcfun (%iup-set-int-attribute-id-2 "IupSetIntId2") :void
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int)
  (value :int))

(defcfun (%iup-set-float-attribute-id-2 "IupSetFloat") :void
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int)
  (value :float))

(defcfun (%iup-set-double-attribute-id-2 "IupSetDouble") :void
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int)
  (value :double))

(defcfun (%iup-get-int-attribute-id-2 "IupGetInt") :int
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int))

(defcfun (%iup-get-float-attribute-id-2  "IupGetFloat") :float
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int))

(defcfun (%iup-get-double-attribute-id-2 "IupGetDouble") :double
  (handle ihandle)
  (name attr-name)
  (line :int)
  (column :int))

(defcfun (%iup-set-str-global "IupSetStrGlobal") :void
  (name attr-name)
  (value :string))

(defcfun (%iup-get-global "IupGetGlobal") :string
  (name attr-name))

(defcfun (%iup-set-focus "IupSetFocus") ihandle
  (handle ihandle))

(defcfun (%iup-get-focus "IupGetFocus") ihandle)

(defcfun (%iup-previous-field "IupPreviousField") ihandle
  (handle ihandle))

(defcfun (%iup-next-field "IupNextField") ihandle
  (handle ihandle))

(defcfun (%iup-set-callback "IupSetCallback") :pointer
  (handle ihandle)
  (name attr-name)
  (func :pointer))

(defcfun (%iup-get-callback "IupGetCallback") :pointer
  (handle ihandle)
  (name attr-name))


(defcfun (%iup-get-function "IupGetFunction") :pointer
  (name attr-name))

(defcfun (%iup-set-function "IupSetFunction") :pointer
  (name attr-name)
  (func :pointer))

(defcfun (%iup-get-handle "IupGetHandle") ihandle
  (name :string))

(defcfun (%iup-set-handle "IupSetHandle") ihandle
  (name :string)
  (handle ihandle))

;; int       IupGetAllNames  (char** names, int n);
;; int       IupGetAllDialogs(char** names, int n);
;; char*     IupGetName      (Ihandle* ih);

(defcfun (%iup-set-attribute-handle "IupSetAttributeHandle") :void
  (handle ihandle)
  (name attr-name)
  (other-handle ihandle))

(defcfun (%iup-get-attribute-handle "IupGetAttributeHandle") ihandle
  (handle ihandle)
  (name attr-name))

;; Ihandle*  IupGetAttributeHandle(Ihandle* ih, const char* name);
;; void      IupSetAttributeHandleId(Ihandle* ih, const char* name, int id, Ihandle* ih_named);
;; Ihandle*  IupGetAttributeHandleId(Ihandle* ih, const char* name, int id);
;; void      IupSetAttributeHandleId2(Ihandle* ih, const char* name, int lin, int col, Ihandle* ih_named);
;; Ihandle*  IupGetAttributeHandleId2(Ihandle* ih, const char* name, int lin, int col);

(defcfun (%iup-get-class-name "IupGetClassName") :string
  (handle ihandle))

(defcfun (%iup-get-class-type "IupGetClassType") :string
  (handle ihandle))

(defcfun (%iup-get-all-classes "IupGetAllClasses") :int
  (names :pointer)
  (n :int))

(defcfun (%iup-get-class-attributes "IupGetClassAttributes") :int
  (classname :string)
  (names :pointer)
  (n :int))

(defcfun (%iup-get-class-callbacks "IupGetClassCallbacks") :int
  (classname :string)
  (names :pointer)
  (n :int))

(defcfun (%iup-save-class-attributes "IupSaveClassAttributes") :void
  (handle ihandle))


(defcfun (%iup-copy-class-attributes "IupCopyClassAttributes") :void
  (source-handle ihandle)
  (destination-handle ihandle))

(defcfun (%iup-set-class-default-attribute "IupSetClassDefaultAttribute") :void
  (classname :string)
  (name attr-name)
  (value :string))

;; int       IupClassMatch(Ihandle* ih, const char* classname);

;; Ihandle*  IupCreate (const char *classname);
;; Ihandle*  IupCreatev(const char *classname, void* *params);
;; Ihandle*  IupCreatep(const char *classname, void* first, ...);

(defcfun (%iup-fill "IupFill") ihandle)
(defcfun (%iup-space "IupSpace") ihandle)

(defcfun (%iup-radio "IupRadio") ihandle
  (child ihandle))

(defcfun (%iup-vbox-v "IupVboxv") ihandle
  (children :pointer))

(defcfun (%iup-zbox-v "IupZboxv") ihandle
  (children :pointer))

(defcfun (%iup-hbox-v "IupHboxv") ihandle
  (children :pointer))

(defcfun (%iup-normalizer-v "IupNormalizerv") ihandle
  (children :pointer))

(defcfun (%iup-cbox-v "IupCboxv") ihandle
  (children :pointer))

(defcfun (%iup-sbox "IupSbox") ihandle
  (child ihandle))

(defcfun (%iup-split "IupSplit") ihandle
  (child1 ihandle)
  (child2 ihandle))

(defcfun (%iup-scroll-box "IupScrollBox") ihandle
  (child ihandle))

(defcfun (%iup-flat-scroll-box "IupFlatScrollBox") ihandle
  (child ihandle))

(defcfun (%iup-gridbox-v "IupGridBoxv") ihandle
  (children :pointer))

(defcfun (%iup-expander "IupExpander") ihandle
  (child ihandle))

(defcfun (%iup-detach-box "IupDetachBox") ihandle
  (child ihandle))

(defcfun (%iup-background-box "IupBackgroundBox") ihandle
  (child ihandle))

(defcfun (%iup-frame "IupFrame") ihandle
  (child ihandle))

(defcfun (%iup-flat-frame "IupFlatFrame") ihandle
  (child ihandle))

(defcfun (%iup-image "IupImage") ihandle
  (width :int)
  (height :int)
  (pixmap :pointer))

(defcfun (%iup-image-rgb "IupImageRGB") ihandle
  (width :int)
  (height :int)
  (pixmap :pointer))

(defcfun (%iup-image-rgba "IupImageRGBA") ihandle
  (width :int)
  (height :int)
  (pixmap :pointer))

(defcfun (%iup-item "IupItem") ihandle
  (title :string)
  (action :string))

(defcfun (%iup-separator "IupSeparator") ihandle)

(defcfun (%iup-submenu "IupSubmenu") ihandle
  (title :string)
  (menu ihandle))

(defcfun (%iup-menu-v "IupMenuv") ihandle
  (children :pointer))

(defcfun (%iup-button "IupButton") ihandle
  (title :string)
  (action :string))

(defcfun (%iup-flat-button "IupFlatButton") ihandle
  (title :string))

(defcfun (%iup-flat-toggle "IupFlatToggle") ihandle
  (title :string))

(defcfun (%iup-drop-button "IupDropButton") ihandle
  (drop-child ihandle))

(defcfun (%iup-flat-label "IupFlatLabel") ihandle
  (title :string))

(defcfun (%iup-flat-separator "IupFlatSeparator") ihandle)

(defcfun (%iup-canvas "IupCanvas") ihandle
  (action :string))

(defcfun (%iup-dialog "IupDialog") ihandle
  (child ihandle))

(defcfun (%iup-user "IupUser") ihandle)

(defcfun (%iup-label "IupLabel") ihandle
  (title :string))

(defcfun (%iup-list "IupList") ihandle
  (action :string))

(defcfun (%iup-text "IupText") ihandle
  (action :string))

(defcfun (%iup-multi-line "IupMultiLine") ihandle
  (action :string))

(defcfun (%iup-toggle "IupToggle") ihandle
  (title :string)
  (action :string))

(defcfun (%iup-timer "IupTimer") ihandle)
(defcfun (%iup-clipboard "IupClipboard") ihandle)
(defcfun (%iup-progress-bar "IupProgressBar") ihandle)

(defcfun (%iup-val "IupVal") ihandle
  (type :string))

(defcfun (%iup-tabs-v "IupTabsv") ihandle
  (children :pointer))

(defcfun (%iup-flat-tabs-v "IupFlatTabsv") ihandle
  (children :pointer))

(defcfun (%iup-tree "IupTree") ihandle)

(defcfun (%iup-link "IupLink") ihandle
  (url :string)
  (title :string))

(defcfun (%iup-animated-label "IupAnimatedLabel") ihandle
  (animation ihandle))

(defcfun (%iup-date-pick "IupDatePick") ihandle)
(defcfun (%iup-calendar "IupCalendar") ihandle)
(defcfun (%iup-colorbar "IupColorbar") ihandle)
(defcfun (%iup-gauge "IupGauge") ihandle)

(defcfun (%iup-dial "IupDial") ihandle
  (orientation :string))

(defcfun (%iup-color-browser "IupColorBrowser") ihandle)

;; /* String compare utility */
;; int IupStringCompare(const char* str1, const char* str2, int casesensitive, int lexicographic);

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

(defcfun (%iup-file-dlg "IupFileDlg") ihandle)
(defcfun (%iup-message-dlg "IupMessageDlg") ihandle)
(defcfun (%iup-color-dlg "IupColorDlg") ihandle)
(defcfun (%iup-font-dlg "IupFontDlg") ihandle)
(defcfun (%iup-progress-dlg "IupProgressDlg") ihandle)

(defcfun (%iup-message "IupMessage") :void
  (title :string)
  (message :string))

(defcfun (%iup-message-error "IupMessageError") :void
  (parent ihandle)
  (message :string))

(defcfun (%iup-message-alarm "IupMessageAlarm") :int
  (parent ihandle)
  (title :string)
  (message :string)
  (buttons :string))

(defcfun (%iup-alarm "IupAlarm") :int
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

;; Ihandle* IupLayoutDialog(Ihandle* dialog);
;; Ihandle* IupElementPropertiesDialog(Ihandle* elem);

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
;; /*                      Pre-Defined Masks                               */
;; /************************************************************************/
;; #define IUP_MASK_FLOAT       "[+/-]?(/d+/.?/d*|/./d+)"
;; #define IUP_MASK_UFLOAT            "(/d+/.?/d*|/./d+)"
;; #define IUP_MASK_EFLOAT      "[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?"
;; #define IUP_MASK_UEFLOAT           "(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?"
;; #define IUP_MASK_FLOATCOMMA  "[+/-]?(/d+/,?/d*|/,/d+)"
;; #define IUP_MASK_UFLOATCOMMA       "(/d+/,?/d*|/,/d+)"
;; #define IUP_MASK_INT          "[+/-]?/d+"
;; #define IUP_MASK_UINT               "/d+"


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

;;; iup_config.h

(defcfun (%iup-config "IupConfig") ihandle)

(defcfun (%iup-config-load "IupConfigLoad") :int
  (handle ihandle))

(defcfun (%iup-config-save "IupConfigSave") :int
  (handle ihandle))

;; /****************************************************************/

;; void IupConfigSetVariableStr(Ihandle* ih, const char* group, const char* key, const char* value);
;; void IupConfigSetVariableStrId(Ihandle* ih, const char* group, const char* key, int id, const char* value);
;; void IupConfigSetVariableInt(Ihandle* ih, const char* group, const char* key, int value);
;; void IupConfigSetVariableIntId(Ihandle* ih, const char* group, const char* key, int id, int value);
;; void IupConfigSetVariableDouble(Ihandle* ih, const char* group, const char* key, double value);
;; void IupConfigSetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id, double value);

;; const char* IupConfigGetVariableStr(Ihandle* ih, const char* group, const char* key);
;; const char* IupConfigGetVariableStrId(Ihandle* ih, const char* group, const char* key, int id);
;; int    IupConfigGetVariableInt(Ihandle* ih, const char* group, const char* key);
;; int    IupConfigGetVariableIntId(Ihandle* ih, const char* group, const char* key, int id);
;; double IupConfigGetVariableDouble(Ihandle* ih, const char* group, const char* key);
;; double IupConfigGetVariableDoubleId(Ihandle* ih, const char* group, const char* key, int id);

;; const char* IupConfigGetVariableStrDef(Ihandle* ih, const char* group, const char* key, const char* def);
;; const char* IupConfigGetVariableStrIdDef(Ihandle* ih, const char* group, const char* key, int id, const char* def);
;; int    IupConfigGetVariableIntDef(Ihandle* ih, const char* group, const char* key, int def);
;; int    IupConfigGetVariableIntIdDef(Ihandle* ih, const char* group, const char* key, int id, int def);
;; double IupConfigGetVariableDoubleDef(Ihandle* ih, const char* group, const char* key, double def);
;; double IupConfigGetVariableDoubleIdDef(Ihandle* ih, const char* group, const char* key, int id, double def);

;; void IupConfigCopy(Ihandle* ih1, Ihandle* ih2, const char* exclude_prefix);

;; /****************************************************************/

;; void IupConfigSetListVariable(Ihandle* ih, const char *group, const char* key, const char* value, int add);

;; void IupConfigRecentInit(Ihandle* ih, Ihandle* menu, Icallback recent_cb, int max_recent);
;; void IupConfigRecentUpdate(Ihandle* ih, const char* filename);

(defcfun (%iup-config-dialog-show "IupConfigDialogShow") :void
  (handle ihandle)
  (dialog-handle ihandle)
  (name :string))

(defcfun (%iup-config-dialog-closed "IupConfigDialogClosed") :void
  (handle ihandle)
  (dialog-handle ihandle)
  (name :string))
