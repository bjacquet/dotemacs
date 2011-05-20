
;;; Handle Allegro 8.0 changes

(defvar *_dirs* nil)
(setf *_dirs* (list "Z:/siscog/"))

(grepfc '(
	  "(custom.feature :access.control)"
	  ) '("lisp" "cl" "bil") *_dirs* nil nil)





 



(grepfc '("device-open") '("lisp") '("Y:/crews/crews-v6-8-0/patches/crews") nil nil)

;; Procurar coisas nos graficos
(grepfc '("defun select.date") '("cl" "lisp" "bil") (get-crews-dirs t t t nil t)  nil nil)


(setq *_dirs* (get-crews-dirs t t t nil t))

(setq *_dirs* (list "z:/siscog-8-0/crews-vdev/crews/win-maps"
		    "z:/siscog-8-0/crews-vdev/siscog-util/wingraphics"
		    ))

(progn
  (beginning-of-buffer)
  (replace-string "E:/meter-reports-total/" "")
;;;  (beginning-of-buffer)
;;;  (replace-string ".txt:   1 LOAD.RSR" "")
;;;  (beginning-of-buffer)
;;;  (replace-string ".txt:     2 (D) SG-SELECT-COLUMNS" "")
;;;  (beginning-of-buffer)
;;;  (replace-string ".txt: 0 SCHEDULER.ST.DATA.LOADER.AFTER" "")
;;;  (beginning-of-buffer)
;;;  (replace-string ".txt: 0 SAVE.SCHEDULER.SHORT.TERM" "")
  (beginning-of-buffer)
  (replace-string "-" "	")
  (beginning-of-buffer))


(grepfc '("double-float") '("cl" "bil" "lisp") *_dirs*  nil nil)

(grepfc '("closed-stream-p") '("cl" "bil" "lisp") *_dirs*  nil nil)

(grepfc '("make-box") '("bil") *_dirs*  nil nil)


(grepfc '(":parent") '("cl" "bil") *_dirs*  nil nil)

(query-replace-modif ":overlapped t" ":child-p nil" '("bil" "cl") *_dirs*)

(grepfc '("child-p nil" "overlapped t") '("cl" "bil") *_dirs*  nil nil)	;;; ???

(grepfc '("pop-up nil") '("cl" "bil") *_dirs*  nil nil)

(grepfc '("allegro") '("cl" "bil") *_dirs*  nil nil)

(grepfc '("lisp-group-box") '("cl" "bil") *_dirs*  nil nil)

(grepfc '("open-stream") '("cl") *_dirs*  nil nil)

;; Call query-replace of wg::

(grepfc '("wg:" "wingraphics:") '("cl" "bil") *_dirs*  nil nil)

(grepfc '("::") '("cl" "bil") *_dirs*  nil nil)


;; END

(grepfc '("overlapped t") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil) ;; <- NEED QUERY-REPLACE

(grepfc '("lisp-group-box") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '("make-box") '("bil") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '(":device") '("cl") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '(":parent") '("cl") (get-crews-dirs t t t nil t)  nil nil)


(grepfc '("freq.real.number.of.days" "freq.expected.number.of.days") '("lisp" "cl") '("z:/find-siscog")  nil nil)

(grepfc '("make.background") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '("wg::") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '("wg:") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '("wingraphics:") '("cl" "bil") (get-crews-dirs t t t nil t)  nil nil)




(grepfc '("make.task.display") '("lisp") (get-crews-dirs t t t nil nil)  nil nil)

(grepfc '("expose") '("cl") (get-crews-dirs t t t nil t)  nil nil)

(defun get-crews-dirs (crews-p all crews-x data gui)
  (let ((all-crews-x '("siscog" "vr"  "dsb"  "nsb" "siscog" "stog")) ;;"brisa" "ns"
	(dirs nil))
    (if crews-p
	(if gui
	    (progn
	      (setf dirs (cons "z:/siscog/crews-vdev/crews/win-maps" dirs))
	      (setf dirs (cons "z:/siscog/crews-vdev/siscog-util/wingraphics" dirs)))
	    (setf dirs (cons "z:/siscog/crews-vdev" dirs))))
    (if all
	(setf crews-x all-crews-x))
    (dolist (x crews-x)
      (if gui
	  (setf dirs (cons (format "z:/siscog/crews-%s-vdev/crews-%s/win-maps" x x) dirs))
	  (setf dirs (cons (format "z:/siscog/crews-%s-vdev/crews-%s" x x) dirs)))
      (when data
	(setf dirs (cons (format "z:/siscog/crews-%s-vdev/%s-data/%s" x x x) dirs))))
    (reverse dirs)))



;;;(grepfc '("get.train.sub.type.items") '("lisp" "cl") (get-crews-dirs t t nil) nil "z:/siscog/grepfc.txt")
;;;(grepfc '("with.selected.ddays" "push.frequency.plist") '("lisp" "cl") '("z:/siscog/crews-ns-vdev/crews-ns") nil "z:/siscog/grepfc.txt")

;;;(grepfc '("with.selected.ddays") '("lisp" "cl") '("z:/siscog/crews-vdev/crews/win-maps") nil "z:/siscog/grepfc.txt")
;;;(grepfc '("'print.task.types") '("cl") (get-crews-dirs t t nil nil t) nil nil)


;;;(grepfc '("current.state.space") '("cl") (get-crews-dirs t t nil nil t) nil nil)
;;;(grepfc '("check.permission.for.action.access") '("cl") '("z:/siscog/crews-vdev/crews/win-maps") nil nil)

;;;(grepfc '("add.keyword") '("lisp") '("z:/siscog/crews-vr-vdev/patches") nil nil)

;;;(while (= (char-after (point)) 122)
;;;  (while (not (= (char-after (point)) 34))
;;;    (delete-char 1))
;;;  (let ((last (char-after (point))))
;;;    (forward-char 1)
;;;    (while (or (not (= (char-after (point)) 34))
;;;	       (= last 92))
;;;      (setf last (char-after (point)))
;;;      (forward-char 1)))
;;;  (forward-char 1)
;;;  (while (not (= (char-after (point)) 10))
;;;    (delete-char 1))
;;;  (forward-line 1)
;;;  (beginning-of-line))


;;;(defun joao (lista)
;;;  (flet ((find.str (str)
;;;	   (maphash #'(lambda (k v) (when (string= str v) (return-from find.str t))) traducao::*all.keywords*) nil))
;;;  (setf lista (sort (delete-duplicates lista :test #'string=) #'string<))
;;;  (dolist (str lista)
;;;    (unless (find.str str)
;;;      (format t "~c~s~%~%" #\~ str)))))

;;;(defun paulo (lista)
;;;  (flet ((find.str (str)
;;;	   (getf (translations (keyword.make str)) :norwegian)))
;;;  (setf lista (sort (delete-duplicates lista :test #'string=) #'string<))
;;;  (dolist (str lista)
;;;    (unless (find.str str)
;;;      (format t "~c~s~%~%" #\~ str)))))

;; TO EXTRACT THE SYMBOLS THAT NEED QUERY-REPLACE TO REMOVE WG:: AFTER MAKING THE GREPFC
;;;(progn
;;;  (beginning-of-buffer)
;;;  (beginning-of-line)
;;;  (let ((start (point)))
;;;    (while (search-forward "cg:" nil t)
;;;      (backward-char 3)
;;;      (delete-region start (point))
;;;      (next-space)
;;;      (unless (= 10 (char-after (point)))
;;;	(kill-line))
;;;      (next-line 1)
;;;      (beginning-of-line)
;;;      (setf start (point)))))

(grepfc '("cg:find-component") '("cl" "bil") *_dirs* nil nil)

;; QUERY-REPLACE-MODIF

(query-replace-modif "(custom.feature :composed.duties)" "(has.composed.duties.p)" '("lisp" "cl" "bil") *_dirs*)
(query-replace-modif "(custom.feature :short.term.gui)" "(short.term.application.p)" '("lisp" "cl" "bil") *_dirs*)


(setf *symbols* (list "ACTIVE.CELL.CONTIGUOUS" "ACTIVE.CELL.RANDOM" "ACTIVE.P" "ADD.CANVAS" "ADD.DASH" "ADD.ITEM.TO.H.SIDER" "ADD.ITEM.TO.V.SIDER"
		      "ADD.N.COLUMNS" "ADD.N.ROWS" "ADD.NEAR.DASH" "ALIGN-SIZE" "ALLOWED.TYPES" "ANCESTOR" "APPEND.OBJECT"
		      "ASK.USER.FOR.DIRECTORY" "ASK.USER.FOR.EXISTING.PATHNAME" "ASK.USER.FOR.NEW.PATHNAME" "BACKGROUND.PANE" "BALANCE.PANES" 
		      "BASIC.CANVAS" "BASIC.PAGE"
		      "BASIC.PRINTER" "BASIC.WORLD" "BITMAP" "BITMAP.HEIGHT" "BITMAP.PIXEL.MAP" "BITMAP.WIDTH" "BITMAPS"
		      "BLINK.HOTSPOT" "BLINKED?" "BOLD.TEXT.FONT" "BOTTOM" "BOX.BOTTOM" "BOX.BOTTOM.LEFT" "BOX.BOTTOM.RIGHT"
		      "BOX.CENTER" "BOX.CENTER.ON.BOX" "BOX.HEIGHT" "BOX.INTERSECT" "BOX.INTERSECT.P" "BOX.LEFT" "BOX.MOVE"
		      "BOX.MOVE.TO" "BOX.MOVE.TO.ZERO" "BOX.NORMALIZE" "BOX.RIGHT" "BOX.TOP" "BOX.TOP.LEFT" "BOX.TOP.RIGHT"
		      "BOX.UNION" "BOX.WIDTH" "BOX=" "BOXP" "BREAK.STRING.BY.WIDTH" "BROWSE.BUTTON" "BUILD.CELL"
		      "BUILD.TABLE" "CANVAS" "CANVAS.LIST" "CANVAS.WITH.HOTSPOTS" "CATCHING.ERRORS" "CELL" "CELL.MOVE.TO"
		      "CENTER.ON" "CHANGE.CONFIGURATION" "CHANGE.MENU.ITEM.ARGS" "CHOOSE.NAME" "CHOOSE.POP.UP" "CLEAR" "CLEAR.CONTENTS"
		      "CLEAR.CURRENT.HOTSPOT" "CLEAR.RECTANGLE" "COLLECT.GCONTAINER.OBJECTS" "COLLECT.WINDOWS" "COLOR-RGB" 
		      "COMPILE.MESSAGE.STRINGS" "COMPLEX.CELL"
		      "COMPLEX.WORLD.OBJECT" "CONFIGURATION" "CONTAINER.FRONTIER" "CONTENTS" "COPY.BOX" "COPY.CELL" "COPY.CELL.ATTRIBUTES"
		      "COPY.HOTSPOT" "COPY.POSITION" "COPY.ROW" "COPY.SIDER" "COPY.TABLE" "CREATE.COLOR.GCONTEXT" "CREATE.FONT.GCONTEXT"
		      "CREATE.OBJECTS" "CREATE.WORLD" "CURRENT.VIEW.MODE" "CURRENT.WINDOW" "CUT" "CUT.TABLE.HORIZONTALLY" "DATA"
		      "DATE.OF.PRINT" "DATE.RANGE" "DATE.WIDGET" "DEACTIVATE" "DEACTIVATE.ON.CHANGE" "DEF.COLOR" "DEF.GCONTEXT"
		      "DEFAULT.DIALOG.FIXED.FONT" "DEFAULT.DIALOG.VARIABLE.FONT" "DEFAULT.GCONTEXT" "DEFAULT.PANE.CLASS" "DEFINE.FRONTIER" 
		      "DELTA" "DEPENDENT.WIDGET"
		      "DESELECT.HOTSPOT" "DESELECT.MENU.ITEMS" "DIMENSION" "DISABLE.HOTSPOT" "DISABLE.MENU.ITEMS" "DISABLED?" "DISPLAY"
		      "DISTRIBUTE.STRING.BY.BOXES" "DO.GCONTAINER.OBJECTS" "DO.WINDOWS" "DONT.SORT" "DRAW.CHAR.IN.WORLD" "DRAW.CONTENTS" "DRAW.DOT"
		      "DRAW.DOT.IN.WORLD" "DRAW.LINE" "DRAW.LINE.IN.WORLD" "DRAW.OBJECT" "DRAW.POLY.LINE" "DRAW.POLY.LINE.IN.WORLD" "DRAW.RECTANGLE"
		      "DRAW.RECTANGLE.IN.WORLD" "DRAW.SEGMENTS" "DRAW.STRING" "DRAW.STRING.IN.BOX.TRIMMING.BY.CHARS" "DRAW.STRING.IN.WORLD" "DRAWER" 
		      "DRAWING.LIMITS" "DROP.BUTTON" "DROP.MENU.BUTTON" "EDITABLE.NUMBER.WITH.RANGE" "EDITABLE.REAL.NUMBER.WITH.RANGE" "EDITABLE.TEXT" 
		      "ELEVATOR.COLORS" "ELLIPSIS"
		      "EMPTY.VALUE.P" "ENABLE.HOTSPOT" "ENABLE.MENU.ITEMS" "ENABLE?" "EVENTS.TO.INFERIORS.MIXIN" "EXECUTE.DOUBLE.CLICK.HOTSPOT" 
		      "EXECUTE.HOTSPOT"
		      "EXPOSE" "EXPOSED.P" "FILL.RECTANGLE" "FILL.RECTANGLE.IN.WORLD" "FIND.BACKGROUND" "FIND.MENU.ITEM" "FIND.WINDOW"
		      "FIXED.SIZE&POSITION.HOTSPOT" "FIXED.SIZE.HOTSPOT" "FIXED.TEXT.FONT" "FORCE.CREATE.OBJECTS" "FRONTIER" "FULL.HOTSPOT" 
		      "GC.FONT.CHAR.HEIGHT"
		      "GC.LINE.WIDTH" "GC.STRING.WIDTH" "GCONTEXT" "GET.ABREVIATE.MENU.ITEMS" "GET.BACKGROUND" "GET.BITMAP" "GET.BLINK.GCONTEXT"
		      "GET.BROTHER" "GET.CELL" "GET.CELL.VALUE" "GET.COLOR" "GET.COLOR.GCONTEXT" "GET.COLOR.RGB" "GET.COMPONENT.VALUE"
		      "GET.CONSTRAINTS" "GET.CURRENT.LAYOUT" "GET.FONT" "GET.FONT.GCONTEXT" "GET.GCONTEXT" "GET.HOTSPOT" "GET.LIGHT.GCONTEXT"
		      "GET.MASTER" "GET.MENU.FROM.ITEMS" "GET.PAGE" "GET.SCHEME.VALUE" "GET.SCREEN.SCROLL.MARGINS" "GET.SELECT.GCONTEXT" "GET.SIDER.CELL"
		      "GET.SUBMENU.FROM.ITEMS" "GET.SUBWINDOW.CLASS&ARGS.FROM.KEY" "GET.VALUES.FROM.PATHNAME" "GET.WINDOW" "GET.WINDOW.TITLE" 
		      "GET.WINDOWS.OF.SAME.CLASS" "GET.WORLD"
		      "GET.X.ZOOM" "GET.Y.ZOOM" "GRAPH.WINDOW" "GRAPH.WINDOW.PANE" "GRAPH.WORLD" "GROUP.MENU.ITEMS" "HEIGHT"
		      "HIDE.HOTSPOT" "HIGHLIGHT" "HIGHLIGHTED?" "HOLLOW.HOTSPOT" "HORIZONTAL.SCROLL.BAR" "HORIZONTAL.SIDER" "HOST"
		      "HOTSPOT" "HOTSPOT.ALIGNMENT" "HOTSPOT.BLINK" "HOTSPOT.DISPLAY" "HOTSPOT.DOCUMENTATION" "HOTSPOT.ENABLE" "HOTSPOT.LIGHT"
		      "HOTSPOT.MENU" "HOTSPOT.ORIENTATION" "HOTSPOT.PLIST" "HOTSPOT.SELECT" "HOTSPOT.VISIBLE" "IDENTITY.PARSER" "IF.IN.VIEW?"
		      "IN.FRONTIER?" "IN.HOTSPOT?" "INFERIORS" "INITIAL.NAME" "INITIALIZE.WINDOWS" "INITIALIZE.WINDOWS" "INSERT.MENU.ITEM"
		      "INSERT.OBJECT" "INSERT.QUOTES.IF.NEED" "INSIDE.BOX.P" "INSTALL.MENU.ITEM.SHORTCUTS" "INVALID.FRONTIER" "IS.BROTHER?" "ITEMS.RANGE"
		      "KILL" "LAYOUT" "LEAF.SUBDIVISIONS" "LEFT" "LINE.SEPARATOR.COLOR" "LINE.WITH.HOTSPOTS" "LISP.TO.BITMAP"
		      "LOAD.SISCOG.BITMAP" "MAIN.PANE" "MAIN.WINDOWS" "MAKE.BOX" "MAKE.BOX.FROM.CORNERS" "MAKE.BOX.RELATIVE" 
		      "MAKE.BOX.RELATIVE.FROM.CORNER" "MAKE.COLOR.GCONTEXT.NAME" "MAKE.FONT.GCONTEXT.NAME" "MAKE.HORIZONTAL.HEADER" "MAKE.MENU.ITEM" 
		      "MAKE.MENU.OPTION" "MAKE.MENU.SEPARATOR" "MAKE.OBJECT"
		      "MAKE.PAGES.SCHEME" "MAKE.POSITION" "MAKE.TABLE" "MAKE.VERTICAL.HEADER" "MAPS.WINDOW.SET" "MENU.DISPLAY" "MENU.TYPE"
		      "MESSAGES" "MODAL?" "MOUSE.BUTTON" "MOUSE.DOCUMENTATION" "MOUSE.LEFT.PRESS" "MOUSE.LEFT.RELEASE" "MOUSE.MIDDLE.PRESS"
		      "MOUSE.MIDDLE.RELEASE" "MOUSE.POSITION" "MOUSE.RIGHT.PRESS" "MOUSE.RIGHT.RELEASE" "MOVE.OBJECT" "NALIGN.SIZE" "NBOX.BOTTOM.LEFT"
		      "NBOX.BOTTOM.RIGHT" "NBOX.CENTER" "NBOX.MOVE" "NBOX.MOVE.TO" "NBOX.MOVE.TO.ZERO" "NBOX.NORMALIZE" "NBOX.TOP.LEFT"
		      "NBOX.TOP.RIGHT" "NBOX.UNION" "NCOPY.BOX" "NCOPY.POSITION" "NMAKE.BOX" "NMAKE.BOX.FROM.CORNERS" "NMAKE.BOX.RELATIVE"
		      "NMAKE.BOX.RELATIVE.FROM.CORNER" "NMAKE.POSITION" "NMOUSE.POSITION" "NOBJECT.SIZE" "NORMALIZE.DIR.STRING" "NPOSITION*" "NPOSITION+"
		      "NPOSITION-" "NPOSITION.ROTATE" "NPOSITION.TRANSFORM" "NSTRING.SIZE" "NSTRING.SIZE.IN" "NUMBER.TO.STRING" "NVIEWPORT.BOX.TO.WORLD"
		      "NVIEWPORT.POSITION.TO.WORLD" "NWORLD.BOX.TO.VIEWPORT" "NWORLD.DRAW.LIMITS.TO.WINDOW" "NWORLD.FRONTIER.TO.WINDOW" 
		      "NWORLD.POSITION.TO.VIEWPORT" "OBJECT" "OBJECT.FOR.PRINTING"
		      "OBJECT.FRONTIER" "OBJECT.SIZE" "OBJECTS" "ON.PRINT" "OUTPUT.DEVICE" "OUTPUT.PAGES" "PAGE.HEADER"
		      "PAGE.NUMBER" "PAGE.TABLE.DIMENSIONS" "PAGE.WORKING.AREA" "PAGES.SCHEME" "PARENT.OBJECT" "PATHNAME.TYPE" "PATHNAME.WIDGET"
		      "PIC.BUTTON" "PLIST" "POINTS" "POLY.LINE.HOTSPOT" "POP.UP.MENU" "POP.UP.MESSAGE.DIALOG" "POP.UP.MODAL.DIALOG"
		      "POSITION*" "POSITION+" "POSITION-" "POSITION.LENGTH" "POSITION.ROTATE" "POSITION.TRANSFORM" "POSITION.X"
		      "POSITION.Y" "POSITION=" "POSSIBLE.STRING.IN.WIDTH" "PRESS.HOTSPOT" "PREVIOUS.DATA" "PRINC.LIST.TO.STRING" "PRINT.CANVAS"
		      "PRINT.CENTRED" "PRINT.DUTY.ID.IN.BALLOON" "PRINT.END.STATION.IN.BALLOON" "PRINT.NAME.IN.BALLOON" "PRINT.STAIR.STRING" 
		      "PRINT.TABLE.ON.CELL" "PRINT.TIME.IN.CELL"
		      "PRINT.VALUE.STRING" "PROMPT" "PULL.DOWN.MENU" "READY?" "REBUILD.OBJECT" "REBUILD.RULER" "REDEFINE.CONSTRAINTS"
		      "REDISPLAY" "REDISPLAY.OBJECT" "REFERENCED.OBJECT" "REFRESH" "RELATIVE.ZOOM" "REMOVE.ALL.DASHES" "REMOVE.ALL.DASHES.TO.MASTER"
		      "REMOVE.ALL.DASHES.TO.SLAVE" "REPLICATE.BITMAP" "REPLICATE.BITMAP.WITH.ZOOM" "RESET.HOTSPOT" "RESIZABLE.DIALOG" "RESTING?" 
		      "RETURN.CANCEL.ON.CHANGE"
		      "RETURN.OK.ON.CHANGE" "RIGHT" "ROOT.WINDOW" "ROW.HEIGHT" "ROW.POSITION" "RULE.PANES" "RULER&MARK.WINDOW"
		      "RULER.BACK.COLOR" "RULER.BORDER" "RULER.DASH.SIDE" "RULER.FONT" "RULER.FORE.COLOR" "RULER.MENU.ITEMS" "RULER.WINDOW"
		      "RULER.WITH.WORLD" "SAVE.TO.FILE.HOST.FOLDER" "SCROLL.BACK.COLOR" "SCROLL.HORIZONTAL" "SCROLL.LIMITS" "SCROLL.VERTICAL" 
		      "SCROLL.WINDOW.HEIGHT"
		      "SCROLL.WINDOW.WIDTH" "SELECT" "SELECT.HOTSPOT" "SELECT.OBJECTS.BY.PREDICATE" "SELECTED.WINDOW" "SELECTED?" "SELECTION.MASTER.MIXIN"
		      "SENSITIVE.LIMITS" "SENSITIVE.NODE" "SET-POSITION-Y" "SET.BOX.BOTTOM" "SET.BOX.LEFT" "SET.BOX.RIGHT" "SET.BOX.TOP"
		      "SET.BROTHER" "SET.CELL" "SET.CELL.VALUE" "SET.CORNER.CELL" "SET.PAGE" "SET.PAGE.HEADER.HEIGHT" "SET.PAGES.SCHEME"
		      "SET.POSITION.X" "SET.SCROLL.LIMITS" "SET.SIDER" "SET.SIDER.CELL" "SET.VIEWPORT" "SET.VISIBILITY.STATE" "SET.WIDGET.VALUE"
		      "SET.WINDOW.INSIDE.SIZE" "SET.WINDOW.POSITION" "SET.WINDOW.TITLE" "SET.WORLD" "SET.WORLD.FRONTIER" "SET.WORLD.WINDOW" 
		      "SET.X.SCROLL.TO"
		      "SET.X.ZOOM.IN.HOURS" "SET.ZOOM.TO" "SHOW.HOTSPOT.POPUP.MENU" "SIDER.MOVE.TO" "SIMPLE.TEXT.PAGE" "SIMPLE.WORLD.OBJECT" "SLICE.TABLE"
		      "SLICE.TABLE.VERTICALLY" "SOURCE.CELLS" "SPLIT.TABLE.HORIZONTALLY" "SPLITER.PANE" "STAIR.STRING.HEIGHT" "STAIR.STRING.N.LINES" 
		      "STRETCHABLE.BITMAP"
		      "STRING.BOX.HEIGHT" "STRING.SIZE" "SUB.BOX.P" "SUPERIOR" "TABLE" "TABLE.DIMENSIONS" "TABLE.MOVE.TO"
		      "TABLE.NR.OF.COLUMNS" "TABLE.NR.OF.ROWS" "TABLE.POSITION" "TEXT.FONT" "THE.SCREEN" "TIME.OF.PRINT" "TIME.RANGE"
		      "TIME.WIDGET" "TOP" "TOP.ANCESTOR" "TOTAL.N.SUBDIVISIONS" "TROCA.OUTPUT" "TURN.OFF.HOTSPOT" "TURN.ON.HOTSPOT"
		      "UNBLINK.HOTSPOT" "UNHIDE.HOTSPOT" "UNHIGHLIGHT" "UNPRESS.HOTSPOT" "UNSET.VISIBILITY.STATE" "UPDATE.MENU" "UPDATE.MENU.ITEM"
		      "UPDATE.RULER" "UPDATE.RULERS" "UPDATE.SCROLL.BARS" "UPDATE.SUBMENU" "VALID.CONFIGURATION" "VALID.DATE?" "VALID.INTEGER?"
		      "VALID.POSITIVE.INTERGER?" "VALID.POSITIVE.REAL?" "VALID.REAL?" "VALID.TIME?" "VALUE.AREA" "VALUE.PRINTING.FUNC" "VARIABLE.HOTSPOT"
		      "VERTICAL.SCROLL.BAR" "VERTICAL.SIDER" "VIEWPORT" "VIEWPORT.POSITION.TO.WORLD" "VISIBILITY" "VISIBLE?" "WG.SET.CURRENT.LAYOUT"
		      "WHILE.INHIBITING" "WIDGET.OLD.VALUE" "WIDGET.PARSER" "WIDGET.TO.SCREEN.UNITS" "WIDGET.TYPE" "WIDGET.VALUE" "WIDTH"
		      "WINDOW" "WINDOW.CELL" "WINDOW.HEIGHT" "WINDOW.INSIDE.HEIGH" "WINDOW.INSIDE.WIDTH" "WINDOW.WIDTH" "WINDOW.WITH.RULER"
		      "WINDOW.WITH.RULER&MARK" "WINDOW.WITH.SCROLLBAR" "WINDOWS.LIST" "WITH.CLIPPING.RECTANGLE" "WITH.COMPONENT.VALUES" "WITH.COMPONENTS" 
		      "WITH.GCONTEXT" "WITH.MENU.ITEMS" "WITH.OBJECT.GCONTEXT" "WITH.OPEN.PRINTER.PROTECTED" "WITH.SAND.GLASS" "WORK.FOREGROUND.COLOR" 
		      "WORLD.BOTTOM" "WORLD.BOX.TO.VIEWPORT" "WORLD.FRONTIER.TO.WINDOW" "WORLD.HEIGHT" "WORLD.ITERATOR" "WORLD.LEFT" "WORLD.OBJECT" 
		      "WORLD.OBJECT.CONTAINER" "WORLD.POSITION.TO.VIEWPORT"
		      "WORLD.RIGHT" "WORLD.TOP" "WORLD.WIDTH" "WORLD.WINDOW" "WORLD.WITH.HOTSPOTS" "X.ZOOM" "Y.ZOOM"))

(dolist (string *symbols*)
  (query-replace-modif (format "wg::%s" (downcase string)) (downcase string) '("cl" "bil") *_dirs* "Remove the reference to package WINGRAPHICS")
  (query-replace-modif (format "wg:%s" (downcase string)) (downcase string) '("cl" "bil") *_dirs* "Remove the reference to package WINGRAPHICS"))


;;; FUTURO

wg::*default.window.foreground.color*
wg::*delta.height*
wg::*delta.x*
wg::*delta.y*
wg::*spliter-size*
wg::bitblt
wg::change.constraint
wg::color-rgb
wg::constraint.name
wg::constraint.space
wg::constraints
wg::current.documentation.hotspot
wg::dash.number.to.point
wg::dashes
wg::deeper.object?
wg::display.tooltip
wg::draw.all.dashes
wg::font.face
wg::get.gcontext.font
wg::font.size
wg::get.system.font
wg::graphics.dialog
wg::graphics.message
wg::hide.tooltip
wg::hotspot.tooltip
wg::menu.item.display
wg::mouse.double.click.in.world
wg::mouse.left.press
wg::mouse.left.release.in.world
wg::nhotspot.object.size
wg::object.info
wg::object.info.size
wg::p.list
wg::page.box
wg::pathname.defaults
wg::print.station.in.balloon
wg::radiation
wg::remove.object
wg::remove.object
wg::report.msg.dialog
wg::reset.duty.id.balloons
wg::reset.end.station.balloons
wg::reset.windows.constraints
wg::rest.constraints
wg::select.objects.by.object
wg::string.size.in
wg::valid.real.range?
wg::valid.time.range.message
wg::window.constraint.set
wg::window.inside.position.and.size
wg::window.inside.size
wg::windows

-------------------------------------------------------------------------------
(grepfc '("wg:") '("lisp" "cl" "bil") '("Z:/siscog/crews-vdev/crews/win-maps") nil)
-------------------------------------------------------------------------------
(grepfc "wg:")
Z:/siscog/crews-vdev/crews/win-maps/background-menu.cl: 			 (and (typep win 'wg::window.constraint.set)
Z:/siscog/crews-vdev/crews/win-maps/background.cl:       (wg::graphics.dialog message :icon icon :title title :yes-text button :no-text nil :cancel-text nil :cancel-all-text nil :beep t))
Z:/siscog/crews-vdev/crews/win-maps/background.cl:       (apply #'wg::graphics.message message others))
Z:/siscog/crews-vdev/crews/win-maps/background.cl: (defun wg::pathname.defaults ()
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl:       (dolist (pair (wg::windows self))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 		  (npush (list :elevator wg::*spliter-size*) windows)
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 		      (npush (list (intern (format nil "~aELEVATOR" count) :keyword) wg::*spliter-size*) windows)))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	    (incf min.height wg::*spliter-size*)
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	  (decf min.height wg::*spliter-size*))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: (defmethod wg::reset.windows.constraints ((self most.basic.state.window))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	   (constraints (wg::rest.constraints (get.constraints self (configuration self))))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	   (remaining.height (nth-value 3 (wg::window.inside.position.and.size self))))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 		       (let ((name (wg::constraint.name c))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 			     (space (wg::constraint.space c)))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	  (let ((name (wg::constraint.name c))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 		(space (wg::constraint.space c)))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 		(wg::change.constraint key constraints (list key (max height 0))))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	      (wg::change.constraint key constraints (list key :even))))))))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl:   (or (assoc config (wg::constraints self))
Z:/siscog/crews-vdev/crews/win-maps/basic-state.cl: 	(push configuration (wg::constraints self))
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: 	    (wg::bitblt self +close.pixmap+ 16 14 0 0 x y)))))))
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: 	    (cg:with-foreground-color (window (color-rgb (getf (wg::p.list gcontext) :FOREGROUND-COLOR)))
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: (defmethod wg::current.documentation.hotspot ((self board.title.window))
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: (defmethod wg::current.documentation.hotspot (window)
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: (defmethod wg::hotspot.tooltip ((self board.title.window))
Z:/siscog/crews-vdev/crews/win-maps/frames.cl:     (wg::hide.tooltip)
Z:/siscog/crews-vdev/crews/win-maps/frames.cl: 	(wg::display.tooltip self self))
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:     :crews.foreground.color 	*crews.foreground.color* ;from def.color wg::crews.foreground.color
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:     :crews.background.color 	*crews.background.color* ;from def.color wg::crews.background.color
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:      (let* ((i (cg:rgb-blue (color-rgb (getf (wg::p.list (get.gcontext :tasks.gcontext)) :background-color))))
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :crews.foreground.color (find.tasks.fore.color *city.night.tasks.background.color*) ;from def.color wg::crews.foreground.color
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :crews.background.color *city.night.tasks.background.color* ;from def.color wg::crews.background.color
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :maps.font		(wg::get.system.font "Verdana" 16) 
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :small.maps.font	(wg::get.system.font "Verdana" 13) 
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :big.maps.font		(wg::get.system.font "Verdana" 19)
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :bold.maps.font		(wg::get.system.font "Verdana" 16 :style '(:bold))
Z:/siscog/crews-vdev/crews/win-maps/gcontexts.cl:       :board.titles.maps.font	(wg::get.system.font "Verdana" 16)
Z:/siscog/crews-vdev/crews/win-maps/hotspots.cl:       (wg::nhotspot.object.size string.size self window)
Z:/siscog/crews-vdev/crews/win-maps/personnel-tasks.cl:          (let ((aux.box (wg::string.size.in window str frontier)))
Z:/siscog/crews-vdev/crews/win-maps/roster-printout.cl:     (with.gcontext (printer (get.font.gcontext (wg::font.face (wg::get.gcontext.font (get.general.information.gcontext))) 
Z:/siscog/crews-vdev/crews/win-maps/roster-printout.cl: 					       (wg::font.size (wg::get.gcontext.font (get.general.information.gcontext))) 
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl: 		   (wg::print.station.in.balloon start.station stream (box.top.left sta.box) 
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl: 		   (wg::print.station.in.balloon end.station stream (box.top.left sta.box) 
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl:     (wg::reset.duty.id.balloons)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl:     (wg::reset.end.station.balloons)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl:     (wg::reset.duty.id.balloons)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-printout.cl:     (wg::reset.end.station.balloons)
Z:/siscog/crews-vdev/crews/win-maps/tasks-predicates.cl: 		       :background-color (color-rgb (getf (wg::p.list (get.gcontext :has.not.version.in.server.color.gc))
Z:/siscog/crews-vdev/crews/win-maps/tasks-predicates.cl: 		       :background-color (color-rgb (getf (wg::p.list (get.gcontext :has.changed.version.in.server.color.gc))
Z:/siscog/crews-vdev/crews/win-maps/tasks-predicates.cl: 		       :background-color (color-rgb (getf (wg::p.list (get.gcontext :may.delete.version.in.server.color.gc))
Z:/siscog/crews-vdev/crews/win-maps/allocator/hotspots.cl:   (defmethod wg::mouse.left.release.in.world ((world lines.world) (window EMPLOYEES.ALLOCATION.WINDOW.PANE) d1 d2)
Z:/siscog/crews-vdev/crews/win-maps/allocator/hotspots.cl:   (defmethod wg::mouse.left.release.in.world ((world lines.world) (window ANONYMOUS.ALLOCATION.WINDOW.PANE) d1 d2)
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl:   (let ((wg::*delta.x* 100)
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl:         (wg::*delta.y* 50)
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl:         (wg::*delta.height* 100))
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl: 	  (wg::window.inside.size (cg:parent pane))
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl: (defmethod wg::object.info.size ((self dependencies.graph) object)
Z:/siscog/crews-vdev/crews/win-maps/data-manager/files-network.cl:   (let ((info (funcall (wg::object.info self) object))
Z:/siscog/crews-vdev/crews/win-maps/dispatcher/basic-boards.cl: 	     (wg::dash.number.to.point self (position.x x.pos)))))
Z:/siscog/crews-vdev/crews/win-maps/dispatcher/basic-boards.cl: 	     (ww            	(wg::page.box window))
Z:/siscog/crews-vdev/crews/win-maps/dispatcher/basic-boards.cl: 	     (last.point 	(when (last.time.x self) (wg::dash.number.to.point self (position.x (last.time.x self)))))
Z:/siscog/crews-vdev/crews/win-maps/dispatcher/basic-boards.cl: (defmethod wg::draw.all.dashes :after ((self disp.ruler.with.world))
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl:     (let ((wg::*delta.x* 40)
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl: 	  (wg::*delta.y* 5))
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl: (defmethod wg::deeper.object? ((self navigator.ipt.graph.world) object level)
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl:     :background-color (wg::color-rgb wg::*default.window.background.color*)
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl:     :foreground-color (wg::color-rgb wg::*default.window.foreground.color*)
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl:     (let ((wg::*delta.x* 40)
Z:/siscog/crews-vdev/crews/win-maps/ipt/ipt-draw-graph.cl: 	  (wg::*delta.y* 5))
Z:/siscog/crews-vdev/crews/win-maps/load-scheduler/select-new-start-time-dialog.cl:     (setf (cg:value valid.intervals.notice.board) (wg::valid.time.range.message new.start.time))
Z:/siscog/crews-vdev/crews/win-maps/recorder/log-in-dialog.cl: 	       (when (typep w 'wg::report.msg.dialog)
Z:/siscog/crews-vdev/crews/win-maps/scheduler/ips-time-space.cl:      (wg::remove.object info.line filter.stations.hotspot) 
Z:/siscog/crews-vdev/crews/win-maps/scheduler/ips-time-space.cl: 	    (wg::remove.object info.line self) 
Z:/siscog/crews-vdev/crews/win-maps/scheduler/time-space-diag.cl:                                                               (stable-sort items #'string< :key #'wg::menu.item.display)))))
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/background.cl:       (let ((current.node.hotspot (awhen (wg::select.objects.by.object (get.world search.tree.window) current.node)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/basic-state.cl: 			     (awhen (wg::dashes ruler.pane)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/basic-state.cl: 			       (return-from get.state.window.dashes (mapcar #'(lambda (pos) (wg::dash.number.to.point ruler.pane pos)) it)))))))
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/search-tree.cl: (defclass tree.radiation (wg::radiation)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/search-tree.cl:     (let ((wg::*delta.x* 40)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/search-tree.cl: 	  (wg::*delta.y* 5))
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/search-tree.cl:     (let ((wg::*delta.x* 40)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/search-tree.cl: 	  (wg::*delta.y* 5))
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/semi-automatic-search-panel.cl: 	    :widget.parser 'wg::valid.real.range?
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/successors.cl: (defmethod wg::mouse.left.press or ((canvas SUCCESSORS.INFO.WINDOW.PANE) mouse.char position)
Z:/siscog/crews-vdev/crews/win-maps/scheduler-roster/successors.cl: (defmethod wg::mouse.double.click.in.world ((self LINES.WORLD) (canvas SUCCESSORS.INFO.WINDOW.PANE) mouse.char position)




(progn
  
  (query-replace-modif "nstring.capitalize" "nstring-capitalize" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "string.capitalize" "string-capitalize" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "nstring.downcase" "nstring-downcase" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "nstring.upcase" "nstring-upcase" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "string.downcase" "string-downcase" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "string.upcase" "string-upcase" '("lisp" "cl" "bil") *_dirs*)
  (query-replace-modif "string.equal" "string-equal" '("lisp" "cl" "bil") *_dirs*)
  
  (query-replace-modif "cg::position-x" "position.x" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::position-y" "position.y" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::with-hourglass" "with.sand.glass" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::nmake-box" "nmake.box" '("cl") *_dirs*)
  (query-replace-modif "cg::make-box" "make.box" '("cl") *_dirs*)
  (query-replace-modif "cg::make-box-relative" "make.box.relative" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::make-position" "make.position" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::box-union" "box.union" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::box-left" "box.left" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::box-top" "box.top" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::box-height" "box.height" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg::box-width" "box.width" '("cl" "bil") *_dirs*)

  (query-replace-modif "cg:position-x" "position.x" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:position-y" "position.y" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:with-hourglass" "with.sand.glass" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:nmake-box" "nmake.box" '("cl") *_dirs*)
  (query-replace-modif "cg:make-box" "make.box" '("cl") *_dirs*)
  (query-replace-modif "cg:make-box-relative" "make.box.relative" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:make-position" "make.position" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:box-union" "box.union" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:box-left" "box.left" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:box-top" "box.top" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:box-height" "box.height" '("cl" "bil") *_dirs*)
  (query-replace-modif "cg:box-width" "box.width" '("cl" "bil") *_dirs*)
  )

;;extra
(query-replace-modif "cg::closed-stream-p" "closed.p" '("lisp" "cl" "bil") *_dirs*)


(query-replace-modif "" "" '("cl" "bil") *_dirs*)

;; CG SYMBOLS

;;;(INVALIDATE 102)
;;;(DRAW-STRING-IN-BOX 37)
;;;(POSITION-X 17) -> position.x
;;;(UPDATE-MENU 15)
;;;(MAKE-BOX 15) -> make.box
;;;(DRAW-LINE 13)
;;;(WITH-DELAYED-REDRAW 13)
;;;(POSITION-Y 12) -> position.y
;;;(SCROLL-TO 10)
;;;(MAKE-BOX-RELATIVE 9) -> make.box.relative
;;;(WITH-HOURGLASS 8) -> with.sand.glass
;;;(CLOSED-STREAM-P 6)
;;;(MAKE-POSITION 5) -> make.position
;;;(BOX-UNION 3) -> box.union
;;;(BOX-TOP 2)
;;;(BOX-HEIGHT 2)
;;;(BOX-WIDTH 2)
;;;(POP-UP-MODAL-DIALOG 1)
;;;(NMAKE-BOX 1)
;;;(CLOSE 1)
;;;(NPOSITION- 1)
;;;(DRAW-BOX 3)

;;;((VALUE 2287) (AVAILABLE 1167) (PARENT 984) (RANGE 775) (STATE 230) (PLIST 166)
;;; (FLAG-MODAL-COMPLETION 141) (STATIC-TEXT 137) (RGB 117)
;;; (TITLE 98) (READ-ONLY 90) (DEFAULT-PANE-CLASS 88) (NAME 83) (TEXTURE 51)
;;; (TEXTURE-INFO 50) (CHECK-BOX 49) (VALUE-PLIST 46) (LIST-WIDGET-ADD-ITEM 45)
;;; (FIND-COMPONENT 44) (SELECTED 44) (ON-CHANGE 42) (DRAW-STRING-IN-BOX 37)
;;; (LIGHT-GRAY 35) (MAKE-RGB 34) (MAKE-WINDOW 34) (GROUP-BOX 33)
;;; (LIST-WIDGET-REMOVE-ITEM 31) (INTERIOR 28) (EXTERIOR 22) (WIDTH 21)
;;; (DEFAULT-BUTTON 21) (CANCEL-BUTTON 21) (FIND-APPLICATION-WINDOW 20)
;;; (OUTLINE-ITEM 19) (TOP 19) (COMBO-BOX 18) (FIND-OUTLINE-ITEM 17) 
;;; (SUBSECTIONS 16) (RIGHT-ATTACHMENT 16) (BOTTOM-ATTACHMENT 16) (RADIO-BUTTON 16)
;;; (FOREGROUND-COLOR 15) (BACKGROUND-COLOR 15) (BLACK 15) (UPDATE-MENU 15)
;;; (FIND-OR-MAKE-APPLICATION-WINDOW 15) (MAKE-BOX 15) (DATA-OBJECT 14)
;;; (PICTURE-BUTTON 14) (EDITABLE-TEXT 14) (LIST-VIEW-ITEM 13) (DRAW-LINE 13)
;;; (COLORS 13) (WITH-DELAYED-REDRAW 13) (USER-CLOSE 13) (DRAW-CELL 12)
;;; (LIST-WIDGET-GET-INDEX 12) (LEFT-ATTACHMENT 12)
;;; (TOP-ATTACHMENT 12) (RESIZE-WINDOW 12) (BUTTON 12) (GRID-DRAW-STRING 11)
;;; (SCROLL-TO 10) (HEIGHT 10) (MENU-ITEMS 10) (ADD-ROW 9)
;;; (CELL-HORIZONTAL-JUSTIFICATION 9) (SET-FOCUS-COMPONENT 9) (MAKE-BOX-RELATIVE 9)
;;; (WITH-FOREGROUND-COLOR 9) (WINDOWS 9) (GRID-ROW-SECTION 8) (GRID-COLUMN-SECTION 8)
;;; (KIND 8) (WITH-HOURGLASS 8) (OPEN-OUTLINE-ITEM-VALUE 8) (LEFT-MARGIN 8)
;;; (LIST-WIDGET-MOVE-ITEM 8) (ADD-OUTLINE-ITEM 7) (REMOVE-OUTLINE-ITEM-VALUE 7)
;;; (INVALIDATE-COMPONENT 7) (SYSTEM-DIALOG-BACKGROUND-COLOR 7) (FONT-STRING-WIDTH 7)
;;; (RIGHT-MARGIN 7) (WITH-LINE-WIDTH 7) (WHITE 7) (TITLE-BAR-HEIGHT 7)
;;; (INVALIDATE-WINDOW 7) (GRID-COLUMN 6) (FILL-TEXTURE 6) (LIST-VIEW-CELL-VALUE 6)
;;; (ON-PRINT 6) (CONTROL-KEY 6) (*WITH-CURSOR* 6) (CYAN 6) (GREEN 6) (DARK-YELLOW 6)
;;; (DARK-GREEN 6) (FILL-BOX 6) (BOTTOM 6) (TAB-CONTROL 6) (NON-REFRESHING-PANE 6)
;;; (NON-REFRESHING-WINDOW 6) (CLOSED-STREAM-P 6) (FONT-SIZE-COMBO-BOX 6)
;;; (FONT-FACE-COMBO-BOX 6) (CELL-VERTICAL-JUSTIFICATION 5)
;;; (EDITABLE-TEXT-COLUMN-MIXIN 5) (TOOLTIP 5) (ADD-ITEM 5) (CELL-WRAPPED-P 5)
;;; (RICH-TEXT 5) (REMOVE-TAB 5) (SCROLL-CURRENT-INDEX-INTO-VIEW 5) (FIND-ITEM 5)
;;; (MAGENTA 5) (YELLOW 5) (RED 5) (DARK-CYAN 5) (DARK-RED 5) (TIMER-INFO 5)
;;; (BOTTOM-MARGIN 5) (TOP-MARGIN 5) (BOX 5) (CURSOR-POSITION 5) (MAKE-POSITION 5)
;;; (OPEN-STREAM 5) (DARK-BLUE 5) (SET-FOCUS 5) (LISP-GROUP-BOX 5) (SCROLL-POSITION 5)
;;; (LEFT 5) (MENU 5) (GRID-WIDGET 4) (WRITE-CELL-VALUE 4) (WIDGET-ROW-MIXIN 4)
;;; (COLUMN-HEADER-ROW 4) (ROW-HEADER-COLUMN 4) (SET-SELECTION 4)
;;; (ADD-OUTLINE-ITEM-VALUE 4) (RIGHT-MOUSE-BUTTON 4) (LIST-VIEW-COLUMN 4) (BLUE 4)
;;; (DARK-MAGENTA 4) (DARK-GRAY 4) (STOP-TIMER 4) (PIXMAP-NAME 4) (SHIFT-KEY 4)
;;; (FIND-OR-MAKE-POP-UP-WINDOW 4) (LABEL 4) (TAB-INFO 4) (VISIBLE-BOX 4)
;;; (MENU-SEPARATOR 4) (UP-DOWN-CONTROL 4) (CELL-AVAILABLE 3) (CELL-CLICK 3)
;;; (VISIBLE-RANGE 3) (ROW-SECTION 3) (CELL-BACKGROUND-COLOR 3) (FIND-PIXMAP 3)
;;; (CLOSE-OUTLINE-ITEM-VALUE 3) (REMOVE-ITEM 3) (WINDOW 3) (COLUMNS 3)
;;; (START-TIMER 3) (TIMER 3) (BOX-UNION 3) (BUDDY-WIDGET 3) (LIST-WIDGET-SET-INDEX 3)
;;; (WINDOW-UNDER-MOUSE 3) (MOUSE-BUTTON-STATE 3) (DRAW-BOX 3) (PO-XOR 3) (RGB-BLUE 3)
;;; (MOVE-WINDOW 3) (DELAYED 3) (INTERIOR-WIDTH 3) (ABOUT-TO-SHOW-MENU 3)
;;; (DEVELOPMENT-MAIN-WINDOW 2) (INVALIDATE-CELL 2) (LEFT-MOUSE-BUTTON 2)
;;; (ADD-COLUMN 2) (CACHED-COMPONENTS 2) (DELETE-ROW 2) (COMBO-BOX-COLUMN-MIXIN 2)
;;; (RGB-P 2) (WITH-PAINT-OPERATION 2) (*DEFAULT-CG-BINDINGS* 2) (COPY-MENU-ITEM 2)
;;; (WINDOW-TO-SCREEN-UNITS 2) (FRAME-WITH-SINGLE-CHILD 2) (DIALOG-ITEMS 2)
;;; (LEAF-PIXMAP-NAME 2) (GET-POSITION 2) (FOCUS-INDEX 2) (OUTLINE 2)
;;; (MOUSE-RIGHT-DOWN 2) (MULTI-LINE-EDITABLE-TEXT 2) (ADD-COMPONENT 2)
;;; (NVISIBLE-BOX 2) (BOX-TOP 2) (COPY-TO-STREAM 2) (BOX-HEIGHT 2) (BOX-WIDTH 2)
;;; (DIALOG 2) (GRAY 2) (BITMAP-WINDOW 2) (REDISPLAY-WINDOW 2)
;;; (LIST-WIDGET-REPLACE-ITEM 2) (DRAW-ELLIPSE-SECTOR 2) (VISIBLE-BOX-HEIGHT 2)
;;; (MOUSE-RIGHT-UP 2) (ALT-KEY 2) (COMMON-STATUS-BAR 2) (DATA-WRITER 1)
;;; (CELL-AND-SECTIONS-AVAILABLE 1) (ROW-SECTION-WITH-SORT-GADGET-MIXIN 1)
;;; (COLUMN-SECTION 1) (SCROLL-SECTION 1) (INVALIDATE-SECTION 1) (READ-CELL-VALUE 1)
;;; (FORM-P 1) (IMAGE-LIST 1) (RESTORE-TAB 1) (FIND-TAB 1) (UPDATE-WINDOW 1)
;;; (EXPAND-WINDOW 1) (INVALIDATE-WHOLE-SECTION 1) (TRACE-FORMAT 1)
;;; (SET-FIRST-VISIBLE-LINE 1) (CREATION-PROCESS 1) (SCROLL-RANGE 1) (NPOSITION- 1)
;;; (ADD-TO-MENU 1) (RICH-EDIT-PRINT 1) (TABS 1) (TRACK-LIMITS 1)
;;; (PLAIN-TO-RICH-TEXT 1) (READ-TEXT 1) (CLOSE 1) (SET-CHARACTER-FORMAT 1)
;;; (SET-PARAGRAPH-FORMAT 1) (OUTLINE-ITEM-FROM-INDEX 1) (RGB-GREEN 1) (RGB-RED 1)
;;; (CHANGE-OUTLINE-ITEM-VALUE-AVAILABILITY 1) (NMAKE-BOX 1) (OPENED-PIXMAP-NAME 1)
;;; (CLOSED-PIXMAP-NAME 1) (INDEX-FROM-OUTLINE-ITEM-VALUE 1) (PAGE-HEIGHT 1)
;;; (POP-UP-MODAL-DIALOG 1) (MAKE-FONT-EX 1) (MULTI-ITEM-LIST 1) (SINGLE-ITEM-LIST 1)
;;; (SELECT-ON-RIGHT-CLICK 1) (DIALOG-ITEM 1) (WIDGET-DEVICE 1) (OUTLINE-TOP-PANE 1)
;;; (REMOVE-ALL-ITEMS 1) (REMOVE-ALL-COLUMNS 1) (SMALL-IMAGE-LIST 1) (LIST-VIEW 1)
;;; (ACTIVE 1) (STREAM-UNITS-PER-INCH 1) (POP-UP-PRINTER-SETUP-DIALOG 1)
;;; (BRING-WINDOW-TO-FRONT 1) (MULTI-PICTURE-BUTTON 1) (BUTTON-INFO 1)
;;; (DEFAULT-VALUE 1) (DEFCOMPONENT 1) (ORIENTATION 1) (PAPER-SIZE 1) (NEW-PAGE 1)
;;; (MOUSE-LEFT-UP 1) (GET-BOX 1) (MOUSE-LEFT-DOWN 1) (WITH-LINE-DASHING 1) (BORDER 1)
;;; (WITH-FONT 1) (WITH-DEVICE-CONTEXT 1) (DRAW-CIRCLE 1) (SET-SCROLL-RANGE 1)
;;; (SET-PAGE-SIZE 1) (VISIBLE-BOX-WIDTH 1) (DRAW-ROUNDED-BOX 1) (DIALOG-MIXIN 1)
;;; (PO-INVERT 1) (MOUSE-MOVED 1) (MOUSE-OUT 1) (ADD-APPLICATION-WINDOW 1)
;;; (DEVICE-CLOSE 1) (ID 1) (FILL-ELLIPSE-SECTOR 1) (SELECT-WINDOW 1)
;;; (INTERIOR-HEIGHT 1) (HANDLE 1))



(defun query-replace-modif (pattern replace file-types directories &optional comment)
  (let ((type-regexps nil)
	(old-buffer (current-buffer))
	(old-label *history-label*))
    (setq *history-label* (or comment (format "%s -> %s" pattern replace)))
    (save-excursion
      (dolist (type file-types)
	(setq type-regexps (cons (format "\.%s$" type) type-regexps)))
      
      (let ((old-font-lock-maximum-size font-lock-maximum-size))
	(setf font-lock-maximum-size 0)
	(unwind-protect
	    (dolist (dir directories)
	      (if (file-directory-p dir)
		  (query-replace-modif-directory pattern replace type-regexps dir)))
	  (setf font-lock-maximum-size old-font-lock-maximum-size)))
      
      (set-buffer old-buffer)
      (setq *history-label* old-label)
      
      )))


(defun query-replace-modif-directory (pattern replace type-regexps directory)
  (dolist (type-regexp type-regexps)
    (dolist (file (directory-files directory t type-regexp))
      (if (not (file-directory-p file))
	  (query-replace-modif-file pattern replace file))))
  (dolist (dir (directory-files directory t "^[^\.]"))
    (if (file-directory-p dir)
	(query-replace-modif-directory pattern replace type-regexps dir))))


(defun query-replace-modif-file (pattern replace file)
  (let* ((old-buffer (get-file-buffer file))
	 (buffer (or old-buffer
		     (find-file-noselect file)))
	 (need.save.p nil))
    (set-buffer buffer)
    (save-excursion
      (let ((end nil))
	(beginning-of-buffer)
	(while (search-forward pattern nil t)
	  (when (save-excursion 
		  (beginning-of-line)
		  (not (looking-at "[ \t]*;")))
	    (switch-to-buffer buffer)
	    (let ((end (point)))
	      (set-mark end)
	      (backward-char (length pattern))
	      (if (y-or-n-p "Replace?")
		  (progn
		    (setf need.save.p t)
		    (delete-region (point) end)
		    (insert replace)
		    (set-definition-changed))
		  (goto-char end)))))))
    (when need.save.p
	(save-buffer buffer))
;;;    (unless old-buffer
;;;      (kill-buffer buffer))
    ))

;;;(defun query-replace-modif-file (pattern replace file)
;;;  (let* ((old-buffer (get-file-buffer file))
;;;	 (buffer (or old-buffer
;;;		     (find-file-noselect file)))
;;;	 (need.save.p nil))
;;;    (set-buffer buffer)
;;;    (save-excursion
;;;      (let ((end nil))
;;;	(beginning-of-buffer)
;;;	(while (search-forward pattern nil t)
;;;	  (when (save-excursion 
;;;		  (beginning-of-line)
;;;		  (not (looking-at "[ \t]*;")))
;;;	    (switch-to-buffer buffer)
;;;	    (when (= 34 (char-after (point)))
;;;	      (forward-char 1)
;;;	      (let ((end (point))
;;;		    (til nil))
;;;		(set-mark end)
;;;		(backward-char 2)
;;;		(while (not (= 34 (char-after (point))))
;;;		  (when (= 126 (char-after (point)))
;;;		    (setf til t))
;;;		  (backward-char 1))
;;;		(if (and (not til) (y-or-n-p "Replace?"))
;;;		    (progn
;;;		      (unless (= 126 (char-after (- (point) 1)))
;;;			(insert "~"))
;;;		      (forward-char 1)
;;;		      (insert "~")
;;;		      (while (not (= 34 (char-after (point))))
;;;			(forward-char 1))
;;;		      (forward-char 1)
;;;		      (setf need.save.p t)
;;;		      (sc-indent-region)
;;;		      (set-definition-changed))
;;;		    (goto-char end))))))))
;;;    (unless old-buffer
;;;      (when need.save.p
;;;	(save-buffer buffer))
;;;      (kill-buffer buffer))))




;;;(ola ~"~OK")

;;; Original definition
(defun ediff-file-with-original ()
  (let ((mod (current-crews-mod))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/" (crews-mod-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (if (file-exists-p (format "%s/%s" (crews-mod-org-dir mod) source-file))
	      (ediff-files (format "%s/%s" (crews-mod-org-dir mod) source-file) filename)
	      (beep-message (format "The file %s does not exist in the original dir %s" source-file (crews-mod-org-dir mod)))))
	(beep-message (format "The file %s does not match the source dir %s" filename (crews-mod-src-dir mod)))))
  )


(defun ediff-file-with-original ()
  (let ((origin "z:/siscog-8.0.0")
	(mod (current-crews-mod))
	(filename (buffer-file-name (current-buffer))))
    (if (string-match (format "^%s/" (crews-mod-src-dir mod)) filename)
	(let ((source-file (substring filename (match-end 0))))
	  (if (file-exists-p (format "%s/%s" origin source-file))
	      (ediff-files (format "%s/%s" origin source-file) filename)
	      (beep-message (format "The file %s does not exist in the original dir %s" source-file origin))))
	(beep-message (format "The file %s does not match the source dir %s" filename (crews-mod-src-dir mod)))))
  )















get-crews-dirs (crews-p all crews-x data gui)


(grepfc '("rosters") '("cl") (get-crews-dirs t t t nil t)  nil nil)

(grepfc '("employees.print.setup.dialog") '("cl") (get-crews-dirs t t t nil t)  nil nil)



;;; Handle meter reports
(grepfc '("SCHEDULER.ST.DATA.LOADER.AFTER") '("txt") (list "E:/meter-reports-drivers/drivers") nil nil)
(grepfc '("LOAD.RSR	" "2 (D) SG-SELECT-COLUMNS") '("txt") (list "E:/meter-reports-total") nil nil)

(grepfc '("0 INITIALIZE.SHORT.TERM.STATE") '("txt") (list "E:/meter-reports-drivers/drivers") nil nil)

(grepfc '("0 SAVE.SCHEDULER.SHORT.TERM") '("txt") (list "E:/meter-reports-drivers/drivers") nil nil)


0 INITIALIZE.SHORT.TERM.STATE

(while (search-forward ".txt:   " nil t)
       (if (looking-at "1 LOAD.RSR")
	   (progn
	     (beginning-of-line)
	     (forward-line 2))
	   (progn
	     (beginning-of-line)
	     (kill-line)
	     (kill-line))))
