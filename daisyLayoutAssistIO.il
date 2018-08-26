


;------------------------------------------------------------------------
; Structure for keeping track of a text object
;------------------------------------------------------------------------
(defstruct abTextCellView libName cellName viewName fileName fullPath mode port)

/************************************************************************
*       *
*  (abOpenTextCellView libName cellName viewName @optional (mode "r")   *
*   (viewType "text") (openPort t))     *
*       *
*  Open up a text cellView in the same kind of way as a CDBA cellView.  *
* The cellView may be opened in "r","a", or "w" modes - default is "r". *
*  The return value is a structure instance, of which the most useful   *
*   slot is the port slot - this enables you to read or write to the    *
* file in the cellView. *
* Can choose the registered viewType, and also whether it should open   *
* a port or not; useful to turn off if you want to edit with an editor  *
*       *
************************************************************************/

(procedure (abOpenTextCellView libName cellName viewName @optional (mode "r")
       (viewType "text") (openPort t))
   (let ((fileTail (ddMapGetViewTypeFileName viewType))
fileObj fileName lockId portId textObj
(locked t))
(setq fileObj (ddGetObj libName cellName viewName fileTail nil mode))
(when fileObj
     (if (member mode '("a" "w"))
 (progn
  ;------------------------------------------------------
  ; Lock the file if in "a" or "w" mode
  ;------------------------------------------------------
  (setq fileName (getq fileObj writePath))
  (setq lockId (ddLockPath fileName))
  ;------------------------------------------------------
  ; If lockable, then try to open the file
  ;------------------------------------------------------
  (setq locked nil)
  (when (ddLockSet lockId "w" nil)
(if
 (and openPort
      (null (setq portId (outfile fileName mode)))
      )
 ;-----------------------------------------------
 ; if it couldn't be opened, free the lock
 ;-----------------------------------------------
 (ddLockFree lockId)
 ;-----------------------------------------------
 ; Otherwise say it was locked
 ;-----------------------------------------------
 (setq locked t)
 )
)
  )
 (progn
  ;------------------------------------------------------
  ; Otherwise if read mode, open file for read - don't
  ; bother locking it
  ;------------------------------------------------------
  (setq fileName (getq fileObj readPath))
  (when openPort
(setq portId (infile fileName))
)
  )
 )
     ;-----------------------------------------------------------
     ; If the file was opened OK, create the structure
     ; and it was locked (or readonly)
     ;-----------------------------------------------------------
     (when (and locked (or portId (null openPort)))
   (setq textObj
 (make_abTextCellView
  ?libName libName
  ?cellName cellName
  ?viewName viewName
  ?fileName fileTail
  ?fullPath fileName
  ?mode mode
  ?port portId
  )
 )
   )
     )
;-----------------------------------------------------------------
; Return the text object
;-----------------------------------------------------------------
textObj
))

/***********************************************************************
*      *
*    (abCloseTextCellView textObj)     *
*      *
*    Close a text cellView, referenced by the textObj structure, as    *
* returned by abOpenTextCellView. This takes care of closing the file, *
*      removing the lock, and cleaning up if the file was empty.       *
*      *
***********************************************************************/

(procedure (abCloseTextCellView textObj)
   (let (lockId)
;-----------------------------------------------------------------
; Close the file itself
;-----------------------------------------------------------------
(when (portp (getq textObj port))
     (close (getq textObj port)))
;-----------------------------------------------------------------
; Mark the port as nil, just to avoid accidents
;-----------------------------------------------------------------
(putpropq textObj nil port)
;-----------------------------------------------------------------
; Free any locks on the file (if there were any)
;-----------------------------------------------------------------
(setq lockId (ddLockPath (getq textObj fullPath)))
(ddLockFree lockId)
;-----------------------------------------------------------------
; Release the object. This has the benefit of removing the cellView
; if the text file was empty
;-----------------------------------------------------------------
(ddReleaseObj
(ddGetObj
 (getq textObj libName)
 (getq textObj cellName)
 (getq textObj viewName)
 (getq textObj fileName)
 )
)
t
))

procedure( daisyRNCreateCorrespondenceTable()
    let( (window libName cellName corrFile)
        window = hiGetCurrentWindow()
        libName = window->cellView->libName
        cellName = window->cellView->cellName
        corrFile = abOpenTextCellView(libName cellName "conn" "w") ; Change to read/write and read whatever is defined there first
        let( (schematic layout)
            schematic = dbOpenCellViewByType(libName cellName "schematic")
            layout = dbOpenCellViewByType(libName cellName "layout")
            (setq schematic_instances makeTable("schematicInstances"))
            (setq layout_instances makeTable("layoutInstances"))
            ;println(schematic->instances~>??)
            (foreach inst schematic->instances
                (setq simLibs list("basic" "analogLib"))
                (if (member inst->libName simLibs) then
                    printf(strcat("Instance " inst->name " - " inst->cellName " is from a Simlib - ignoring\n"))
                else
                    let( ((terms list()))
                        (foreach term inst->instTerms
                            terms = append(list(term->name) terms)
                        )
                        schematic_instances[inst->name] = terms
                    )
                )
            )
            (foreach inst layout->instances
                (setq simLibs list("basic" "analogLib"))
                (if (member inst->libName simLibs) then
                    printf(strcat("Instance " inst " is from a Simlib - ignoring\n"))
                else
                    ;println(inst->master->terminals~>??)
                    let( ((terms list()))
                        (foreach term inst->master->terminals
                            terms = append(list(term->name) terms)
                        )
                        layout_instances[inst->name] = terms
                    )
                )
            )
            ; Check corresponding schematic instances based on the schematic instances
            printf("Starting to match instances from layout to schematic\n")
            (let ((matched list()) (unmatched list()))
                (foreach inst layout_instances
                    ;println(inst)
                    ;println(schematic_instances[inst])
                    (if (listp(schematic_instances[inst]) && sort(schematic_instances[inst] nil) == sort(layout_instances[inst] nil)) then
                        ; Matching names and terms
                        printf(strcat("Instance " inst " is matched.\n"))
                        ;println(schematic_instances[inst])
                        ;println(layout_instances[inst])
                        ;println(sort(schematic_instances[inst] nil))
                        ;println(sort(layout_instances[inst] nil))
                            
                        ; Since match we add them to the matched list
                        matched = append(list(inst layout_instances[inst]) matched)
                    else
                        printf(strcat("Instance " inst " is NOT matched\n"))
                            
                        ; Since unmatch we add them to the unmatched list
                        unmatched = append(list(inst layout_instances[inst]) unmatched) 
                    )
                )
                print(list(list("schematic" schematic_instances~>??) list("layout" layout_instances~>??) list("matched" matched) list("unmatched" unmatched)) corrFile->port)
                (if null(unmatched) then
                    printf("All instances matched!\n")
                )
            )
        )
        abCloseTextCellView(corrFile)
    )
)