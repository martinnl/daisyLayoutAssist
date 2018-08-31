/* functionName(para1, para2)
 * Parameters
 * - para1
 * - para2
 *
 * Description
 *
 * T1ODO
 * - 
 */


/************************************************************************
 * Fixed above
 * TODO Fix below
 ***********************************************************************/

    
(procedure daisyLABindInstances(tree treeField)
    data = hiGetCurrentWindow()->data
    selItems = hiTreeTableGetSelectedItems(treeField)
    selItemsCount = hiTreeTableGetSelectedItemCount(treeField)
    
    (cond
        (selItemsCount > 2
            println("Cannot currently bind more than 2 items")
        )
        (selItemsCount == 2
            item1 = car(selItems)
            item2 = car(cdr(selItems))
            desc1 = hiGetTreeItemDescription(eval(item1))
            desc2 = hiGetTreeItemDescription(eval(item2))
            
            (let (schDesc layDesc (wrongInput nil))
                (cond
                    (nth(0 desc1) == "-" && nth(1 desc1) != "-" && nth(0 desc2) != "-" && nth(1 desc2) == "-"
                        schDesc = desc2
                        layDesc = desc1
                    )
                    (nth(0 desc1) != "-" && nth(1 desc1) == "-" && nth(0 desc2) == "-" && nth(1 desc2) != "-"
                        schDesc = desc1
                        layDesc = desc2
                    )
                    (t
                        println("A schematic and layout instance has to be selected.")
                        wrongInput = t
                    )
                )
                
                (cond
                    (wrongInput
                        println("Aborting")
                    )
                    (t
                        ; Create form to map pins
                        ; Display form and get result
                        ; Store in bindings and regenerate tree
                        schTerms = nth(1 daisyLAFindTermsInMatchedBindingsFromInstName(data->bindings->unmatched_schematic nth(0 schDesc)))
                        layTerms = nth(2 daisyLAFindTermsInMatchedBindingsFromInstName(data->bindings->unmatched_layout nth(1 layDesc)))
                        (if daisyLACreateAndDisplayBindingWindow(data schDesc layDesc list() schTerms layTerms tree)
                            println("DONE")
                        )
                    )
                )
            )
        )
        (t
            ; Rebind existing binding
            item = car(selItems)
            desc = hiGetTreeItemDescription(eval(item))
            
            (let ((wrongInput nil))
                (if nth(0 desc) == "-" || nth(1 desc) == "-" then
                    println("A matched instance has to be selected.")
                    wrongInput = t
                )
                
                (cond
                    (wrongInput
                        println("Aborting")
                    )
                    (t
                        ; Create form to map pins
                        ; Display form and get result
                        ; Store in bindings and regenerate tree
    
                        terms = daisyLAFindTermsInMatchedBindingsFromInstName(data->bindings->matched nth(0 desc))
                        println(terms)
                        matchedTerms = nth(0 terms)
                        schTerms = nth(1 terms)
                        layTerms = nth(2 terms)
                        (if daisyLACreateAndDisplayBindingWindow(data desc nil matchedTerms schTerms layTerms tree)
                            println("DONE")
                        )
                    )
                )
            )
        )
    )
)

(procedure daisyLABindingFormBindTermsCB(form)
    schTermsValue = car(form->schTermsField->value)
    layTermsValue = car(form->layTermsField->value)
    
    form->schTermsField->choices = remove(schTermsValue form->schTermsField->choices)
    form->layTermsField->choices = remove(layTermsValue form->layTermsField->choices)
    
    form->bindedTermsField->choices = append(list(sprintf(nil "%s %s" schTermsValue layTermsValue)) form->bindedTermsField->choices)
)

(procedure daisyLABindingFormUnbindTermsCB(form)
    bindingTermsValue = car(form->bindedTermsField->value)
    terms = parseString(bindingTermsValue " ")
    schTermValue = nth(0 terms)
    layTermValue = nth(1 terms)
    
    form->bindedTermsField->choices = remove(bindingTermsValue form->bindedTermsField->choices)
    
    form->schTermsField->choices = cons(schTermValue form->schTermsField->choices)
    form->layTermsField->choices = cons(layTermValue form->layTermsField->choices)
)

(procedure daisyLAReplaceBinding(bindings newBinding)
    (let ((result list()))
        (foreach binding bindings
            (cond
                (car(binding) == car(newBinding)
                    result = cons(newBinding result)
                )
                (t
                    result = cons(binding result)
                )
            )
            println(binding)
        )
        
        result
    )
)

(procedure daisyLARemoveMatchedInstance(bindings instName)
    matched = bindings->matched
    println(matched)
    (let ((stillMatched list()))
        (foreach match matched
            (cond
                (car(match) == instName
                    ; Schematic
                    ; Skip
                    t
                )
                (nth(1 match) == instName
                    ; Schematic
                    ; Skip
                    t
                )
                (t
                    stillMatched = cons(match stillMatched)
                )
            )
        )
        stillMatched
    )
)

(procedure daisyLAPopulateNetsTreeView(netTree)
    ; Add nets
    (let ((nets daisyLAGetSchematicFromLayout(hiGetCurrentWindow()->cellView)->nets))
        (foreach net nets
            tmp = hiCreateTreeItem(gensym(net->name) list(net->name))
            hiTreeAppendItem(netTree tmp)
            tmp2 = hiCreateTree(gensym(net->name))
            hiItemInsertTree(tmp tmp2)
            instTerms = list()
            (foreach instTerm net->instTerms
                ;(if !member(instTerm->inst->baseName instTerms) then TODO ?
                    println(instTerms)
                    println(instTerm)
                    instTerms = append(list(instTerm->inst->baseName) instTerms)
                    hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(instTerm->inst->baseName) list(instTerm->inst->baseName instTerm->name "Not done" 13)))
                ;)
            )
        )
    )
)
; TODO Unbind instances

(procedure daisyLANetTreeCB(name itemSymList)
    geDeselectAll(hiGetCurrentWindow())
    clearAllHighlights()
    
    data = hiGetCurrentWindow()->data
    
    while(itemSymList
        itemSym=car(itemSymList)
        println(hiGetTreeItemDescription(eval(itemSym)))
        desc = hiGetTreeItemDescription(eval(itemSym))
        println(itemSym~>??)
        (case length(desc)
            ( 4
                ; Instance
                 instName = car(desc)
                 instTerm = nth(1 desc)
                 instStatus = nth(2 desc)
                 instNoConns = nth(3 desc)
             
                 println(instName)
                 println(instStatus)
                 println(instNoConns)
                 ; Highlight instance name
                 println(hiGetCurrentWindow()->cellView->instances)
                 
                 layInst = daisyLAGetLayoutInstanceFromName(data data->layoutWindow->cellView instName)
                geSelectObject(layInst)
                leZoomToSelSet() ; Zoom to instance
                hiZoomRelativeScale(hiGetCurrentWindow() 0.7)
             
                daisyLAHighlightdaisyLAForSelectedInstances(instTerm nil)
                daisyLATraceTerm(layInst instTerm nil)
             )
             ( 1
                 ; Net - TODO Highlight all terminals that belongs to net and draw flighlines
                 netName = car(desc)
                 println(netName)
             )
         )
        instName = car(desc)
     
        itemSymList=cdr(itemSymList)
     )
)

(procedure daisyLABindingTreeCB(name itemSymList)
    data = hiGetCurrentWindow()->data
    geDeselectAll(data->layoutWindow)
    
    (if daisyLASchWindowInData(data)
        geDeselectAll(data->schWindow)
    )
    
    while(itemSymList
        itemSym=car(itemSymList)
        println(hiGetTreeItemDescription(eval(itemSym)))
        desc = hiGetTreeItemDescription(eval(itemSym))
        
        parentTree = car(hiGetTreeParent(eval(car(hiGetTreeItemParent(eval(itemSym))))))
        (if !null(parentTree) then
            parentDesc = hiGetTreeItemDescription(eval(parentTree))
            schInstName = car(parentDesc)
            layInstName = nth(1 parentDesc)
            schCell = nth(3 parentDesc)
            layCell = nth(3 parentDesc)
            bindingStatus = nth(4 parentDesc)
            
            schTermFilter = nth(0 desc)
            layTermFilter = nth(1 desc)
        else            
            schInstName = car(desc)
            layInstName = nth(1 desc)
            schCell = nth(3 desc)
            layCell = nth(3 desc)
            bindingStatus = nth(4 desc)
            
            schTermFilter = "" ; No filter if we select instance
            layTermFilter = "" ; No filter if we select instance
        )
        
        (if schInstName != "-" && schInstName != "" && daisyLASchWindowInData(data) then
            schInst = daisyLAGetInstanceFromName(data->schWindow->cellView schInstName)
            hiRaiseWindow(data->schWindow)
            hiSetCurrentWindow(data->schWindow)
            geSelectObject(schInst)
            schHiZoomToSelSet()
            hiRaiseWindow(data->layoutWindow)
            hiSetCurrentWindow(data->layoutWindow)
            
            ; TODO Add highlight for schematic
            ;daisyLAHighlightdaisyLAForSelectedInstances(schTermFilter nil)
            ;daisyLATraceTerm(schInst schTermFilter nil)
        )
        
        (if layInstName != "-" then
            layInst = daisyLAGetInstanceFromName(data->layoutWindow->cellView layInstName)
            (if !null(layInst) then
                geSelectObject(layInst)
                leZoomToSelSet(data->layoutWindow) ; Zoom to instance
                hiZoomRelativeScale(data->layoutWindow 0.7)
                
                daisyLAHighlightdaisyLAForSelectedInstances(layTermFilter t)
                daisyLATraceTerm(layInst layTermFilter nil)
            )
        )
        
        itemSymList = cdr(itemSymList)
    )
)

(procedure daisyLAResetBindings(tree)    
    data = hiGetCurrentWindow()->data
    
    hiTreeRemoveAllItems(tree)
    
    (if !daisyLASchWindowInData(data)
        daisyLAOpenSchematic()
    )
    
    libName = data->libName
    cellName = data->cellName
    
    ; Perform auto bind
    (let (schematic layout (unmatched_schematic list()) (unmatched_layout list()))
        schematic = dbOpenCellViewByType(libName cellName "schematic")
        layout = dbOpenCellViewByType(libName cellName "layout")
        (setq schematic_instances makeTable("schematicInstances"))
        (setq layout_instances makeTable("layoutInstances"))
        
        ; Loop over schematic instances and check for their terminals and save in a struct
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
            
        (let ((terms list()))
            (foreach inst schematic_instances
                terms = list()
                       
                (foreach term sort(copy(schematic_instances[inst]) nil)
                    terms = append(list(list(term "-")) terms)
                )
                
                unmatched_schematic = append(list(list(inst "-" list(list() terms list()))) unmatched_schematic)
            )
        )
        
        ; Loop over layout instances, check for their terminals and save in a struct
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
            
        (let ((terms list()))
            (foreach inst layout_instances
                terms = list()
                       
                (foreach term sort(copy(layout_instances[inst]) nil)
                    terms = append(list(list("-" term)) terms)
                )
                
                unmatched_layout = append(list(list("-" inst list(list() list() terms))) unmatched_layout)
            )
        )
            
        data->bindings = ncons(nil)
        data->bindings->matched  = list()
        data->bindings->unmatched_schematic = unmatched_schematic
        data->bindings->unmatched_layout = unmatched_layout
    )
    
    daisyLAPopulateBindingsTreeWithData(data tree)
)

(procedure daisyLASaveBinding(data tree)
    daisyLAlog(0 "daisyLASaveBinding")
    
    libName = data->libName
    cellName = data->cellName
    
    packed = daisyLAPackBinding(data data->bindings)
    
    corrFile = abOpenTextCellView(libName cellName "conn2" "w") ; Change to read/write and read whatever is defined there first
    
    print(packed corrFile->port)
        
    abCloseTextCellView(corrFile)   
    
    packed
)

(procedure daisyLASaveBindingButtonCB(tree)
    data = hiGetCurrentWindow()->data
    
    daisyLASaveBinding(data tree)
)

(procedure daisyLALoadBindingButtonCB(tree)
    data = hiGetCurrentWindow()->data
    
    daisyLALoadBinding(data tree)
)

(procedure daisyLAAutoBindCB(tree)
    daisyLAAutoBind()
    daisyLAPopulateBindingsTreeWithData(hiGetCurrentWindow()->data tree)
)

(procedure daisyLADockForm()
    hiGetCurrentWindow()->data = ncons(nil)
    hiGetCurrentWindow()->data->layoutWindow = hiGetCurrentWindow()
    hiGetCurrentWindow()->data->libName = hiGetCurrentWindow()->data->layoutWindow->cellView->libName
    hiGetCurrentWindow()->data->cellName = hiGetCurrentWindow()->data->layoutWindow->cellView->cellName
    
    let( (fileLocation loadFileButton swin dockForm myDock)
        fileLocation = hiCreateFileSelectorField(
            ?name   'fileLocation
            ?mode   'existingFile
            ?prompt "Choose a file"
            ?editable       t)

        ; create a root tree
        indexTree=hiCreateTree('index)

        ; Populate the tree with nets
        daisyLAPopulateNetsTreeView(indexTree)
        
        ;bindingTree = daisyLAGenerateBindingTree()
        bindingTree = hiCreateTree('bindings)
        bindingTree->data = data

        ; create tree table and use indexTree as the value for ?choice
        treeField = hiCreateTreeTable(
            ?name `treeField
            ?title "Nets"
            ?titleAlignment `center
            ?headers list(
                        list("Name"    125    'left     `string)
                        list("Pin"      30    'left     `string)
                        list("Status"   60    'left     `string)
                        list("Unconn"   60    'right    `int)
            )
            ?choice indexTree
            ?callback "daisyLANetTreeCB"
        )
        
        bindingTreeField = hiCreateTreeTable(
            ?name `treeField
            ?title "Bindings"
            ?titleAlignment `center
            ?headers list(
                        list("Sch"       80    'left    `string)
                        list("Lay"       80    'left    `string)
                        list("Sch Cell"      70    'left    `string)
                        list("Lay Cell"      70    'left    `string)
                        list("Binded"    40    'right    `string)
            )
            ?sort list(0 t)
            ?sortCallback "daisyLABindingTreeSortCB"
            ?choice bindingTree
            ?callback "daisyLABindingTreeCB"
        )
        
        readInSchematicButton = hiCreateButton(
            ?name     `readInSchematicButton
            ?buttonText    "Read sch"
            ?callback "daisyLAPopulateNetsTreeView(treeField)"
        )
        
        loadFileButton = hiCreateButton(
            ?name   'loadFileButton
            ?buttonText     "Load File"
            ?callback       "printf(\"Loading file from docked form %s\n\" myForm2->fileLocation->value)")
        
        resetBindingsButton = hiCreateButton(
            ?name        `resetBindingsButton
            ?buttonText    "Reset"
            ?callback    "daisyLAResetBindings(bindingTree)"
        )
        
        openSchematicButton = hiCreateButton(
            ?name        `openSchematicButton
            ?buttonText    "Schematic"
            ?callback    "daisyLAOpenSchematic"
        )
        
        autoBindButton = hiCreateButton(
            ?name       `autoBindButton
            ?buttonText "Auto bind"
            ?callback   "daisyLAAutoBindCB(bindingTree)"
        )
        
        bindInstancesButton = hiCreateButton(
            ?name       `bindInstancesButton
            ?buttonText "Bind"
            ?callback   "daisyLABindInstances(bindingTree bindingTreeField)"
        )
        
        unbindInstancesButton = hiCreateButton(
            ?name       `unbindInstancesButton
            ?buttonText "Unbind"
            ?callback   "daisyLAUnbindInstances(bindingTree bindingTreeField)"
        )
        
        saveBindingsButton = hiCreateButton(
            ?name          `saveBindingsButton
            ?buttonText    "Save bindings"
            ?callback      "daisyLASaveBindingButtonCB(tree)"
        )
        
        loadBindingButton = hiCreateButton(
            ?name     `loadBindingButton
            ?buttonText "Load binding"
            ?callback "daisyLALoadBindingButtonCB(bindingTree)"
        )
        
        tabField = hiCreateTabField(
            ?name `tabField
            ?pageScroller nil
            ?fields list(
                    list(
                        list(readInSchematicButton 10:10 80:25 0)
                        list(treeField 10:45 430:765 15)
                    )
                    list(
                        list(autoBindButton 10:10 80:25 0)
                        list(bindInstancesButton 100:10 80:25 0)
                        list(unbindInstancesButton 190:10 80:25 0)
                        list(saveBindingsButton 280:10 100:25 0)
                        list(bindingTreeField 10:45 430:765 15)
                    )
                )
            ?tabPlacement `top
            ?tabs    list("Nets" "Bindings")
        )

        swin = hiGetSessionWindow(hiGetCurrentWindow())
        daisyLAAppForm = hiCreateAppForm(
                  ?name        gensym('myForm)
            ?formTitle    "Daisy Ratsnest which should be docked"
            ;?buttonLayout   'Close
            ?fields    list(
                        list(resetBindingsButton 10:10 100:25 0)
                        list(openSchematicButton 120:10 100:25 0)
                        list(loadBindingButton 230:10 100:25 0)
                        list(tabField 10:70 450:850 100)
                    )
            ?unmapAfterCB   t)
            
        daisyLADockForm = hiCreateDockWindow(
            ?appType    "Daisy Ratsnest"
            ?widgetType    "form"
            ?handle    'dockedForm
            ?title        "Daisy Ratsnest"
            ?dockSizeHint list(280 280)
            ?form        daisyLAAppForm
        )
        
        daisyLADock = hiDockWindow(
            ?window daisyLADockForm
            ?session swin
            ?side   'right)
            /**/
    ); let
)

procedure( daisyLASetupInterfaceItem()
    daisyLADockForm()
)

; TODO Rename properly
(procedure daisyLAMenu( tmpVar )
    let( (daisyLADisplayLayoutInstancesItem daisyLADisplaySchematicInstancesItem MenuItem2)
        daisyLASetupInterfaceItem = hiCreateAction(
            ?name 'daisyLASetupInterfaceItem
            ?iconText "daisyLASetupInterface"
            ?callback "daisyLASetupInterfaceItem()"
        )
    
        daisyLADisplayLayoutInstancesItem = hiCreateAction(
            ?name       'daisyLADisplayLayoutInstancesItem
            ?iconText   "daisyLADisplayLayoutInstances"
            ?callback   "daisyLADisplayLayoutInstances()"
        )
 
        daisyLADisplaySchematicInstancesItem = hiCreateAction(
            ?name       'daisyLADisplaySchematicInstancesItem
            ?iconText   "daisyLADisplaySchematicInstances"
            ?callback   "daisyLADisplaySchematicInstances()"
        )
  
        clearAllHighlightsItem = hiCreateAction(
            ?name       'clearAllHighlightsItem
            ?iconText   "clearAllHighlights"
            ?callback   "clearAllHighlights()"
        )
  
        daisyLACreateCorrespondenceTableItem = hiCreateAction(
               ?name    'daisyLACreateCorrespondenceTableItem
               ?iconText "daisyLACreateCorrespondenceTable"
               ?callback "daisyLACreateCorrespondenceTable()"
        )

        daisyLAHighlightdaisyLAForSelectedInstancesItem = hiCreateAction(
            ?name 'daisyLAHighlightdaisyLAForSelectedInstancesItem
            ?iconText "daisyLAHighlightdaisyLAForSelectedInstances"
            ?callback "daisyLAHighlightdaisyLAForSelectedInstances()"
        )
   
        hiCreatePulldownMenu(
            'MyPulldownMenu
            "&ScriptIC daisyLA"
            '(daisyLASetupInterfaceItem daisyLADisplayLayoutInstancesItem daisyLADisplaySchematicInstancesItem clearAllHighlightsItem daisyLACreateCorrespondenceTableItem daisyLAHighlightdaisyLAForSelectedInstancesItem )
        )
  
         hiInsertBannerMenu(
            hiGetCurrentWindow()
            MyPulldownMenu
             hiGetNumMenus( hiGetCurrentWindow() )
        )
    )
)

(procedure daisyLABindingTreeSortCB(field settings)
    println(field)
    println(settings)
)

(procedure daisyLAFindTermsInBindingsFromInstName(bindingList instName)
    (let (result)
        (foreach binding bindingList
            (cond
                (car(binding) == instName
                    println(binding)
                    (let ((tmp list()))
                        (foreach term nth(1 nth(2 binding))
                            tmp = cons(car(term) tmp)
                        )
                        result = tmp
                     )
                )
                (nth(1 binding) == instName
                    println(binding)
                    (let ((tmp list()))
                        (foreach term nth(2 nth(2 binding))
                            tmp = cons(nth(1 term) tmp)
                        )
                        result = tmp
                     )
                 )
            )
        )
        result
    )
)

(procedure daisyLAFindTermsInMatchedBindingsFromInstName(bindingList instName)
    (let (result_matched result_sch result_lay)
        (foreach binding bindingList
            (cond
                (car(binding) == instName || nth(1 binding) == instName
                    println(binding)
                    
                    ; Matched terms
                    (foreach term nth(0 nth(2 binding))
                        result_matched = cons(sprintf(nil "%s %s" car(term) nth(1 term)) result_matched)
                    )
                    
                    ; Schematic unmatched terms
                    (foreach term nth(1 nth(2 binding))
                        result_sch = cons(car(term) result_sch)
                    )
                    
                    ; Layout unmatched terms
                    (foreach term nth(2 nth(2 binding))
                        result_lay = cons(nth(1 term) result_lay)
                    )
                )
            )
        )
        list(result_matched result_sch result_lay)
    )
)

(procedure daisyLAUnbindInstances(bindingTree bindingTreeField)
    data = hiGetCurrentWindow()->data
    selItems = hiTreeTableGetSelectedItems(bindingTreeField)
    selItemsCount = hiTreeTableGetSelectedItemCount(bindingTreeField)
    
    (cond
        (selItemsCount != 1
            println("Can currently only unbind one object at a time.")
            
            nil
        )
        (t
            item = car(selItems)
            desc = hiGetTreeItemDescription(eval(item))
            
            daisyLAUnbindInstance(data desc)
        )
    )
        
    ; Force reload
    daisyLAPopulateBindingsTreeWithData(data bindingTree)
)

(procedure daisyLAGetBindingForInstance(bindings instName)
    matched = bindings->matched
    unmatched_schematic = bindings->unmatched_schematic
    unmatched_layout = bindings->unmatched_layout
    
    (let ((result list()))
        (foreach inst matched
            (cond
                (nth(0 inst) == instName
                    result = cons(inst result)
                )
            )
        )
        (if length(result) == 0 then
            (foreach inst unmatched_schematic
                (cond
                    (nth(0 inst) == instName
                        result = cons(inst result)
                    )
                )
            )
            
            (if length(result) == 0 then
                (foreach inst unmatched_schematic
                    (cond
                        (nth(0 inst) == instName
                            result = cons(inst result)
                        )
                    )
                )
            )
        )
        
        result
    )
)