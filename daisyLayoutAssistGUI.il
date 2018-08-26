

; create callback, which is called whenever any tree items are selected and which
; prints the selected item's description
(procedure treeCB(name itemSymList)
    while(itemSymList
        itemSym=car(itemSymList)
        println(hiGetTreeItemDescription(eval(itemSym)))
        itemSymList=cdr(itemSymList)
    )
)
    
(procedure daisyRNBindInstances(tree treeField)
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
                        schTerms = nth(1 daisyRNFindTermsInMatchedBindingsFromInstName(data->bindings->unmatched_schematic nth(0 schDesc)))
                        layTerms = nth(2 daisyRNFindTermsInMatchedBindingsFromInstName(data->bindings->unmatched_layout nth(1 layDesc)))
                        (if daisyRNCreateAndDisplayBindingWindow(data schDesc layDesc list() schTerms layTerms tree)
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
    
                        terms = daisyRNFindTermsInMatchedBindingsFromInstName(data->bindings->matched nth(0 desc))
                        println(terms)
                        matchedTerms = nth(0 terms)
                        schTerms = nth(1 terms)
                        layTerms = nth(2 terms)
                        (if daisyRNCreateAndDisplayBindingWindow(data desc nil matchedTerms schTerms layTerms tree)
                            println("DONE")
                        )
                    )
                )
            )
        )
    )
)

(procedure daisyRNBindingFormBindTermsCB(form)
    schTermsValue = car(form->schTermsField->value)
    layTermsValue = car(form->layTermsField->value)
    
    form->schTermsField->choices = remove(schTermsValue form->schTermsField->choices)
    form->layTermsField->choices = remove(layTermsValue form->layTermsField->choices)
    
    form->bindedTermsField->choices = append(list(sprintf(nil "%s %s" schTermsValue layTermsValue)) form->bindedTermsField->choices)
)

(procedure daisyRNBindingFormUnbindTermsCB(form)
    bindingTermsValue = car(form->bindedTermsField->value)
    terms = parseString(bindingTermsValue " ")
    schTermValue = nth(0 terms)
    layTermValue = nth(1 terms)
    
    form->bindedTermsField->choices = remove(bindingTermsValue form->bindedTermsField->choices)
    
    form->schTermsField->choices = cons(schTermValue form->schTermsField->choices)
    form->layTermsField->choices = cons(layTermValue form->layTermsField->choices)
)

(procedure daisyRNReplaceBinding(bindings newBinding)
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

(procedure daisyRNRemoveMatchedInstance(bindings instName)
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

(procedure daisyRNPopulateNetsTreeView(netTree)
    ; Add nets
    (let ((nets daisyRNGetSchematicFromLayout(hiGetCurrentWindow()->cellView)->nets))
        (foreach net nets
            println(net~>??)
            println(net->name)
            tmp = hiCreateTreeItem(gensym(net->name) list(net->name))
            println(net->instTerms~>inst~>baseName)
            hiTreeAppendItem(netTree tmp)
            tmp2 = hiCreateTree(gensym(net->name))
            hiItemInsertTree(tmp tmp2)
            instTerms = list()
            (foreach instTerm net->instTerms
                ;(if !member(instTerm->inst->baseName instTerms) then
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

(procedure daisyRNNetTreeCB(name itemSymList)
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
                 
                 layInst = daisyRNGetLayoutInstanceFromName(data data->layoutWindow->cellView instName)
                geSelectObject(layInst)
                leZoomToSelSet() ; Zoom to instance
                hiZoomRelativeScale(hiGetCurrentWindow() 0.7)
             
                daisyRNHighlightdaisyRNForSelectedInstances(instTerm nil)
                daisyRNTraceTerm(layInst instTerm nil)
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

(procedure daisyRNBindingTreeCB(name itemSymList)
    data = hiGetCurrentWindow()->data
    geDeselectAll(data->layoutWindow)
    
    (if daisyRNSchWindowInData(data)
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
        
        (if schInstName != "-" && schInstName != "" && daisyRNSchWindowInData(data) then
            schInst = daisyRNGetInstanceFromName(data->schWindow->cellView schInstName)
            hiRaiseWindow(data->schWindow)
            hiSetCurrentWindow(data->schWindow)
            geSelectObject(schInst)
            schHiZoomToSelSet()
            hiRaiseWindow(data->layoutWindow)
            hiSetCurrentWindow(data->layoutWindow)
            
            ; TODO Add highlight for schematic
            ;daisyRNHighlightdaisyRNForSelectedInstances(schTermFilter nil)
            ;daisyRNTraceTerm(schInst schTermFilter nil)
        )
        
        (if layInstName != "-" then
            layInst = daisyRNGetInstanceFromName(data->layoutWindow->cellView layInstName)
            (if !null(layInst) then
                geSelectObject(layInst)
                leZoomToSelSet(data->layoutWindow) ; Zoom to instance
                hiZoomRelativeScale(data->layoutWindow 0.7)
                
                daisyRNHighlightdaisyRNForSelectedInstances(layTermFilter t)
                daisyRNTraceTerm(layInst layTermFilter nil)
            )
        )
        
        itemSymList = cdr(itemSymList)
    )
)

(procedure daisyRNResetBindings(tree)
    println(tree~>??)
    
    data = hiGetCurrentWindow()->data
    ;data->binding = list()
    
    hiTreeRemoveAllItems(tree)
    
    (if !daisyRNSchWindowInData(data)
        daisyRNOpenSchematic()
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
    
    daisyRNPopulateBindingsTreeWithData(data tree)
)

(procedure daisyRNSaveBinding(data tree)
    println("daisyRNSaveBinding")
    
    libName = data->libName
    cellName = data->cellName
    
    packed = daisyRNPackBinding(data data->bindings)
    
    corrFile = abOpenTextCellView(libName cellName "conn2" "w") ; Change to read/write and read whatever is defined there first
    
    print(packed corrFile->port)
        
    abCloseTextCellView(corrFile)   
    
    packed
)

(procedure daisyRNSaveBindingButtonCB(tree)
    data = hiGetCurrentWindow()->data
    
    daisyRNSaveBinding(data tree)
)

(procedure daisyRNLoadBindingButtonCB(tree)
    data = hiGetCurrentWindow()->data
    
    daisyRNLoadBinding(data tree)
)

(procedure daisyRNLoadBinding(data tree)
    println("daisyRNLoadBinding")
    
    libName = data->libName
    cellName = data->cellName
    
    corrFile = abOpenTextCellView(libName cellName "conn2" "r") ; Change to read/write and read whatever is defined there first
    
    line = lineread(corrFile->port)
    
    println(line)
    
    unpacked = daisyRNUnpackBinding(data line)
        
    abCloseTextCellView(corrFile)
    
    data->bindings = unpacked
    
    daisyRNPopulateBindingsTreeWithData(data tree)    
    
    unpacked
)

(procedure daisyRNAutoBindCB(tree)
    daisyRNAutoBind()
    daisyRNPopulateBindingsTreeWithData(hiGetCurrentWindow()->data tree)
)

(procedure daisyRNDockForm()
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
        daisyRNPopulateNetsTreeView(indexTree)
        
        ;bindingTree = daisyRNGenerateBindingTree()
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
            ?callback "daisyRNNetTreeCB"
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
            ?sortCallback "daisyRNBindingTreeSortCB"
            ?choice bindingTree
            ?callback "daisyRNBindingTreeCB"
        )
        
        readInSchematicButton = hiCreateButton(
            ?name     `readInSchematicButton
            ?buttonText    "Read sch"
            ?callback "daisyRNPopulateNetsTreeView(treeField)"
        )
        
        loadFileButton = hiCreateButton(
            ?name   'loadFileButton
            ?buttonText     "Load File"
            ?callback       "printf(\"Loading file from docked form %s\n\" myForm2->fileLocation->value)")
        
        resetBindingsButton = hiCreateButton(
            ?name        `resetBindingsButton
            ?buttonText    "Reset"
            ?callback    "daisyRNResetBindings(bindingTree)"
        )
        
        openSchematicButton = hiCreateButton(
            ?name        `openSchematicButton
            ?buttonText    "Schematic"
            ?callback    "daisyRNOpenSchematic"
        )
        
        autoBindButton = hiCreateButton(
            ?name       `autoBindButton
            ?buttonText "Auto bind"
            ?callback   "daisyRNAutoBindCB(bindingTree)"
        )
        
        bindInstancesButton = hiCreateButton(
            ?name       `bindInstancesButton
            ?buttonText "Bind"
            ?callback   "daisyRNBindInstances(bindingTree bindingTreeField)"
        )
        
        unbindInstancesButton = hiCreateButton(
            ?name       `unbindInstancesButton
            ?buttonText "Unbind"
            ?callback   "daisyRNUnbindInstances(bindingTree bindingTreeField)"
        )
        
        saveBindingsButton = hiCreateButton(
            ?name          `saveBindingsButton
            ?buttonText    "Save bindings"
            ?callback      "daisyRNSaveBindingButtonCB(tree)"
        )
        
        loadBindingButton = hiCreateButton(
            ?name     `loadBindingButton
            ?buttonText "Load binding"
            ?callback "daisyRNLoadBindingButtonCB(bindingTree)"
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
        daisyRNAppForm = hiCreateAppForm(
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
            
        daisyRNDockForm = hiCreateDockWindow(
            ?appType    "Daisy Ratsnest"
            ?widgetType    "form"
            ?handle    'dockedForm
            ?title        "Daisy Ratsnest"
            ?dockSizeHint list(280 280)
            ?form        daisyRNAppForm
        )
        
        daisyRNDock = hiDockWindow(
            ?window daisyRNDockForm
            ?session swin
            ?side   'right)
            /**/
    ); let
)

procedure( daisyRNSetupInterfaceItem()
    daisyRNDockForm()
)

(procedure MyMenu( tmpVar )
    let( (daisyRNDisplayLayoutInstancesItem daisyRNDisplaySchematicInstancesItem MenuItem2)
        daisyRNSetupInterfaceItem = hiCreateAction(
            ?name 'daisyRNSetupInterfaceItem
            ?iconText "daisyRNSetupInterface"
            ?callback "daisyRNSetupInterfaceItem()"
        )
    
        daisyRNDisplayLayoutInstancesItem = hiCreateAction(
            ?name       'daisyRNDisplayLayoutInstancesItem
            ?iconText   "daisyRNDisplayLayoutInstances"
            ?callback   "daisyRNDisplayLayoutInstances()"
        )
 
        daisyRNDisplaySchematicInstancesItem = hiCreateAction(
            ?name       'daisyRNDisplaySchematicInstancesItem
            ?iconText   "daisyRNDisplaySchematicInstances"
            ?callback   "daisyRNDisplaySchematicInstances()"
        )
  
        clearAllHighlightsItem = hiCreateAction(
            ?name       'clearAllHighlightsItem
            ?iconText   "clearAllHighlights"
            ?callback   "clearAllHighlights()"
        )
  
        daisyRNCreateCorrespondenceTableItem = hiCreateAction(
               ?name    'daisyRNCreateCorrespondenceTableItem
               ?iconText "daisyRNCreateCorrespondenceTable"
               ?callback "daisyRNCreateCorrespondenceTable()"
        )

        daisyRNHighlightdaisyRNForSelectedInstancesItem = hiCreateAction(
            ?name 'daisyRNHighlightdaisyRNForSelectedInstancesItem
            ?iconText "daisyRNHighlightdaisyRNForSelectedInstances"
            ?callback "daisyRNHighlightdaisyRNForSelectedInstances()"
        )
   
        hiCreatePulldownMenu(
            'MyPulldownMenu
            "&ScriptIC daisyRN"
            '(daisyRNSetupInterfaceItem daisyRNDisplayLayoutInstancesItem daisyRNDisplaySchematicInstancesItem clearAllHighlightsItem daisyRNCreateCorrespondenceTableItem daisyRNHighlightdaisyRNForSelectedInstancesItem )
        )
  
         hiInsertBannerMenu(
            hiGetCurrentWindow()
            MyPulldownMenu
             hiGetNumMenus( hiGetCurrentWindow() )
        )
    )
)

(procedure daisyRNBindingTreeSortCB(field settings)
    println(field)
    println(settings)
)

(procedure daisyRNFindTermsInBindingsFromInstName(bindingList instName)
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

(procedure daisyRNFindTermsInMatchedBindingsFromInstName(bindingList instName)
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

(procedure daisyRNUnbindInstances(bindingTree bindingTreeField)
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
            
            daisyRNUnbindInstance(data desc)
        )
    )
        
    ; Force reload
    daisyRNPopulateBindingsTreeWithData(data bindingTree)
)

(procedure daisyRNGetBindingForInstance(bindings instName)
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