; Created in March 2018 by Martin Nielsen-Lönn (c)
;
; Creates a GUI for connecting schematic and layout instances in
Cadence Virtuoso.
; 1. Load the file
; 2. In the layout, load the GUI
; 3. Add connections between the schematic and layout instances
; 4. Select an instance to see the traces in layout and schematic
; 
; TODO Propagate from pin if selecting net
; TODO Update - check for deleted and new instances
; TODO Trace net in schematic
; TODO Status symbol for saving
; TODO Alert box for auto bind
; TODO REFACTORING REQUIRED!



; create callback, which is called whenever any tree items are selected and which
; prints the selected item's description
(procedure treeCB(name itemSymList)
    while(itemSymList
        itemSym=car(itemSymList)
        println(hiGetTreeItemDescription(eval(itemSym)))
        itemSymList=cdr(itemSymList)
    )
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

(procedure daisyRNUnbindInstance(data desc)
    ; Get schematic and layout parts
    (let (
            (instBindings car(daisyRNGetBindingForInstance(data->bindings car(desc))))
            (schTerms list())
            (layTerms list())
            (schBinding list())
            (layBinding list())
         )
        println(instBindings)

        (foreach term append(car(nth(2 instBindings)) nth(1 nth(2 instBindings)))
            schTerms = cons(list(car(term) "-") schTerms)
        )

        (foreach term append(car(nth(2 instBindings)) nth(2 nth(2 instBindings)))
            layTerms = cons(list("-" nth(1 term)) layTerms)
        )
        
        ; Split binding
        schBinding = list(car(instBindings) "-" list(list() schTerms list()))
        layBinding = list("-" nth(1 instBindings) list(list() list() layTerms))
    
        ; Remove from matched instances
        data->bindings->matched = daisyRNRemoveMatchedInstance(data->bindings car(desc))
        
        data->bindings->unmatched_schematic = cons(schBinding data->bindings->unmatched_schematic)
        data->bindings->unmatched_layout= cons(layBinding data->bindings->unmatched_layout)
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

(procedure daisyRNCreateAndDisplayBindingWindow(data schDesc layDesc matchedTerms schTerms layTerms tree)    
    ; Create fields
    schTermsLabel = hiCreateLabel(
        ?name    `schTermsLabel
        ?labelText "Schematic terms"
    )
    schTermsField = hiCreateListBoxField(
        ?name `schTermsField
        ?choices schTerms
    )
    
    layTermsLabel = hiCreateLabel(
        ?name    `layTermsLabel
        ?labelText "Layout terms"
    )
    layTermsField = hiCreateListBoxField(
        ?name `layTermsField
        ?choices layTerms
    )
    
    bindedTermsLabel = hiCreateLabel(
        ?name    `bindedTermsLabel
        ?labelText "Binded terms"
    )
    bindedTermsField = hiCreateListBoxField(
        ?name `bindedTermsField
        ?choices matchedTerms
    )
    
    bindButton = hiCreateButton(
        ?name `bindTermsButton
        ?buttonText "Bind terms"
        ?callback "daisyRNBindingFormBindTermsCB(form)"
    )
    
    unbindButton = hiCreateButton(
        ?name `unbindTermsButton
        ?buttonText "Unbind terms"
        ?callback "daisyRNBindingFormUnbindTermsCB(form)"
    )
    
    ; hiCreateAppForm
    form = hiCreateAppForm(
        ?name gensym("daisyRNBindForm")
        ?formTitle "Select bindings for instance"
        ?callback "daisyRNBindingFormCB(data schDesc layDesc tree form)"
        ?fields list(
                    bindButton
                    unbindButton
                    schTermsLabel
                    schTermsField
                    layTermsLabel
                    layTermsField
                    bindedTermsLabel
                    bindedTermsField)
        ?help "daisyRN"
    )

    ; hiDisplayForm or hiOpenWindow
    status = hiDisplayForm( form )
    (cond
        (status
            println("Clicked OK")
        )
        (t
            println("What happened?")
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

(procedure daisyRNBindingFormCB(data schDesc layDesc tree form)
    println(form~>??)
    bindingTermsValues = form->bindedTermsField->choices

    ; TODO Rename
    (if null(layDesc) then ; Editing existing binding
        test = list(car(schDesc) nth(1 schDesc))
    else
        test = list(car(schDesc) nth(1 layDesc))
    )
    println(test)
    
    (let ((terms list()))
        (foreach pair bindingTermsValues
            tupel = parseString(pair " ")
            schTermValue = nth(0 tupel)
            layTermValue = nth(1 tupel)
            
            terms = cons(list(schTermValue layTermValue) terms)
        )
        (let ((termsSch list()) (termsLay list()))            
            (foreach termSch form->schTermsField->choices
                termsSch = cons(list(termSch "-") termsSch)
            )
            
            (foreach termLay form->layTermsField->choices
                termsLay = cons(list("-" termLay) termsLay)
            )
            
            terms = append(list(terms) append(list(termsSch) list(termsLay)))
        )
        test = append(test list(terms))
    )
    
    (if !null(layDesc) then
        data->bindings->matched = cons(test data->bindings->matched)
        
        data->bindings->unmatched_schematic = daisyRNRemoveUnmatchedInstance(data->bindings->unmatched_schematic test)
        data->bindings->unmatched_layout = daisyRNRemoveUnmatchedInstance(data->bindings->unmatched_layout test)
    else
        data->bindings->matched = daisyRNReplaceBinding(data->bindings->matched test)
    )
        
    println("IN CALLBACK")
    
    daisyRNPopulateBindingsTreeWithData(data tree)
    
    ; Save data
    ;daisyRNSaveBinding(data tree)
    ; Load bindings
    ;daisyRNLoadBinding(data tree)
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

(procedure daisyRNRemoveUnmatchedInstance(unmatched instName)
    println(unmatched)
    (let ((stillUnmatched list()))
        (foreach unmatch unmatched
            (cond
                (car(unmatch) == car(instName)
                    ; Schematic
                    ; Skip
                    t
                )
                (nth(1 unmatch) == nth(1 instName)
                    ; Schematic
                    ; Skip
                    t
                )
                (t
                    stillUnmatched = cons(unmatch stillUnmatched)
                )
            )
        )
        stillUnmatched
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

(procedure daisyRNSchWindowInData(data)
    (cond
        (null(hiGetCurrentWindow()->data->schWindow)
            println("No schematic window")
            nil
        )
        (!member(hiGetCurrentWindow()->data->schWindow hiGetWindowList())
            println("No schematic window")
            nil
        )
        (t
            t
        )
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

(procedure daisyRNAddBindingFromInstance(bindings inst)
    println("daisyRNAddBindingFromInstance")
    println(inst)
    
    ; Get terms
    ; Construct list
    ; Append list
    
    data = hiGetCurrentWindow()->data
    
    libName = data->libName
    cellName = data->cellName
    
    ; Perform auto bind
    (let (schematic layout)
        schematic = dbOpenCellViewByType(libName cellName "schematic")
        layout = dbOpenCellViewByType(libName cellName "layout")
        (setq schematic_instances makeTable("schematicInstances"))
        (setq layout_instances makeTable("layoutInstances"))
        ;println(schematic->instances~>??)
        
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
        
        println(schematic_instances~>??)
        
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
        
        println(layout_instances~>??)
        
        ; Check corresponding schematic instances based on the schematic instances
        printf("Starting to match instances from layout to schematic\n")
        (let ((matched list()) (unmatched_schematic list()) (unmatched_layout list()) (terms list()))
            (foreach inst layout_instances
                terms = list()
                (if (listp(schematic_instances[inst]) && sort(copy(schematic_instances[inst]) nil) == sort(copy(layout_instances[inst]) nil)) then
                    ; Matching names and terms
                    printf(strcat("Instance " inst " is matched.\n"))
                    
                    ; Since match we add them to the matched list
                    (foreach term sort(copy(schematic_instances[inst]) nil)
                        terms = append(list(list(term term)) terms)
                    )
                    matched = append(list(list(inst inst list(terms list() list()))) matched)
                else
                    printf(strcat("Instance " inst " is NOT matched\n"))
                        
                    ; Since unmatch we add them to the unmatched list
                    
                    ; Since match we add them to the matched list
                    (foreach term sort(copy(layout_instances[inst]) nil)
                        terms = append(list(list("" term)) terms)
                    )
                    unmatched_layout = append(list(list("" inst list(list() list() terms))) unmatched_layout) 
                )
            )            
            
            (let ((terms list()))
                ; Check for matching
                (foreach inst schematic_instances
                    terms = list()
                    (cond
                        (daisyRNInstInTable(inst matched)
                            printf("schematic instance matched - %s\n" inst)
                        )
                        (t
                            printf("schematic instance UNMATCHED - %s\n" inst)
                            printf("Appending to schematic_unmatched\n")
                            
                            (foreach term sort(copy(schematic_instances[inst]) nil)
                                terms = append(list(list(term "")) terms)
                            )
                            unmatched_schematic = append(list(list(inst "" list(list() terms list()))) unmatched_schematic)
                        )
                    )
                )
            )
            
            (if null(unmatched_layout) then
                printf("All layout instances matched!\n")
            )
            
            pack = ncons(nil)
            pack->matched  = matched
            pack->unmatched_schematic = unmatched_schematic
            pack->unmatched_layout = unmatched_layout
            
            println(pack)
            
            println(daisyRNPackBinding(data pack))
            
            
            ;print(list(list("schematic" schematic_instances~>??) list("layout" layout_instances~>??) list("matched" matched) list("unmatched" unmatched_layout)) corrFile->port)
            
        )
    )
)

(procedure daisyRNSaveBindingButtonCB(tree)
    data = hiGetCurrentWindow()->data
    
    daisyRNSaveBinding(data tree)
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

(procedure daisyRNPopulateBindingsTreeWithData(data tree)
    hiTreeRemoveAllItems(tree)

    ; Add all schematic items
    ; Add all layout items
    
    ; Check if binding exists
    ; If exists - reload
    ; If it does not populate with schematic and layout
    
    (if !daisyRNSchWindowInData(data)
        daisyRNOpenSchematic()
    )
    
    ; Add matched objects first
    ; Add unmatched schematic
    ; Add unmatched layout
    
    (foreach instBind data->bindings->matched
        schInst = daisyRNGetInstanceFromName(data->schWindow->cellView car(instBind))
        layInst = daisyRNGetInstanceFromName(data->layoutWindow->cellView nth(1 instBind))
        tmp = hiCreateTreeItem(gensym(schInst->name) list(schInst->name layInst->name schInst->cellName layInst->cellName "OK"))
        hiTreeAppendItem(tree tmp)
        ; Add both terminals
        tmp2 = hiCreateTree(gensym(schInst->name))
        hiItemInsertTree(tmp tmp2)
        (foreach terms nth(0 nth(2 instBind)) ; Matched
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list(car(terms) car(cdr(terms)) "" "" "")))
        )
        (foreach terms nth(1 nth(2 instBind)) ; Schematic
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list(car(terms) "-" "" "" "")))
        )
        (foreach terms nth(2 nth(2 instBind)) ; Layout
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list("-" car(cdr(terms)) "" "" "")))
        )
    )
    
    (foreach instBind data->bindings->unmatched_schematic
        schInst = daisyRNGetInstanceFromName(data->schWindow->cellView car(instBind))
        tmp = hiCreateTreeItem(gensym(schInst->name) list(schInst->name "-" schInst->cellName "-" "NO"))
        hiTreeAppendItem(tree tmp)
        ; Add both terminals
        tmp2 = hiCreateTree(gensym(schInst->name))
        hiItemInsertTree(tmp tmp2)
        (foreach terms nth(1 nth(2 instBind))
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list(car(terms) "-" "" "" "")))
        )
    )
    
    (foreach instBind data->bindings->unmatched_layout
        layInst = daisyRNGetInstanceFromName(data->layoutWindow->cellView car(cdr(instBind)))
        tmp = hiCreateTreeItem(gensym(layInst->name) list("-" layInst->name "-" layInst->cellName "NO"))
        hiTreeAppendItem(tree tmp)
        ; Add both terminals
        tmp2 = hiCreateTree(gensym(layInst->name))
        hiItemInsertTree(tmp tmp2)
        (foreach terms nth(2 nth(2 instBind))
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list("-" car(cdr(terms)) "" "" "")))
        )
    )
    
    /*(let ((instsSch data->schWindow->cellView->instances)
        (instsLay data->layoutWindow->cellView->instances))
        (foreach inst instsSch
            tmp = hiCreateTreeItem(gensym(inst->name) list(inst->name "" inst->cellName "NO"))
            hiTreeAppendItem(tree tmp)
        )
        (foreach inst instsLay            
            tmp = hiCreateTreeItem(gensym(inst->name) list("" inst->name inst->cellName "NO"))
            hiTreeAppendItem(tree tmp)
        )
    )*/
)

(procedure daisyRNPackBinding(data unpacked)
    (let ((result list(list("matched" unpacked->matched) list("unmatched" list("schematic" unpacked->unmatched_schematic) list("layout" unpacked->unmatched_layout)))))
        result
    )
)

(procedure daisyRNUnpackBinding(data pack)
    ; Unpack pack to a struct of the correct DPL
    
    (let (result)
        result = ncons(nil)
        (foreach subList car(pack)
            (case car(subList)
                ("matched"
                    result->matched = car(cdr(subList))
                )
                ("unmatched"
                    (if car(car(cdr(subList))) == "schematic" then
                        result->unmatched_schematic = car(cdr(car(cdr(subList))))
                        result->unmatched_layout = car(cdr(car(cdr(cdr(subList)))))
                    else
                        result->unmatched_schematic = car(cdr(car(cdr(cdr(subList)))))
                        result->unmatched_layout = car(cdr(car(cdr(subList))))
                    )
                )
            )
        )
        
        result
    )
)

(procedure daisyRNAutoBindCB(tree)
    daisyRNAutoBind()
    daisyRNPopulateBindingsTreeWithData(hiGetCurrentWindow()->data tree)
)

(procedure daisyRNAutoBind()
    println("daisyRNAutoBind")
    
    ; Ask for confirmation about rewrite?
    
    ; Loop through unmatched layout instances to find matching schematic instances
    
    data = hiGetCurrentWindow()->data
    
    libName = data->libName
    cellName = data->cellName
    
    ; Perform auto bind
    (let (schematic layout)
        ; Check corresponding schematic instances based on the schematic instances
        printf("Starting to match instances from layout to schematic\n")
        (let (matching (matchedTerms list()) (unmatched_schematic list()) (unmatched_layout list())) ; unmatched_layout is not used!
            (foreach layBinding data->bindings->unmatched_layout
                matchedTerms = list()
                ;println(layBinding)
                (foreach schBinding data->bindings->unmatched_schematic
                    ; Check if bindings might match
                    (if car(schBinding) == nth(1 layBinding) then
                        ; Instance name matches
                        matching = t
                        (foreach layTerm nth(2 nth(2 layBinding))
                            ; Check each layout term
                            (let ((matched_term nil))
                                (foreach schTerm nth(1 nth(2 schBinding))
                                    (if car(schTerm) == nth(1 layTerm) then
                                        matched_term = t
                                        matchedTerms = cons(list(car(schTerm) nth(1 layTerm)) matchedTerms)
                                    )
                                )
                                
                                (if !matched_term
                                    matching = nil
                                )
                            )
                        )
                    else
                        matching = nil
                    )
                    
                    (if matching then 
                        ; Add unmatched schematic pins
                        unmatched_schematic = list()
                        (foreach schTerm nth(1 nth(2 schBinding))
                            ;println(schTerm)
                            (let ((schTermMatched nil))
                                (foreach layTerm nth(2 nth(2 layBinding))
                                    (if car(schTerm) == nth(1 layTerm) then
                                        schTermMatched = t
                                    )
                                )
                                
                                (if !schTermMatched then
                                    unmatched_schematic = cons(schTerm unmatched_schematic)
                                )
                            )
                        )
                        
                        println("Add to matched instances and remove from unmatched schematic and layout.")
                        
                        data->bindings->matched = cons(list(car(schBinding) nth(1 layBinding) list(matchedTerms unmatched_schematic list())) data->bindings->matched)
        
                        data->bindings->unmatched_schematic = daisyRNRemoveUnmatchedInstance(data->bindings->unmatched_schematic list(car(schBinding) nth(1 layBinding)))
                        data->bindings->unmatched_layout = daisyRNRemoveUnmatchedInstance(data->bindings->unmatched_layout list(car(schBinding) nth(1 layBinding)))
                    else
                        println("No match")
                    )
                )
            )
        )
    )
)

(procedure daisyRNInstInTable(instName table)
    (let ((result nil))
        (foreach entry table
            (if car(entry) == instName
                result = t
            )
        )
        result
    )
)

/*(procedure daisyRNGenerateBindingTree()
    ; Add bindings
    (let ((insts daisyRNGetSchematicInstances(hiGetCurrentWindow()->cellView)))
        (foreach inst insts
            println(inst~>??)
            /*println(net~>??)
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
            )*
        )
    )
)*/

procedure( daisyRNOpenSchematic()
    data = hiGetCurrentWindow()->data
    (if !daisyRNSchWindowInData(data) then
        data->schWindow = geOpen(
            ?lib geGetEditCellView()~>libName
            ?cell geGetEditCellView()~>cellName
            ?view "schematic"
            ?mode "r")
        data->schWindow->data = data
        ;hiResizeWindow(data->schWindow list(1200:248 1410:1175))
        hiRaiseWindow(data->layoutWindow)
     else
         hiRaiseWindow(data->schWindow)
     )
)

(procedure daisyRNBindingTreeSortCB(field settings)
    println(field)
    println(settings)
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
 
(procedure daisyTransformBBox(bBox transform)
    (let ((xy car(transform)) (R nth(1 transform)) (intbBox bBox))
        /*(setq startX (car(lowerLeft(intbBox)) + car(xy)))
        (setq startY (cadr(lowerLeft(intbBox)) + cadr(xy)))
        (setq endX (car(upperRight(intbBox)) + car(xy)))
        (setq endY (cadr(upperRight(intbBox)) + cadr(xy)))*/
        (setq startX car(lowerLeft(intbBox)))
        (setq startY cadr(lowerLeft(intbBox)))
        (setq endX   car(upperRight(intbBox)))
        (setq endY   cadr(upperRight(intbBox)))
        
        (case R
            ("R0"
                intbBox = list(list(startX startY) list(endX endY))
            )
            ("R90"
                intbBox = list(list(-startY endX) list(-endY startX))
            )
            ("R180"
                intbBox = list(list(-endX -endY) list(-startX -startY))
            )
            ("R270"
                intbBox = list(list(startY -endX) list(endY -startX))
            )
            ("MY"
                intbBox = list(list(-endX startY) list(-startX endY))
            )
            ("MYR90"
                intbBox = list(list(-startY -endX) list(-endY -startX))
            )
            ("MX"
                intbBox = list(list(startX -startY) list(endX -endY))
            )
            ("MXR90"
                intbBox = list(list(startY endX) list(endY startX))
            )
        )
        
        ;println(transform)
        ;println(bBox)
        ;println(intbBox)
        
        (if !null(xy)
            intbBox = daisyMoveBBoxOrigin(intbBox xy)
        )
        
        intbBox
    )
)
    
; Moves the bounding box (bBox) so the lower left corner is at xy
(procedure daisyMoveBBoxOrigin(bBox xy)
    (setq startX (car(lowerLeft(bBox)) + car(xy)))
    (setq startY (cadr(lowerLeft(bBox))  + cadr(xy)))
    (setq start list(startX startY))
    (setq endX (car(upperRight(bBox)) + car(xy)))
    (setq endY (cadr(upperRight(bBox))  + cadr(xy)))
    (setq end list(endX endY))
    
    list(start end)
)
 
(procedure daisyRNDisplayLayoutInstances()
    ; Clear all markers first
    geHiDeleteAllMarker()
    ;println( "You have clicked daisyRNDisplayLayoutInstances" )
    let( (layoutWindow layout)
           layoutWindow = hiGetCurrentWindow()
           ;println(layoutWindow~>??)
           layout = layoutWindow->cellView
           ;println(layout~>??)
        (setq hiliteset geCreateHilightSet(layout list("hilite" "drawing") t))
        hiliteset~>enable = t
        gePushHilightStack(hiliteset)
           foreach(inst layout->instances
               ;println(inst->name)
               ;println(inst->cellName)
               ;println(inst->xy)
               ;(setq instLocation)
               foreach(terminal inst->master->terminals
                   ;println(terminal->name)
                   ;println((car (car terminal->net->pins)->figs)->bBox)
                   (setq bBox (car (car terminal->net->pins)->figs)->bBox)
                   geCreateMarkerByBBox(layout "warning" "daisy" "pin" terminal->name daisyTransformBBox(bBox inst->transform))
                   geAddHilightLine(hiliteset list(list(0 0) centerBox(daisyTransformBBox(bBox inst->transform))))
            )
        )
    )
)

procedure( daisyRNHighlightSchematicNet(schematic net)
       ;geHiDeleteAllMarker()
    (setq hiliteset geCreateHilightSet(schematic list("hilite" "drawing") nil))
    hiliteset~>enable = t
    ;gePushHilightStack(hiliteset)
    ;println(strcat("_" net->name "_"))
    
    foreach(fig net->figs
        ;println(strcat("   " fig~>??))
        ;println(fig~>??)
        ;println(strcat("objType: " fig->objType))
        case( fig->objType
            ("line"
                geAddHilightLine(hiliteset fig->bBox)
                )
            (t
                println(strcat("objType: " fig->objType " not recognized."))
                )
            ) ; case
    )
)

procedure( daisyRNGetSchematicFromLayout(layout)
    (let (libName cellName)
        libName = layout->cellView->libName
        cellName = layout->cellView->cellName
        
        dbOpenCellViewByType(libName cellName "schematic")
    )
)

procedure( daisyRNGetInstanceFromName(cellView instName)
    ;printf("daisyRNGetInstanceFromName\n")
    (let (returnInstance)
        (foreach inst cellView->instances
            ;println(inst~>??)
            (if inst->name == instName then
                ;println(inst~>??)
                returnInstance = inst
            )
        )
        returnInstance
    )
)

(procedure daisyRNGetSchematicInstanceFromLayoutName(data layInstName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (if nth(1 binding) == layInstName
                result = daisyRNGetInstanceFromName(data->schWindow->cellView nth(0 binding))
            )
        )
        result
    )
)

(procedure daisyRNGetLayoutInstanceFromName(data cellView schInstName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (if nth(0 binding) == schInstName
                result = daisyRNGetInstanceFromName(cellView nth(1 binding))
            )
        )
        result
    )
)

procedure( daisyRNGetInstanceTermFromName(inst termName)
    ;printf("daisyRNGetInstanceTermFromName\n")
    ; Check if layout or schematic?
    ;println(inst~>??)
    ;println(termName)
    (let (returnTerm)
        ;println(inst->master~>??)
        ;(foreach term inst->instTerms
        (foreach term inst->master->terminals
            ;printf("Search name %s current name %s\n" termName term->name)
            (if term->name == termName then
                ;println(term~>??)
                returnTerm = term
            )
        )
        returnTerm
    )
)

(procedure daisyRNGetLayoutInstanceTermFromName(data layInst ischInstName schTermName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (foreach term nth(0 nth(2 binding))
                (if car(term) == schTermName
                    result = daisyRNGetTermPinInMetalOrPoly(daisyRNGetInstanceTermFromName(layInst nth(1 term)))
                )
            )
        )
        result
    )
)

(procedure daisyRNGetViewBBoxForTerminal(layout termFig inst)
       ;println(termFig)
       (setq bBox termFig->bBox)

    bBox
)

procedure( daisyRNbBoxToPoints(bBox)
    ;let( ((points list()))
    ;    points = append(points lowerLeft(bBox)
    ;)
    bBox
    ; FIX
)

; TODO Add better check for valid layers
(procedure daisyRNGetTermPinInMetalOrPoly(term)
    ;printf("daisyRNGetTermPinInMetal\n")
    ;println(term~>??)
    ;println(term~>pins~>figs~>??)
    (let ((metalFigs list()))
        (foreach pin term->pins
            (foreach fig pin->figs
                ;println(fig~>??)
                (setq metalLayerNames list("Poly" "Metal1" "Metal2" "Metal2" "Metal3" "Metal4" "Metal5" "Metal6" "Metal7"))
                ;println(fig->layerName)
                (if member(fig->layerName metalLayerNames) then
                    ;println("Found metal or poly")
                    ;println(fig)
                    metalFigs = append(metalFigs list(fig))
                )
            )
        )
        metalFigs
    )
)

(procedure daisyRNTraceTerm(instance termName @optional (clear t))
    println("daisyRNTraceTerm")
    ;println(instance~>??)
    ;println(termName)
    
    ; Check so we are in layout
    (if clear
        clearAllHighlights()
    )
    ; Only Metals and poly!!!
    
    (if instance->objType == "inst" then
        ;println(instance~>??)
        ;println(daisyRNGetInstanceTermFromName(instance termName)~>??)
        
        hiliteSet = geCreateHilightSet(hiGetCurrentWindow()->cellView list("hilite" "drawing2") nil)
        hiliteSet->enable = t
        
        (let ((term daisyRNGetInstanceTermFromName(instance termName)) (bBoxes list()))
            exploredInstances = list(instance)
            
            ;println(term)
            (foreach fig daisyRNGetTermPinInMetalOrPoly(term)
                bBox = daisyRNGetViewBBoxForTerminal(hiGetCurrentWindow()->cellView fig instance)
                
                geAddHilightRectangle(hiliteSet daisyTransformBBox(bBox instance->transform))
                daisyRNTraceNetInst(hiGetCurrentWindow()->cellView fig->layerName daisyTransformBBox(bBox instance->transform) instance fig exploredInstances hiliteSet 20)
            )
        )
    else
        printf("Please select a instance and not a %s.\n" instance->objType)
    )
    "DONE"
)

(procedure daisyRNTraceNet()
    printf("daisyRNTraceNet\n")
    instance = car(geGetSelSet()) ; Limited to one object for now
    daisyRNTraceTerm(instance "S")
)

; TODO Should it remember parentInst or term? Or bBox? Or?
(procedure daisyRNTraceNetInst(cellView layerName bBox parentInst parentFig exploredInstances hiliteSet depth)
    println("daisyRNTraceNetInst")
    ;printf("explored0: %d\n" length(exploredInstances))
    
    (cond
        (depth == 0
            println("Reached depth") ; Should be printed! TODO
            exploredInstances
        )
        (t
            ; Get overlaps
                ;printf("exploredpre: %d\n" length(exploredInstances))
            overlaps = dbGetOverlaps(cellView bBox)
                ;printf("exploredpost: %d\n" length(exploredInstances))
            ;println(overlaps~>??)
            
            ; Loop through overlaps
            (foreach overlap overlaps
                ;printf("explored1: %d\n" length(exploredInstances))
                ;println("OVERLAP:")
                ;println(overlap~>??)
                ;println("Already a member?")
                ;println(member(overlap exploredInstances))
                ; Check if parentInst is polygon, if it is then doublecheck overlap with special function
                (if parentInst->objType != "polygon" || daisyRNIsIntersecting(overlap parentFig layerName) then
                    (cond
                        (!listp(exploredInstances)
                            println("exploredInstances is not a list!!!")
                            nil
                        )
                        (member(overlap exploredInstances)
                            ;println("Already explored.")
                            nil
                        )
                        (t
                            ;println(overlap->objType)
                            ;printf("explored2: %d\n" length(exploredInstances))
                            (case overlap->objType
                                ("path" || "rect" || "pathSeg"
                                    ;printf("explored3: %d\n" length(exploredInstances))
                                    ;println("path or rect")
                                    (if overlap->layerName == layerName then
                                        exploredInstances = append(list(overlap) exploredInstances)
                                        ;println(overlap~>??)
                                        geAddHilightRectangle(hiliteSet overlap->bBox)
                                        exploredInstances = daisyRNTraceNetInst(cellView layerName overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
                                    )
                                )
                                ("stdVia"
                                    ; TODO Get specific bBox for that metal
                                    ;println(overlap->viaHeader->viaDef~>??)
                                    ;printf("explored4: %d\n" length(exploredInstances))
                                    (cond
                                        (overlap->viaHeader->viaDef->layer1->name == layerName
                                            exploredInstances = append(list(overlap) exploredInstances)
                                            ;println(overlap~>??)
                                            geAddHilightRectangle(hiliteSet overlap->bBox)
                                            exploredInstances = daisyRNTraceNetInst(cellView overlap->viaHeader->viaDef->layer2->name overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
                                        )
                                        (overlap->viaHeader->viaDef->layer2->name == layerName
                                            exploredInstances = append(list(overlap) exploredInstances)
                                            ;println(overlap~>??)
                                            geAddHilightRectangle(hiliteSet overlap->bBox)
                                            exploredInstances = daisyRNTraceNetInst(cellView overlap->viaHeader->viaDef->layer1->name overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
                                        )
                                        (t
                                            ; Not the correct via, ignore if for now since it might be used later
                                            println("Lost via")
                                            ;exploredInstances = append(list(overlap) exploredInstances)
                                        )
                                    )
                                )
                                ("polygon"
                                    printf("explored5: %d\n" length(exploredInstances))
                                    ; If polygon do a reverse TrueOverlap to adjust for the crooked bBox
                                    ;println("Polygon!")
                                    ;println(overlap~>??)
                                    ;println(parentInst~>??)
                                    ;println(layerName)
                                    ;println("Is intersecting????")
                                    (if overlap->layerName == layerName && daisyRNIsIntersecting(overlap parentInst layerName) then
                                        ;println("Is intersecting")
                                        exploredInstances = append(list(overlap) exploredInstances)
                                        ;println(overlap~>??)
                                        geAddHilightPolygon(hiliteSet overlap->points)
                                        exploredInstances = daisyRNTraceNetInst(cellView layerName overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
                                    )
                                )
                                ("inst"
                                    ; Find correct pins and propagate from there
                                    ;println("Instance found.")
                                    ;println(overlap~>??)
                                    ; Find terminals
                                    (let ((isOverlapping nil))
                                        (foreach term overlap->master->terminals
                                            ;println(term~>??)
                                            metalFigs = daisyRNGetTermPinInMetalOrPoly(term)
                                            (foreach fig metalFigs
                                                ;println(fig~>??)
                                                (if fig->layerName == layerName then
                                                    (if daisyRNIsIntersecting(fig parentFig layerName overlap->xy) then
                                                        ;println("It is intersecting!")
                                                        (if !isOverlapping then
                                                            isOverlapping = t
                                                            exploredInstances = append(list(overlap) exploredInstances)
                                                            ;println("exploredInstances")
                                                            ;println(exploredInstances~>??)
                                                        )
                                                        ;println(fig~>??)
                                                        geAddHilightRectangle(hiliteSet daisyTransformBBox(fig->bBox overlap->transform))
                                                        exploredInstances = daisyRNTraceNetInst(cellView layerName daisyTransformBBox(fig->bBox overlap->transform) overlap fig exploredInstances hiliteSet depth-1)
                                                        ;exploredInstances
                                                    )
                                                )
                                            )
                                        )
                                    )
                                    ; Compare bounding boxes
                                    ; Check layer
                                    ; Continue!
                                    exploredInstances = append(list(overlap) exploredInstances) ; Dummy for now
                                )
                            )
                            ;geAddHilightRectangle(hiliteSet overlap->fig->bBox)
                        )
                    )
                )
            )
            exploredInstances
            ;geAddHilightRectangle(hiliteSet bBox)
        )
    )    
)

procedure( daisyRNIsBoundingBoxesOverlap(bBox1, bBox2)
    
    (cond
        (car(upperRight(bBox1)) < car(lowerLeft(bBox2))
            nil
        )
        (car(lowerLeft(bBox1)) > car(upperRight(bBox2))
            nil
        )
        (car(cdr(upperRight(bBox1))) < car(cdr(lowerLeft(bBox2)))
            nil
        )
        (car(cdr(lowerLeft(bBox1))) > car(cdr(upperRight(bBox2)))
            nil
        )
        (t
            t
        )
    )
)

procedure( daisyRNMovePointsXY(points xy)
    (cond
        (null(points)
            list()
        )
        (length(points) == 0
            list()
        )
        (listp(points) && listp(car(points))
            ; List in list
            cons(daisyRNMovePointsXY(car(points) xy) daisyRNMovePointsXY(cdr(points) xy))
        )
        (listp(points)
            daisyRNMovePointXY(points xy)
        )
        (t
            println("It should never reach this statement in daisyRNMovePointsXY.")
        )
    )
)

procedure( daisyRNMovePointXY(point xy)
    list((car(point) + car(xy)) (car(cdr(point)) + car(cdr(xy))))
)

(procedure daisyRNIsIntersecting(inst1 inst2 layerName @optional (inst1_xy_offset list(0 0)) (inst2_xy_offset list(0 0)))
    ;println("daisyRNIsIntersecting")
    
    ; Bounding box overlap?
    (cond
        ; lowerLeft 
        (daisyRNIsBoundingBoxesOverlap(daisyTransformBBox(inst1->bBox inst1->transform) daisyMoveBBoxOrigin(inst2->bBox inst2_xy_offset))
            ;println("Bounding boxes overlap.")
            ; Check more carefully
            ;println(list(daisyRNGetPointsFromInstance(inst1)))
            ;println(list(daisyRNGetPointsFromInstance(inst2)))
            ;println(dbPointArrayAnd(inst1->cellView list(daisyRNGetPointsFromInstance(inst1)) list(daisyRNGetPointsFromInstance(inst2))))
            (let ((points1 daisyRNMovePointsXY(list(daisyRNGetPointsFromInstance(inst1)) inst1_xy_offset)) (points2 list(daisyRNGetPointsFromInstance(inst2))))
                (cond
                    (!null(points1) && !null(points2)
                        t
                    )
                    (!null(dbPointArrayAnd(hiGetCurrentWindow()->data->layoutWindow->cellView points1 points2))
                        t
                    )
                    (t
                        nil
                    )
                )
            )
        )
        (t
            nil
        )
    )
)

; Not used for now, found dbPointArrayAnd :-(
procedure( daisyRNIsEdgesIntersecting(edge1 edge2)
    println("daisyRNIsEdgesIntersecting")
    t
)

procedure( daisyRNGetEdgesFromPoints(points)
    (let ((edges list()))
        (for i 1 length(points)-1
            edges = append(list(list(car(points) car(cdr(points)))) edges)
            points = cdr(points)
        )
        edges
    )
)

; Assumes closed polygons
procedure( daisyRNIsPolygonIntersecting(points1 points2)
    println("HJ")
)

; Returns the points of the closed polygon forming an instance
procedure( daisyRNGetPointsFromInstance(inst)
    (case inst->objType
        ("rect" || "path" || "pathSeg"
            list(lowerLeft(inst->bBox) list(car(upperRight(inst->bBox)) car(cdr(lowerLeft(inst->bBox)))) upperRight(inst->bBox) list(car(lowerLeft(inst->bBox)) car(cdr(upperRight(inst->bBox)))) lowerLeft(inst->bBox))
        )
        ("polygon"
            append(inst->points list(car(inst->points)))
        )
        (t
            list()
        )
    )
)

/*procedure( daisyRNTraceBBox(layerName bBox)
    printf("daisyRNTraceBBox\n")
    println(layerName)
    println(bBox)
    
    
    ;geCreateMarkerByBBox(hiGetCurrentWindow()->cellView "warning" "daisy" "pin" "test" bBox)
    
)*/

; TODO Add termFilter
(procedure daisyRNHighlightdaisyRNForSelectedInstances(@optional (termFilter "") (clear t))
    printf("daisyRNHighlightdaisyRNForSelectedInstances\n")
    data = hiGetCurrentWindow()->data
    (if clear
        clearAllHighlights()
    )
    let( (window instances schInst layInst hilitePurposes)
        hilitePurposes = list("drawing2" "drawing3" "drawing4" "drawing5" "drawing6" "drawing7" "drawing8" "drawing9" "drawing2" "drawing3" "drawing4" "drawing5" "drawing6" "drawing7" "drawing8" "drawing9")
        window = hiGetCurrentWindow()
        ; Layout or schematic?
        (if window->cellView->cellViewType == "maskLayout" then
            ; Get selected instance
            instances = geGetSelectedSet()
            ;println(instances~>??)
            ; Find correspondence in schematic
            (if null(instances) then
            
                printf("No instances selected. At least one instance has to be selected.\n")
            )
            (foreach inst instances
                ; TODO Refactor
                schInst = daisyRNGetSchematicInstanceFromLayoutName(data inst->name)
                
                (if null(schInst) then
                    printf("No corresponding schematic instance.\n")
                )
                layInst = daisyRNGetInstanceFromName(window->cellView inst->name)
                ; Find connected nets and print
                (foreach conn schInst->conns
                    ;printf("hilitePurpose: %s\n" hilitePurposes)
                    ;printf("hilitePurpseose: %s\n" car(hilitePurposes))
                    ;println(hilitePurposes)
                    (setq hiliteset geCreateHilightSet(window->cellView list("hilite" car(hilitePurposes)) nil))
                    hilitePurposes = cdr(hilitePurposes)
                    hiliteset~>enable = t
                    ;gePushHilightStack(hiliteset)
                
                    ;println(conn->name)
                    ;println(conn->net->name)
                    
                    (setq layMainTerm daisyRNGetInstanceTermFromName(layInst conn->name))
                    ;println(layMainTerm~>??)
                    
                    (foreach term conn->net->allInstTerms
                        (if (term->name != "noConn" && !null(layMainTerm) && layInst->name != term->inst->name) then
                            ;println(strcat("Connected to " term->inst->name))
                            (setq layConnInst daisyRNGetLayoutInstanceFromName(data data->layoutWindow->cellView term->inst->name))
                            (setq layTerm daisyRNGetLayoutInstanceTermFromName(data layConnInst term->inst->name term->name))
                            ;daisyRNGetViewBBoxForTerminal(window->cellView car(layTerm->pins~>fig) layConnInst)
                            
                            ;printf("term name: %s\n" term->name)
                            (if null(layTerm) || null(layConnInst) then
                                ;printf("Layout instance does not exist in layout - skipping\n" term->inst->name)
                                (if !null(layConnInst) then
                                    printf("Layout pin %s does not exist in instance %s - skipping\n" term->name layConnInst->name)
                                )
                            else
                                ;printf("layTerm: %s\n" layTerm)
                            
                                ;println(layMainTerm->pins~>fig~>??)
                                ;println(layInst->xy)
                                ;println(car(layTerm->pins~>fig~>??))
                                ;println(layConnInst->xy)
                                geAddHilightLine(
                                    hiliteset
                                    list(
                                        centerBox(daisyMoveBBoxOrigin(car(layMainTerm->pins~>fig)->bBox layInst->xy))
                                        centerBox(daisyMoveBBoxOrigin(car(layTerm)->bBox layConnInst->xy))
                                    )
                                )
                            )
                        )
                    )
                )
                
                ; Hilight rats nest from instance to connected instances
            )
            
        else
            printf("Not layout, aborting.")
        )
    )
)

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
  
/*procedure( daisyRNDisplaySchematicInstances()
    println( "You have clicked daisyRNDisplaySchematicInstances" )
    ;shell( "ls ~/" )
    let( (schematicWindow schematic)
        schematicWindow = hiGetCurrentWindow()
        ;println(schematicWindow~>??)
        schematic = schematicWindow->cellView
        ;println(schematic~>??)
        ;println(schematic->nets~>??)
        geDeleteAllHilightSet(schematic)
        foreach(net schematic->nets
            daisyRNHighlightSchematicNet(schematic net)
        )
        foreach(inst schematic->instances
            ;println(inst->name)
            ;println(inst~>??)
            ;println(inst->cellView->cell~>??)
            (setq simLibs list("basic" "analogLib"))
            (if (member inst->libName simLibs) then
                println("Simlib")
            else
                println(strcat("   " inst->name ": " inst->libName " - " inst->cellName))
                ;println(inst~>??)
                ;foreach(conn inst->conns
                ;    println(conn~>??)
                ;)
                ;println(inst->libName)
                ;println(inst->cellName)
            )
        )
    )
)*/
   
procedure( clearAllHighlights()
    let( (window cellview)
        window = hiGetCurrentWindow()
        cellview = window->cellView
        geDeleteAllHilightSet(cellview)
        )
)

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
     
deUnRegUserTriggers("schematic")
deUnRegUserTriggers("maskLayout")
deRegUserTriggers("schematic" nil nil 'MyMenu)
deRegUserTriggers("maskLayout" nil nil 'MyMenu)  
