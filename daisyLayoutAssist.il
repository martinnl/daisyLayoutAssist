; Created in March 2018 by Martin Nielsen-Lönn (c)
;
; Creates a GUI for connecting schematic and layout instances in Cadence Virtuoso.
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

load("daisyLayoutAssistGui.il")
load("daisyLayoutAssistHelpers.il")


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

procedure( daisyRNGetEdgesFromPoints(points)
    (let ((edges list()))
        (for i 1 length(points)-1
            edges = append(list(list(car(points) car(cdr(points)))) edges)
            points = cdr(points)
        )
        edges
    )
)


     
deUnRegUserTriggers("schematic")
deUnRegUserTriggers("maskLayout")
deRegUserTriggers("schematic" nil nil 'MyMenu)
deRegUserTriggers("maskLayout" nil nil 'MyMenu)  
