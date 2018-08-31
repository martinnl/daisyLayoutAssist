; Created in March 2018 by Martin Nielsen-Lönn ( c )
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

(load "daisyLayoutAssistGui.il")
(load "daisyLayoutAssistHelpers.il")

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

/* daisyLAUnpackBinding(data pack)
 * Parameters
 * - data
 * - pack
 *
 * Description
 *
 * T1ODO
 * - 
 */
procedure( daisyLAUnpackBinding(data pack)
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


procedure( daisyLAUnpackBinding(data pack)
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

procedure( daisyLAUnbindInstance(data desc)
    ; Get schematic and layout parts
    (let (
            (instBindings car(daisyLAGetBindingForInstance(data->bindings car(desc))))
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
        data->bindings->matched = daisyLARemoveMatchedInstance(data->bindings car(desc))
        
        data->bindings->unmatched_schematic = cons(schBinding data->bindings->unmatched_schematic)
        data->bindings->unmatched_layout= cons(layBinding data->bindings->unmatched_layout)
    )
)

procedure( daisyLACreateAndDisplayBindingWindow(data schDesc layDesc matchedTerms schTerms layTerms tree)    
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
        ?callback "daisyLABindingFormBindTermsCB(form)"
    )
    
    unbindButton = hiCreateButton(
        ?name `unbindTermsButton
        ?buttonText "Unbind terms"
        ?callback "daisyLABindingFormUnbindTermsCB(form)"
    )
    
    ; hiCreateAppForm
    form = hiCreateAppForm(
        ?name gensym("daisyLABindForm")
        ?formTitle "Select bindings for instance"
        ?callback "daisyLABindingFormCB(data schDesc layDesc tree form)"
        ?fields list(
                    bindButton
                    unbindButton
                    schTermsLabel
                    schTermsField
                    layTermsLabel
                    layTermsField
                    bindedTermsLabel
                    bindedTermsField)
        ?help "daisyLA"
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

procedure( daisyLABindingFormCB(data schDesc layDesc tree form)
    daisyLAlog(0 "daisyLABindingFormCB")
    bindingTermsValues = form->bindedTermsField->choices

    ; TODO Rename
    (if null(layDesc) then ; Editing existing binding
        test = list(car(schDesc) nth(1 schDesc))
    else
        test = list(car(schDesc) nth(1 layDesc))
    )
    daisyLAlog(0 test)
    
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
        
        data->bindings->unmatched_schematic = daisyLARemoveUnmatchedInstance(data->bindings->unmatched_schematic test)
        data->bindings->unmatched_layout = daisyLARemoveUnmatchedInstance(data->bindings->unmatched_layout test)
    else
        data->bindings->matched = daisyLAReplaceBinding(data->bindings->matched test)
    )
        
    println("IN CALLBACK")
    
    daisyLAPopulateBindingsTreeWithData(data tree)
    
    ; Save data
    ;daisyLASaveBinding(data tree)
    ; Load bindings
    ;daisyLALoadBinding(data tree)
)

procedure( daisyLARemoveUnmatchedInstance(unmatched instName)
    daisyLAlog(1 unmatched)
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


(procedure daisyLASchWindowInData(data)
    (cond
        (null(hiGetCurrentWindow()->data->schWindow)
            daisyLAlog(2 "No schematic window")
            nil
        )
        (!member(hiGetCurrentWindow()->data->schWindow hiGetWindowList())
            daisyLAlog(2 "No schematic window")
            nil
        )
        (t
            t
        )
    )
)

(procedure daisyLAAddBindingFromInstance(bindings inst)
    daisyLAlog(0 "daisyLAAddBindingFromInstance")
    daisyLAlog(0 inst)
    
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
                        (daisyLAInstInTable(inst matched)
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
            
            println(daisyLAPackBinding(data pack))
            
            
            ;print(list(list("schematic" schematic_instances~>??) list("layout" layout_instances~>??) list("matched" matched) list("unmatched" unmatched_layout)) corrFile->port)
            
        )
    )
)

(procedure daisyLAPopulateBindingsTreeWithData(data tree)
    hiTreeRemoveAllItems(tree)

    ; Add all schematic items
    ; Add all layout items
    
    ; Check if binding exists
    ; If exists - reload
    ; If it does not populate with schematic and layout
    
    (if !daisyLASchWindowInData(data)
        daisyLAOpenSchematic()
    )
    
    ; Add matched objects first
    ; Add unmatched schematic
    ; Add unmatched layout
    
    (foreach instBind data->bindings->matched
        schInst = daisyLAGetInstanceFromName(data->schWindow->cellView car(instBind))
        layInst = daisyLAGetInstanceFromName(data->layoutWindow->cellView nth(1 instBind))
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
        schInst = daisyLAGetInstanceFromName(data->schWindow->cellView car(instBind))
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
        layInst = daisyLAGetInstanceFromName(data->layoutWindow->cellView car(cdr(instBind)))
        tmp = hiCreateTreeItem(gensym(layInst->name) list("-" layInst->name "-" layInst->cellName "NO"))
        hiTreeAppendItem(tree tmp)
        ; Add both terminals
        tmp2 = hiCreateTree(gensym(layInst->name))
        hiItemInsertTree(tmp tmp2)
        (foreach terms nth(2 nth(2 instBind))
            hiTreeAppendItem(tmp2 hiCreateTreeItem(gensym(car(terms)) list("-" car(cdr(terms)) "" "" "")))
        )
    )
)

(procedure daisyLAPackBinding(data unpacked)
    (let ((result list(list("matched" unpacked->matched) list("unmatched" list("schematic" unpacked->unmatched_schematic) list("layout" unpacked->unmatched_layout)))))
        result
    )
)

(procedure daisyLAAutoBind()
    daisyLAlog(0 "daisyLAAutoBind")
    
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
        
                        data->bindings->unmatched_schematic = daisyLARemoveUnmatchedInstance(data->bindings->unmatched_schematic list(car(schBinding) nth(1 layBinding)))
                        data->bindings->unmatched_layout = daisyLARemoveUnmatchedInstance(data->bindings->unmatched_layout list(car(schBinding) nth(1 layBinding)))
                    else
                        println("No match")
                    )
                )
            )
        )
    )
)

(procedure daisyLAInstInTable(instName table)
    (let ((result nil))
        (foreach entry table
            (if car(entry) == instName
                result = t
            )
        )
        result
    )
)

procedure( daisyLAOpenSchematic()
    data = hiGetCurrentWindow()->data
    (if !daisyLASchWindowInData(data) then
        data->schWindow = geOpen(
            ?lib geGetEditCellView()~>libName
            ?cell geGetEditCellView()~>cellName
            ?view "schematic"
            ?mode "r")
        data->schWindow->data = data
        hiRaiseWindow(data->layoutWindow)
     else
         hiRaiseWindow(data->schWindow)
     )
)
 
(procedure daisyLATransformBBox(bBox transform)
    (let ((xy car(transform)) (R nth(1 transform)) (intbBox bBox))
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
        
        (if !null(xy)
            intbBox = daisyLAMoveBBoxOrigin(intbBox xy)
        )
        
        intbBox
    )
)
    
; Moves the bounding box (bBox) so the lower left corner is at xy
(procedure daisyLAMoveBBoxOrigin(bBox xy)
    (setq startX (car(lowerLeft(bBox)) + car(xy)))
    (setq startY (cadr(lowerLeft(bBox))  + cadr(xy)))
    (setq start list(startX startY))
    (setq endX (car(upperRight(bBox)) + car(xy)))
    (setq endY (cadr(upperRight(bBox))  + cadr(xy)))
    (setq end list(endX endY))
    
    list(start end)
)
 
(procedure daisyLADisplayLayoutInstances()
    ; Clear all markers first
    geHiDeleteAllMarker()
    let( (layoutWindow layout)
           layoutWindow = hiGetCurrentWindow()
           layout = layoutWindow->cellView
        (setq hiliteset geCreateHilightSet(layout list("hilite" "drawing") t))
        hiliteset~>enable = t
        gePushHilightStack(hiliteset)
           foreach(inst layout->instances
               foreach(terminal inst->master->terminals
                   (setq bBox (car (car terminal->net->pins)->figs)->bBox)
                   geCreateMarkerByBBox(layout "warning" "daisy" "pin" terminal->name daisyLATransformBBox(bBox inst->transform))
                   geAddHilightLine(hiliteset list(list(0 0) centerBox(daisyLATransformBBox(bBox inst->transform))))
            )
        )
    )
)

procedure( daisyLAHighlightSchematicNet(schematic net)
    (setq hiliteset geCreateHilightSet(schematic list("hilite" "drawing") nil))
    hiliteset~>enable = t
    
    foreach(fig net->figs
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

(procedure daisyLAGetSchematicInstanceFromLayoutName(data layInstName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (if nth(1 binding) == layInstName
                result = daisyLAGetInstanceFromName(data->schWindow->cellView nth(0 binding))
            )
        )
        result
    )
)

(procedure daisyLAGetLayoutInstanceFromName(data cellView schInstName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (if nth(0 binding) == schInstName
                result = daisyLAGetInstanceFromName(cellView nth(1 binding))
            )
        )
        result
    )
)

(procedure daisyLAGetLayoutInstanceTermFromName(data layInst ischInstName schTermName)
    (let ((result nil))
        (foreach binding data->bindings->matched
            (foreach term nth(0 nth(2 binding))
                (if car(term) == schTermName
                    result = daisyLAGetTermPinInMetalOrPoly(daisyLAGetInstanceTermFromName(layInst nth(1 term)))
                )
            )
        )
        result
    )
)

(procedure daisyLATraceTerm(instance termName @optional (clear t))
    daisyLAlog(0 "daisyLATraceTerm")
    
    ; TODO Check so we are in layout
    (if clear
        clearAllHighlights()
    )
    ; TODO Only Metals and poly!!!
    
    (if instance->objType == "inst" then        
        hiliteSet = geCreateHilightSet(hiGetCurrentWindow()->cellView list("hilite" "drawing2") nil)
        hiliteSet->enable = t
        
        (let ((term daisyLAGetInstanceTermFromName(instance termName)) (bBoxes list()))
            exploredInstances = list(instance)
            
            (foreach fig daisyLAGetTermPinInMetalOrPoly(term)
                bBox = daisyLAGetViewBBoxForTerminal(hiGetCurrentWindow()->cellView fig instance)
                
                geAddHilightRectangle(hiliteSet daisyLATransformBBox(bBox instance->transform))
                daisyLATraceNetInst(hiGetCurrentWindow()->cellView fig->layerName daisyLATransformBBox(bBox instance->transform) instance fig exploredInstances hiliteSet 20)
            )
        )
    else
        printf("Please select a instance and not a %s.\n" instance->objType)
    )
    "DONE"
)

(procedure daisyLATraceNet()
    printf("daisyLATraceNet\n")
    instance = car(geGetSelSet()) ; Limited to one object for now
    daisyLATraceTerm(instance "S")
)

procedure( daisyLAIsBoundingBoxesOverlap(bBox1, bBox2)
    
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

(procedure daisyLAIsIntersecting(inst1 inst2 layerName @optional (inst1_xy_offset list(0 0)) (inst2_xy_offset list(0 0)))
    daisyLAlog(0 "daisyLAIsIntersecting")
    
    ; Bounding box overlap?
    (cond
        ; lowerLeft 
        (daisyLAIsBoundingBoxesOverlap(daisyLATransformBBox(inst1->bBox inst1->transform) daisyLAMoveBBoxOrigin(inst2->bBox inst2_xy_offset))
            (let ((points1 daisyLAMovePointsXY(list(daisyLAGetPointsFromInstance(inst1)) inst1_xy_offset)) (points2 list(daisyLAGetPointsFromInstance(inst2))))
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

procedure( daisyLAGetEdgesFromPoints(points)
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
deRegUserTriggers("schematic" nil nil 'daisyLAMenu)
deRegUserTriggers("maskLayout" nil nil 'daisyLAMenu)  
