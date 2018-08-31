/* functionName(para1, para2)
 * Parameters
 * - para1
 * - para2
 *
 * Description
 */

/** daisyLAlog(level message)
 * Parameters
 * - level: what is printed is set by the level set at start-up
 *      0 - debug
 *      1 - info
 *      2 - warn
 *      3 - error
 *      4 - fatal
 * - message: message to be printed
 *
 * Description
 *
 * TODO
 * - add level to start-up
 */
procedure( daisyLAlog(level message)
    ; TODO For now everything is printed
    print("LA:: ")
    println(message)
 )


/************************************************************************
 * Fixed above
 * TODO Fix below
 ***********************************************************************/


procedure( daisyLAGetSchematicFromLayout(layout)
    (let (libName cellName)
        libName = layout->cellView->libName
        cellName = layout->cellView->cellName
        
        dbOpenCellViewByType(libName cellName "schematic")
    )
)

procedure( daisyLAGetInstanceFromName(cellView instName)
    daisyLAlog(0 "daisyLAGetInstanceFromName\n")
    (let (returnInstance)
        (foreach inst cellView->instances
            (if inst->name == instName then
                returnInstance = inst
            )
        )
        returnInstance
    )
)

procedure( daisyLAGetInstanceTermFromName(inst termName)
    daisyLAlog(0 "daisyLAGetInstanceTermFromName\n")
    ; Check if layout or schematic?
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

; TODO Fix daisyLAbBoxToPoints
procedure( daisyLAbBoxToPoints(bBox)
    ;let( ((points list()))
    ;    points = append(points lowerLeft(bBox)
    ;)
    bBox
    ; FIX
)

; TODO Add better check for valid layers
(procedure daisyLAGetTermPinInMetalOrPoly(term)
    daisyLAlog(0 "daisyLAGetTermPinInMetal\n")
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

; TODO Should it remember parentInst or term? Or bBox? Or?
(procedure daisyLATraceNetInst(cellView layerName bBox parentInst parentFig exploredInstances hiliteSet depth)
    daisyLAlog(0 "daisyLATraceNetInst")
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
                (if parentInst->objType != "polygon" || daisyLAIsIntersecting(overlap parentFig layerName) then
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
                                        exploredInstances = daisyLATraceNetInst(cellView layerName overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
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
                                            exploredInstances = daisyLATraceNetInst(cellView overlap->viaHeader->viaDef->layer2->name overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
                                        )
                                        (overlap->viaHeader->viaDef->layer2->name == layerName
                                            exploredInstances = append(list(overlap) exploredInstances)
                                            ;println(overlap~>??)
                                            geAddHilightRectangle(hiliteSet overlap->bBox)
                                            exploredInstances = daisyLATraceNetInst(cellView overlap->viaHeader->viaDef->layer1->name overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
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
                                    (if overlap->layerName == layerName && daisyLAIsIntersecting(overlap parentInst layerName) then
                                        ;println("Is intersecting")
                                        exploredInstances = append(list(overlap) exploredInstances)
                                        ;println(overlap~>??)
                                        geAddHilightPolygon(hiliteSet overlap->points)
                                        exploredInstances = daisyLATraceNetInst(cellView layerName overlap->bBox overlap overlap exploredInstances hiliteSet depth-1)
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
                                            metalFigs = daisyLAGetTermPinInMetalOrPoly(term)
                                            (foreach fig metalFigs
                                                ;println(fig~>??)
                                                (if fig->layerName == layerName then
                                                    (if daisyLAIsIntersecting(fig parentFig layerName overlap->xy) then
                                                        ;println("It is intersecting!")
                                                        (if !isOverlapping then
                                                            isOverlapping = t
                                                            exploredInstances = append(list(overlap) exploredInstances)
                                                            ;println("exploredInstances")
                                                            ;println(exploredInstances~>??)
                                                        )
                                                        ;println(fig~>??)
                                                        geAddHilightRectangle(hiliteSet daisyLATransformBBox(fig->bBox overlap->transform))
                                                        exploredInstances = daisyLATraceNetInst(cellView layerName daisyLATransformBBox(fig->bBox overlap->transform) overlap fig exploredInstances hiliteSet depth-1)
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

procedure( daisyLAMovePointsXY(points xy)
    (cond
        (null(points)
            list()
        )
        (length(points) == 0
            list()
        )
        (listp(points) && listp(car(points))
            ; List in list
            cons(daisyLAMovePointsXY(car(points) xy) daisyLAMovePointsXY(cdr(points) xy))
        )
        (listp(points)
            daisyLAMovePointXY(points xy)
        )
        (t
            println("It should never reach this statement in daisyLAMovePointsXY.")
        )
    )
)

; Returns the points of the closed polygon forming an instance
procedure( daisyLAGetPointsFromInstance(inst)
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

; TODO Add termFilter
(procedure daisyLAHighlightdaisyLAForSelectedInstances(@optional (termFilter "") (clear t))
    printf("daisyLAHighlightdaisyLAForSelectedInstances\n")
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
                schInst = daisyLAGetSchematicInstanceFromLayoutName(data inst->name)
                
                (if null(schInst) then
                    printf("No corresponding schematic instance.\n")
                )
                layInst = daisyLAGetInstanceFromName(window->cellView inst->name)
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
                    
                    (setq layMainTerm daisyLAGetInstanceTermFromName(layInst conn->name))
                    ;println(layMainTerm~>??)
                    
                    (foreach term conn->net->allInstTerms
                        (if (term->name != "noConn" && !null(layMainTerm) && layInst->name != term->inst->name) then
                            ;println(strcat("Connected to " term->inst->name))
                            (setq layConnInst daisyLAGetLayoutInstanceFromName(data data->layoutWindow->cellView term->inst->name))
                            (setq layTerm daisyLAGetLayoutInstanceTermFromName(data layConnInst term->inst->name term->name))
                            ;daisyLAGetViewBBoxForTerminal(window->cellView car(layTerm->pins~>fig) layConnInst)
                            
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
                                        centerBox(daisyLAMoveBBoxOrigin(car(layMainTerm->pins~>fig)->bBox layInst->xy))
                                        centerBox(daisyLAMoveBBoxOrigin(car(layTerm)->bBox layConnInst->xy))
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
   
procedure( clearAllHighlights()
    let( (window cellview)
        window = hiGetCurrentWindow()
        cellview = window->cellView
        geDeleteAllHilightSet(cellview)
        )
)

procedure( daisyLAMovePointXY(point xy)
    list((car(point) + car(xy)) (car(cdr(point)) + car(cdr(xy))))
)

(procedure daisyLAGetViewBBoxForTerminal(layout termFig inst)
       ;println(termFig)
       (setq bBox termFig->bBox)

    bBox
)