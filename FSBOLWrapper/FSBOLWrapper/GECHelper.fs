// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.FSBOLWrapper.GECHelper


open FSBOL.ComponentDefinition
open FSBOL.SequenceAnnotation
open FSBOL.Component
open FSBOL.Range
open FSBOL.Sequence
open FSBOL.ComponentInstance
open FSBOL.Location

let createComponent =
    let innerFn (cd:ComponentDefinition)=
        let uri = Helper.append_url cd.uri "component"
        let name = Helper.append_name cd.name "component"
        let displayId = Helper.append_name cd.displayId "component"
        let persistentid = Helper.append_url_option cd.persistentIdentity "component"
        Component(uri,name,displayId,cd.version,persistentid,cd.uri,Access.Private,[],cd.roles,[])
    innerFn

let createSequenceAnnotations (components:Component list) =
    [0..(components.Length-1)] 
        |> List.map (fun i -> 
            let c = components.Item(i)
            let uri = Helper.append_url c.uri ("sa" + i.ToString())
            let name = Helper.append_name c.name ("sa" + i.ToString())
            let displayId = Helper.append_name c.displayId ("sa" + i.ToString())
            let persistentid = Helper.append_url_option c.persistentIdentity ("sa" + i.ToString())
            
            let uri_range = Helper.append_url uri ("range")
            let name_range = Helper.append_name name ("range")
            let displayId_range = Helper.append_name displayId ("range")
            let persistentid_range = Helper.append_url_option persistentid ("range")
            let r = Range(uri_range,name_range,displayId_range,c.version,persistentid_range,Orientation.Inline,(i+1),(i+1))
            
            SequenceAnnotation(uri,name,displayId,c.version,persistentid,Some(c),[r],c.roles)
        )
    
let createHigherFunction (name:string) (persistentId:string) (version:string) (cdlist:ComponentDefinition list) =
    let uri = persistentId + "/" + version + "/"

    let components = cdlist |> List.map (fun x -> createComponent x)
    //let createComponentForUrlPrefix = ComponentDefinition.createComponent urlPrefix
    //let componentList = components |> List.map createComponentForUrlPrefix
    //let rangeList = ComponentDefinition.createRanges urlPrefix components
    //let indexList = [1 .. components.Length]
    
    //let forSA = List.zip3 indexList componentList rangeList 
    //let sequenceAnnotationForUrlPrefix = ComponentDefinition.createSequenceAnnotationFromSingleRange urlPrefix
    //let sequenceAnnotations = forSA |> List.map sequenceAnnotationForUrlPrefix
    //let seq = Sequence(name+"_sequence",urlPrefix,displayId+"_sequence",version,ComponentDefinition.getConcatenatedSequence(components),Terms.dnasequence)
    
    let sequenceAnnotations = createSequenceAnnotations components
    ComponentDefinition(uri,Some(name),Some(name),Some(version),Some(persistentId),[],[ComponentDefinitionType.DNA],[FSBOL.Role.Role.EngineeredGene],[],components,sequenceAnnotations,[])


(*let createRanges (urlPrefix:string) (cdList:ComponentDefinition list)= 
    let mutable index = 1;
    let mutable start:int = 1;
    let ranges:System.Collections.Generic.List<Range> = new System.Collections.Generic.List<Range>() 
    for (cd:ComponentDefinition) in cdList do
        if List.isEmpty cd.sequences then 
            ranges.Add(Range("range",urlPrefix + "/annotation" + index.ToString() + "/range",cd.displayId+"_range",cd.version,start,start,Terms.inlineOrientation))
            start <- start+1
            index <- index+1
        else 
            for (seq:Sequence) in cd.sequences do
                match seq.elements with 
                | "" ->
                    ranges.Add(Range("range",urlPrefix + "/annotation" + index.ToString() + "/range",cd.displayId+"_range",cd.version,start,start,Terms.inlineOrientation))
                    start <- start + 1
                    index <- index+1
                | _ -> 
                    ranges.Add(Range("range",urlPrefix + "/annotation" + index.ToString() + "/range",cd.displayId+"_range",cd.version,start,start + seq.elements.Length-1,Terms.inlineOrientation))
                    start <- start + seq.elements.Length
                    index <- index+1

                

    let rangeList = ranges.ToArray() |> Array.toList
    rangeList


let getConcatenatedSequence (cdList:ComponentDefinition list) = 
    let rec concatSeq (cdListL:ComponentDefinition list) = 
        match cdListL with
        | [] -> ""
        | cd :: remaining -> 
            let concaternatedSequence = cd.sequences |> List.fold (fun acc (seq:Sequence) -> (acc + seq.elements) ) ""
            concaternatedSequence + concatSeq(remaining)
    
    concatSeq cdList


*)    