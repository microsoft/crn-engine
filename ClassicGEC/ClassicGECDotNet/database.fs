[<JavaScript>]
module Microsoft.Research.GEC.Database

open Microsoft.Research.GEC
open Microsoft.Research.FSBOLWrapper
open FSBOL
open FSBOL.Annotation
open FSBOL.ComponentDefinition
open FSBOL.ModuleDefinition
open FSBOL.FunctionalComponent
open FSBOL.Participation
open FSBOL.Interaction
open FSBOL.SBOLDocument
open FSBOL.TopLevel
open Parser
open System.Diagnostics

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

(* ************************************************************************************************* *)

(* The different kinds of property that part types can have. *)

type device = string * string list


type pcrProperty = CODES of string list * float
type promProperty = POS of string list * float * float * float
                  | NEG of string list * float * float * float
                  | CON of float
                  | FRATE of Ast.aexp
type rbsProperty = RATE of float

(* An encoding of part types, along with their properties. *)
type partType = PCR of pcrProperty
              | PROM of promProperty list
              | RBS of rbsProperty
              | TER

let fold_string_list (strList:string list) (folder:string) = 
    match strList.Length with 
    | 0 -> ""
    | 1 -> strList.Head
    | _ -> strList.Tail |> List.fold (fun f s -> (f + folder + s)) strList.Head

let device_to_string (d:device) = 
    let (devname,deviceComps) = d
    devname + "[" + (fold_string_list deviceComps ";") + "]"

(* Produce a string representation of a part type. *)
let string_of_partType (pt:partType) =
  let string_of_promProperty = function
    | POS(zs,q1,q2,q3) -> "pos(" + (Ast.complexString zs) + ", " + (Lib.display_float q1) + ", " + (Lib.display_float q2) + ", " + (Lib.display_float q3) + ")"
    | NEG(zs,q1,q2,q3) -> "neg(" + (Ast.complexString zs) + ", " + (Lib.display_float q1) + ", " + (Lib.display_float q2) + ", " + (Lib.display_float q3) + ")"
    | CON(q) -> "con(" + (Lib.display_float q) + ")"
    | FRATE(a) -> "frate(" + (Ast.stringOfAExp a) + ")"
  in
  match pt with
    | PCR(CODES(zs,q)) -> "pcr<codes(" + (Ast.complexString zs) + ", " + (Lib.display_float q) + ")>"
    | PROM(pps) -> "prom<" + (Lib.string_of_list string_of_promProperty ", " pps) + ">"
    | RBS(RATE(q)) -> "rbs<rate(" + (Lib.display_float q) + ")>"
    | TER -> "ter"

(* Compute FS(Q+t). *)
let speciesInPartType (qt:partType) : string list list =
  let getPromPropertySpeciesNames = function POS(xs,_,_,_) | NEG(xs,_,_,_) -> [xs] | _ -> [] in
  let snames = match qt with
                | PCR(CODES(xs,_)) -> [xs]
                | PROM(pps) -> Lib.collect_union Ast.complexesEqual getPromPropertySpeciesNames pps
                | RBS _ | TER -> []
  in
  Lib.remove_duplicates (=) snames



(* ************************************************************************************************* *)

(* A "database" consists of a "parts database" and a "reaction database".
   A "parts database" is just a mapping of part identifiers (i.e. strings) to their part types.
   A "reaction database" is just a list of reactions. *)
type 'a entry = { value:'a; enabled:bool; comments:string }
type t = { parts:partType entry Stringmap.t; devices: device list; reactions:Gecreaction.t entry list }

let partTypeToSBOL (name:string) (p:partType entry)= 
    
    let role = match p.value with 
               | PROM _ -> Role.Promoter
               | RBS _ -> Role.RBS
               | PCR _ -> Role.CDS
               | TER -> Role.Terminator
    
    let renamedName = name.Replace("::","_")
    let gecSO = "www.microsoft.com/gec#"
    (*let RB = gecSO + "RB"
    let RUB = gecSO + "RUB"
    let RTB = gecSO + "RTB"
    let RT = gecSO + "RT"
    let R = gecSO + "R"
    let RD = gecSO + "RD"
    let P = gecSO + "P"*)
    
    let RB = "RB"
    let RUB = "RUB"
    let RTB = "RTB"
    let RT = "RT"
    let R = "R"
    let RD = "RD"
    let P = "P"
    

    let annotations:Annotation list = 
        match p.value with 
        | PROM(a) -> 
            let propAnnotations = 
                a |> List.map(fun promprop ->
                    match promprop with 
                    | POS(p,rb,rub,rtb) -> 
                        let compositeName = p |> List.reduce(fun a b -> a + "::" + b)
                        [Helper.create_string_annotation (Name(P)) compositeName ; 
                         Helper.create_double_annotation (Name(RB)) rb;
                         Helper.create_double_annotation (Name(RUB)) rub;
                         Helper.create_double_annotation (Name(RTB)) rtb]
                    | NEG(p,rb,rub,rtb) -> 
                        let compositeName = p |> List.reduce(fun a b -> a + "::" + b)
                        [Helper.create_string_annotation (Name(P)) compositeName; 
                         Helper.create_double_annotation (Name(RB)) rb;
                         Helper.create_double_annotation (Name(RUB)) rub;
                         Helper.create_double_annotation (Name(RTB)) rtb]
                    | CON(rt) -> 
                        [Helper.create_double_annotation (Name(RT)) rt]
                    | _ -> [])
            [0..(propAnnotations.Length-1)] 
                |> List.map (fun i -> 
                    let na = NestedAnnotation(Name("PromoterProperty" + i.ToString() + "Annotations"),gecSO + "PromoterProperties",propAnnotations.Item(i))
                    Annotation(Name("PromoterProperty" + i.ToString()),AnnotationValue.NestedAnnotation(na)))            
        | RBS(RATE(r)) -> [Helper.create_double_annotation (Name(R)) r]
        | PCR(CODES(codes,rd)) -> 
            let compositeName = codes |> List.reduce(fun a b -> a + "::" + b)
            [Helper.create_string_annotation (Name(P)) compositeName;
             Helper.create_double_annotation (Name(RD)) rd]
        | TER -> []

    let version = "1"
    let persistentId = "http://www.microsoft.com/gec/db/" + renamedName + "/"
    let uri = persistentId + renamedName + "/" + version + "/"
    let c = new ComponentDefinition(uri,Some(renamedName),Some(renamedName),Some(version),Some(persistentId),[],[ComponentDefinitionType.DNA],[role],[],[],[],[])
    c.annotations <- annotations
    c
    

let createProteinCDs (tableList:((string*(partType entry))list)) = 
    let partTypes = tableList 
                    |> List.map (fun (x,y) -> y.value)
    let promProts = partTypes 
                    |> List.choose ( fun x ->
                             match x with 
                             | PROM(props) -> Some props
                             | _ -> None)
                    |> List.map (fun props ->
                    props |> List.map(fun prop ->   
                        match prop with 
                        | POS(p,rb,rub,rtb) -> Some(p |> List.reduce(fun a b -> a + "::" + b))
                        | NEG(p,rb,rub,rtb) -> Some(p |> List.reduce(fun a b -> a + "::" + b))
                        | _ -> None
                        )
                    ) |> List.concat 
                    |> List.filter (fun x-> 
                        match x with 
                        | Some(_) -> true
                        | None -> false
                    ) |> List.map (fun x -> x.Value)
                    
    let cdsProts = partTypes 
                   |> List.choose (fun x -> 
                        match x with 
                        | PCR(CODES(prots,r)) -> Some (prots,r)
                        | _ -> None
                   ) |> List.map (fun (prots,r) -> 
                        prots |>  List.reduce(fun a b -> a + "::" + b)
                   ) 
    
    let prots = (promProts@cdsProts) |> Set.ofList |> List.ofSeq
    prots |> List.map(fun protName -> 
        let renamedProt = protName.Replace("::","_")
        let version = "1"
        let persistentId = "http://www.microsoft.com/gec/db/" + renamedProt + "/"
        let uri = persistentId + renamedProt + "/" + version + "/"    
        new ComponentDefinition(uri,Some(renamedProt),Some(renamedProt),Some(version),Some(persistentId),[],[ComponentDefinitionType.Protein],[],[],[],[],[]))
    

let createModuleDefinitions (cds:ComponentDefinition list) (tableList:((string*(partType entry))list))=
    let urlPrefix = "http://www.microsoft.com/gec/db/"
    let version = "1"

    
    let findCDwithURI (uri:string) = 
        match cds |> List.tryFind (fun cd -> cd.uri = uri) with 
        | Some (x) -> x
        | None -> failwith "Error. This CD should have been created in an earlier step"
    let findCDwithDisplayID (display:string) = 
        let displayIdsMatch (cdDisplayId:string option) (display:string) = 
            match cdDisplayId with 
            | Some(d) -> (d=display) 
            | None -> failwith "Error. This CD should have a displayId created in an earlier step"
        match cds |> List.tryFind (fun cd -> displayIdsMatch (cd.displayId)  display) with 
        | Some(x) -> x
        | None -> failwith "Error. This CD should have been created in an earlier step"
    let pcrs = tableList 
               |> List.choose (fun (name,x) -> 
                   match x.value with 
                   | PCR(CODES(prots,r)) -> Some(name,prots,r)
                   | _ -> None
               )
    
    let proms = tableList 
                |> List.choose (fun (name,x) -> 
                    match x.value with 
                    | PROM(props) -> Some(name,props)
                    | _ -> None
                ) 

    let mds = 
        pcrs |> List.map (fun (pcrName,prots,r) -> 
            let compositeName = prots |>  List.reduce(fun a b -> a + "_" + b)
            let mdsinProms = 
                proms |> 
                List.map(fun (promName,props) -> 
                    let mdsInProps = 
                        props |> List.map(fun prop -> 
                            match prop with 
                            | POS(regList,_,_,_) ->
                               let compositeReg = regList |> List.reduce(fun a b -> a + "_" + b)
                               if compositeReg = compositeName then 
                                   let pcr_cd = findCDwithDisplayID pcrName
                                   let prot_cd = findCDwithDisplayID compositeReg
                                   let prom_cd =  findCDwithDisplayID promName
                                   
                                   let compoundNameProd = pcrName + "_" + compositeName
                                   let mdProd_name = compoundNameProd  + "_production_md"
                                   let mdProd_perId  = urlPrefix + "/" + mdProd_name + "/"
                                   let mdProd_uri = mdProd_perId + version + "/"

                                   let fc_pcr_name = pcrName + "_FC"
                                   let fc_pcr_perId = urlPrefix + fc_pcr_name + "/"
                                   let fc_pcr_uri = fc_pcr_perId + version + "/"

                                   let fc_prot_name = compositeName + "_protein_FC"
                                   let fc_prot_perId = urlPrefix + fc_prot_name + "/"
                                   let fc_prot_uri = fc_pcr_perId + version + "/"

                                   let fc_pcr = new FunctionalComponent(fc_pcr_uri,Some(fc_pcr_name),Some(fc_pcr_name),Some(version),Some(fc_pcr_perId),pcr_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   let fc_prot = new FunctionalComponent(fc_prot_uri,Some(fc_prot_name),Some(fc_prot_name),Some(version),Some(fc_prot_perId),prot_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   
                                   let part_pcr_name = pcrName+"_participation"
                                   let part_pcr_perId = urlPrefix + part_pcr_name + "/"
                                   let part_pcr_uri = part_pcr_perId + version + "/"
                                    
                                   let part_prot_name = compositeName+"prot_participation"
                                   let part_prot_perId = urlPrefix + part_prot_name + "/"
                                   let part_prot_uri = part_prot_perId + version + "/"

                                   let part_pcr = new Participation(part_pcr_uri,Some(part_pcr_name),Some(part_pcr_name),Some(version),Some(part_pcr_perId),[ParticipationRole.Template],fc_pcr)                                   
                                   let part_prot = new Participation(part_prot_uri, Some(part_prot_name),Some(part_prot_name),Some(version),Some(part_prot_perId),[ParticipationRole.Product],fc_prot)
                            
                                   let interactionProd_name = compoundNameProd + "_production_interaction"
                                   let interactionProd_perId = mdProd_perId + "/" + interactionProd_name + "/"
                                   let interactionProd_uri = interactionProd_perId + version + "/"

                                   let interactionProd = new Interaction(interactionProd_uri,Some(interactionProd_name),Some(interactionProd_name),Some(version),Some(interactionProd_perId),[InteractionType.GeneticProduction],[part_pcr;part_prot])
                                   
                                    
                                   let mdProd = new ModuleDefinition(mdProd_uri,Some(mdProd_name),Some(mdProd_name),Some(version),Some(mdProd_perId),[],[],[],[interactionProd],[fc_pcr;fc_prot],[])
                               
                                   let compoundNameStim = compositeName + "_" + promName
                                   let mdStim_name = compoundNameStim  + "_stimulation_md"
                                   let mdStim_perId  = urlPrefix + "/" + mdStim_name + "/"
                                   let mdStim_uri  = mdStim_perId + "/" + version + "/"
                                   
                                   let fc_complex_name = compositeName + "_complex_FC"
                                   let fc_complex_perId = urlPrefix + fc_complex_name + "/"
                                   let fc_complex_uri = fc_pcr_perId + version + "/"

                                   let fc_prom_name = promName + "_FC"
                                   let fc_prom_perId = urlPrefix + fc_prom_name + "/"
                                   let fc_prom_uri = fc_prom_perId + version + "/"

                                   let fc_complex = new FunctionalComponent(fc_complex_uri,Some(fc_complex_name),Some(fc_complex_name),Some(version),Some(fc_complex_perId),prot_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   let fc_prom = new FunctionalComponent(fc_prom_uri,Some(fc_prom_name),Some(fc_prom_name),Some(version),Some(fc_prom_perId),prom_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                    
                                   let part_complex_name = compositeName+"complex_participation"
                                   let part_complex_perId = urlPrefix + part_complex_name + "/"
                                   let part_complex_uri = part_complex_perId + version + "/"
                                   
                                   let part_prom_name = pcrName+"_participation"
                                   let part_prom_perId = urlPrefix + part_prom_name + "/"
                                   let part_prom_uri = part_prom_perId + version + "/"
                                   
                                   let part_complex = new Participation(part_complex_uri,Some(part_complex_name),Some(part_complex_name),Some(version),Some(part_complex_perId),[ParticipationRole.Stimulator],fc_complex)
                                   let part_prom = new Participation(part_prom_uri,Some(part_prom_name),Some(part_prom_name),Some(version),Some(part_prom_perId),[ParticipationRole.Stimulated],fc_prom)
                                   
                                   let interactionStim_name = compoundNameStim + "_stimulation_interaction"
                                   let interactionStim_perId = mdStim_perId + "/" + interactionStim_name + "/"
                                   let interactionStim_uri = interactionStim_perId + "/" + version + "/"

                                   let interactionStim = new Interaction(interactionStim_uri,Some(interactionStim_name),Some(interactionStim_name),Some(version),Some(interactionStim_perId),[InteractionType.Stimulation],[part_complex;part_prom])
                            
                                   let mdStim = new ModuleDefinition(mdStim_uri,Some(mdStim_name),Some(mdStim_name),Some(version),Some(mdStim_perId),[],[],[],[interactionStim],[fc_complex;fc_prom],[])
                            
                                   Some([mdProd;mdStim])
                               else 
                                   None
                            | NEG(regList,_,_,_) -> 
                               let compositeReg = regList |> List.reduce(fun a b -> a + "::" + b)
                               if compositeReg = compositeName then 
                                   let pcr_cd = findCDwithDisplayID pcrName
                                   let prot_cd = findCDwithDisplayID compositeReg
                                   let prom_cd =  findCDwithDisplayID promName
                                   
                                   let compoundNameProd = pcrName + "_" + compositeName
                                   let mdProd_name = compoundNameProd  + "_production_md"
                                   let mdProd_perId  = urlPrefix + "/" + mdProd_name + "/"
                                   let mdProd_uri = mdProd_perId + version + "/"

                                   let fc_pcr_name = pcrName + "_FC"
                                   let fc_pcr_perId = urlPrefix + fc_pcr_name + "/"
                                   let fc_pcr_uri = fc_pcr_perId + version + "/"

                                   let fc_prot_name = compositeName + "_protein_FC"
                                   let fc_prot_perId = urlPrefix + fc_prot_name + "/"
                                   let fc_prot_uri = fc_pcr_perId + version + "/"

                                   let fc_pcr = new FunctionalComponent(fc_pcr_uri,Some(fc_pcr_name),Some(fc_pcr_name),Some(version),Some(fc_pcr_perId),pcr_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   let fc_prot = new FunctionalComponent(fc_prot_uri,Some(fc_prot_name),Some(fc_prot_name),Some(version),Some(fc_prot_perId),prot_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   
                                   let part_pcr_name = pcrName+"_participation"
                                   let part_pcr_perId = urlPrefix + part_pcr_name + "/"
                                   let part_pcr_uri = part_pcr_perId + version + "/"
                                    
                                   let part_prot_name = compositeName+"prot_participation"
                                   let part_prot_perId = urlPrefix + part_prot_name + "/"
                                   let part_prot_uri = part_prot_perId + version + "/"

                                   let part_pcr = new Participation(part_pcr_uri,Some(part_pcr_name),Some(part_pcr_name),Some(version),Some(part_pcr_perId),[ParticipationRole.Template],fc_pcr)                                   
                                   let part_prot = new Participation(part_prot_uri, Some(part_prot_name),Some(part_prot_name),Some(version),Some(part_prot_perId),[ParticipationRole.Product],fc_prot)
                            
                                   let interactionProd_name = compoundNameProd + "_production_interaction"
                                   let interactionProd_perId = mdProd_perId + "/" + interactionProd_name + "/"
                                   let interactionProd_uri = interactionProd_perId + version + "/"

                                   let interactionProd = new Interaction(interactionProd_uri,Some(interactionProd_name),Some(interactionProd_name),Some(version),Some(interactionProd_perId),[InteractionType.GeneticProduction],[part_pcr;part_prot])
                                   
                                    
                                   let mdProd = new ModuleDefinition(mdProd_uri,Some(mdProd_name),Some(mdProd_name),Some(version),Some(mdProd_perId),[],[],[],[interactionProd],[fc_pcr;fc_prot],[])
                                   
                                   
                                   let compoundNameInh = compositeName + "_" + promName
                                   let mdInh_name = compoundNameInh  + "_inhibition_md"
                                   let mdInh_perId  = urlPrefix + "/" + mdInh_name + "/"
                                   let mdInh_uri  = mdInh_perId + "/" + version + "/"
                                   
                                   let fc_complex_name = compositeName + "_complex_FC"
                                   let fc_complex_perId = urlPrefix + fc_complex_name + "/"
                                   let fc_complex_uri = fc_pcr_perId + version + "/"

                                   let fc_prom_name = promName + "_FC"
                                   let fc_prom_perId = urlPrefix + fc_prom_name + "/"
                                   let fc_prom_uri = fc_prom_perId + version + "/"

                                   let fc_complex = new FunctionalComponent(fc_complex_uri,Some(fc_complex_name),Some(fc_complex_name),Some(version),Some(fc_complex_perId),prot_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   let fc_prom = new FunctionalComponent(fc_prom_uri,Some(fc_prom_name),Some(fc_prom_name),Some(version),Some(fc_prom_perId),prom_cd.uri,ComponentInstance.Access.Private,[],Direction.InOut)
                                   
                                   let part_complex_name = compositeName+"complex_participation"
                                   let part_complex_perId = urlPrefix + part_complex_name + "/"
                                   let part_complex_uri = part_complex_perId + version + "/"
                                   
                                   let part_prom_name = pcrName+"_participation"
                                   let part_prom_perId = urlPrefix + part_prom_name + "/"
                                   let part_prom_uri = part_prom_perId + version + "/"
                                   
                                   let part_complex = new Participation(part_complex_uri,Some(part_complex_name),Some(part_complex_name),Some(version),Some(part_complex_perId),[ParticipationRole.Inhibitor],fc_complex)
                                   let part_prom = new Participation(part_prom_uri,Some(part_prom_name),Some(part_prom_name),Some(version),Some(part_prom_perId),[ParticipationRole.Inhibited],fc_prom)
                                   
                                   let interactionInh_name = compoundNameInh + "_inhibition_interaction"
                                   let interactionInh_perId = mdInh_perId + "/" + interactionInh_name + "/"
                                   let interactionInh_uri = interactionInh_perId + "/" + version + "/"

                                   let interactionInh = new Interaction(interactionInh_uri,Some(interactionInh_name),Some(interactionInh_name),Some(version),Some(interactionInh_perId),[InteractionType.Inhibition],[part_complex;part_prom])
                            
                                   let mdInh = new ModuleDefinition(mdInh_uri,Some(mdInh_name),Some(mdInh_name),Some(version),Some(mdInh_perId),[],[],[],[interactionInh],[fc_complex;fc_prom],[])
                            
                                   Some([mdProd;mdInh])
                               else 
                                   None
                            | _ -> None) 
                            |> List.filter (fun optionX -> 
                                match optionX with 
                                | Some (_) -> true
                                | None -> false)
                            |> List.map( fun x -> x.Value)
                    if mdsInProps.IsEmpty then 
                        []
                    else 
                        mdsInProps |> List.reduce (fun a b -> a@b)
                            )
            if mdsinProms.IsEmpty then 
                []
            else
                mdsinProms |>List.reduce (fun a b -> a@b)
            )
    if mds.IsEmpty then 
        []
    else 
        mds |>List.reduce (fun a b -> a@b)

let convertTableToSBOLDocument (table:t) = 
    let cds = table.parts |> List.ofSeq |> List.map (fun (x) -> (x.Key,x.Value)) |> List.map (fun (name,part) -> partTypeToSBOL name part)
    
    let protCds = createProteinCDs (table.parts |> List.ofSeq |> List.map (fun x -> (x.Key,x.Value)))
    let allCds = (cds@protCds)
    let mds = createModuleDefinitions allCds (table.parts |> List.ofSeq |> List.map (fun x -> (x.Key,x.Value)))
    let collections = 
        (allCds |> List.map (fun x -> x :> TopLevel))
        @ (mds|> List.map (fun x -> x :> TopLevel))
    SBOLDocument(collections)

(* The "empty" database. *)
let empty = { parts=Stringmap.empty; devices = []; reactions=[] }

(* Produce a string representation of a database entry. *)
let string_of_entry (display:'a -> string) (entry:'a entry) : string =
  let enabledStr = if entry.enabled then "[*] " else "[ ] " in
  let valueStr = display entry.value in
  let commentsStr = if entry.comments = "" then "" else " (Comments: \"" + entry.comments + "\")" in
  enabledStr + valueStr + commentsStr

(* Produce a string representation of a database. *)
let display (db:t) : string = 
  let partsString = Stringmap.fold (fun str x entry -> str + Lib.newline + x + " : " + (string_of_entry string_of_partType entry)) "" db.parts in
  let reactionsString = Lib.fold_left (fun str entry -> str + Lib.newline + (string_of_entry Gecreaction.display entry)) "" db.reactions in
  "PARTS:" + partsString + Lib.newline + Lib.newline +
  "REACTIONS:" + reactionsString + Lib.newline

(* Find out the part type for an identifer (if it exists). *)
let findPart (db:t) (id:string) : partType option =
  match Stringmap.tryFind id db.parts with
  | Some entry -> Some entry.value
  | None -> None

(* ************************************************************************************************* *)
(* Functions for adding entries to the database. *)

(* Convert an abstractComplex into a string list, assuming it is "simple" (i.e. just strings...) *)
let convertSimpleComplex (vs:Ast.abstractComplex) : string list =
  let convertSimpleValue (v:Ast.value) : string =
    match v with
    | Ast.IdVal x -> x
    | _ -> failwith ("Only strings are allowed in species names in the database " + Lib.paren("found " + Ast.stringOfValue v))
  in
  List.map convertSimpleValue vs

(* Get a float from an Ast.value. *)
let getValueFloat (v:Ast.value) =
  match v with
  | Ast.FloatVal f -> f
  | _ -> failwith ("Only float rates are allowed in the database " + Lib.paren("found " + Ast.stringOfValue v))


type parser = Parser.t<t>
type partParser = Parser.t<t>



let createEntry (id:string, part:partType) = 
    let entry:partType entry = {value = part;enabled = true;comments = ""}
    (id,entry)
    

let createTable (partMapList:(string*partType)list) (devicelist:device list)=
    let tableMap = partMapList |> List.map createEntry
    {parts = Stringmap.of_list(tableMap); reactions=[]; devices = devicelist}
    



let TAB = Parser.kw "\t"
let COMMA = Parser.kw ","
let SEMICOLON = Parser.kw ";"

let delimiter = COMMA //Primary delimiter
let sDelimiter = SEMICOLON //Secondary Delimiter used to separate elements or values within a cell

let NEWLINE = Parser.newline

let lookaheadLinebreak = Parser.pTry (Parser.linebreak >>. Parser.failParser "" <|> Parser.satisfy Parser.isWhiteSpace >>. preturn ())
let spacesnlb :t<string>  = fun st -> 
  match many  (commentLine <|> commentMultiline () <|> lookaheadLinebreak) <| st with
  | OkEmpty    (_, st') -> OkEmpty ("", st')
  | OkConsumed (_, st') -> OkEmpty ("", st')
  | FailEmpty _         -> OkEmpty ("", st)
  | FailConsumed (e, p) -> FailConsumed (e, p)
let kwnlb s = pstring s .>> spacesnlb
let bracketnlb l r = Parser.between (Parser.kw l) (Parser.spaces >>. kwnlb r)

let bracketNoSpace l r = Parser.between (Parser.pstring l) (Parser.spaces >>. Parser.pstring r)
let sqbracketNoSpace a = bracketnlb "[" "]" a
let parenNoSpace a      = bracketnlb "(" ")" a 
//let nameGEC = (Parser.many1Satisfy Parser.isLetter .>>. Parser.manySatisfy (fun c -> Parser.isLetter c || Parser.isDigit c || c = '_'|| c = '-' || c = '\'') |>> fun (a,b) -> a + b) <?> "an identifier"

let parsePos = Parser.kw "pos" >>. parenNoSpace(GecSpecies.parse_species .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat) |>> fun(((regBy,rb),rub),rtb) -> POS(regBy,rb,rub,rtb)
let parseNeg = Parser.kw "neg" >>. parenNoSpace(GecSpecies.parse_species .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat) |>> fun(((regBy,rb),rub),rtb) -> NEG(regBy,rb,rub,rtb)
let parseCon = Parser.kw "con" >>. parenNoSpace (Parser.pfloat) |>> fun (rt:float) -> CON(rt)

let parsePromProperties = Parser.choice[
                            parsePos
                            parseNeg
                            parseCon
                          ]

let parseCodes = Parser.kw "codes" >>. parenNoSpace (GecSpecies.parse_species .>> sDelimiter .>>. Parser.pfloat) |>> fun ((codes:string list), rd:float) -> CODES(codes,rd)
(*let parseConstitutiveProm = parseCon |>> fun (con:promProperty) -> PROM([con])
let parseRegulatedProm = (Parser.kw "pos" <|> Parser.kw "neg") .>>. parenNoSpace (nameGEC .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat .>> sDelimiter .>>. Parser.pfloat) .>> sDelimiter .>>. parseCon |>> fun ((regType:string,((((regBy:string list),rb:float),rub:float),rtb:float)),conProp:promProperty) -> 
    match regType with
    | "pos" -> PROM([POS(regBy,rb,rub,rtb);conProp])
    | "neg" -> PROM([NEG(regBy,rb,rub,rtb);conProp])
    | _ -> failwith ""*)

let parseRate = Parser.kw "rate" >>. parenNoSpace (Parser.pfloat) |>> fun(r:float) -> RATE(r) 


let pdevicedelim = Parser.kw "|" <|> Parser.kw ";"
let (parse_device:t<device>) = Parser.name .>> Parser.spaces  .>> Parser.kw "=" .>>. Parser.sqBrackets( Parser.sepBy (Parser.name .>> Parser.spaces) pdevicedelim)
let deviceParser = Parser.kw "devices" >>.  
                    (Parser.sqBrackets (Parser.many parse_device)) .>> Parser.spaces



type dnacomponent = 
    | Part of string * partType
    | Device of device

let parse_deviceComponents = Parser.kw "components" >>. sqbracketNoSpace (Parser.sepBy (Parser.name .>> Parser.spaces) pdevicedelim) 


let partParser =  
    Parser.name .>> delimiter >>= fun n ->
        Parser.choice [
            Parser.kw "prom" >>. delimiter >>. (Parser.sepBy parsePromProperties sDelimiter) |>> fun(props) -> Part(n,PROM(props))
            Parser.kw "rbs" >>. delimiter >>. parseRate |>> fun(rate) -> Part(n,RBS(rate))
            Parser.kw "pcr" >>. delimiter >>. parseCodes |>> fun(codes) -> Part(n,PCR(codes))
            pstring "ter" |>> fun (_) -> Part(n, TER)
            Parser.kw "device" >>. delimiter >>. parse_deviceComponents |>> fun (components) -> Device(n,components)
        ]

                    
let fileParser = (Parser.sepBy partParser NEWLINE) 


let parse = fileParser .>> Parser.eof |>> fun(components) -> 
                let partComponents = components |> List.choose 
                                        (fun x -> 
                                            match x with 
                                            | Part(x,y) -> Some(x,y) 
                                            | _ -> None)
                let deviceList = components |> List.choose 
                                        (fun x -> 
                                            match x with 
                                            | Device(x) -> Some(x) 
                                            | _ -> None) 
                                            
                createTable partComponents deviceList
