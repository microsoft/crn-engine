// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.GECEngine

open System
open Microsoft.Research.GEC 
open Microsoft.Research.CRNEngine
//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine
open FSBOL
open FSBOL.JsonSerializer
open FSBOL.SBOLDocument
open FSBOL.TopLevel
open FSBOL.ComponentDefinition
open Microsoft.Research.CRNEngine.InferenceSiteGraph
open Microsoft.Research.GEC.Trans
open Microsoft.Research.GEC.Settings
open Microsoft.Research.GEC.Program
open Microsoft.Research.FSBOLWrapper

(* Main GUI datatype. *)
type t = {
            options:Options.t;
            database:Database.t;
            solution:(Ast.directive list * Main.tSolution * Trans.gecConstraint list * Trans.tArithmeticConstraints * string list) option 
         }
let empty = {options = Options.default_options; database = Database.empty; solution = None}

(******************************************************************************)
(* *** Get/set GUI options. *)

(* Get or set the "options" part of the GUI data structure. *)
let getOptions (gec:t) = gec.options
let setOptions (opts:Options.t) (gec:t) = {gec with options = opts}

(* Get, set or erase the "solution" part of the GUI data structure. *)
let setSolution (ds:Ast.directive list) (sol:Main.tSolution) (prologConstraints:Trans.gecConstraint list) (arithmeticConstraints:Trans.tArithmeticConstraints)
                (log:string list) (gec:t) = {gec with solution = Some (ds, sol,prologConstraints,arithmeticConstraints,log)}
let getSolution (gec:t) = gec.solution
let eraseSolution (gec:t) = {gec with solution = None}

(* Access the parts and reactions database. *)
let getDatabase (gec:t) = gec.database
let setDatabase (db:Database.t) (gec:t) = {gec with database = db}

(* Produce a string representation of the results of the translation process, for debugging purposes... *)
let getDebuggingOutput (g:t) =
  match (getSolution g) with
  | None -> ""
  | Some (ds, sol,prologConstraints,arithmeticConstraints,log) ->
      let bbTemplatesDebug = Lib.string_of_list (fun xs -> "[" + (Lib.string_of_list Lib.id "; " xs) + "]") Lib.newline sol.bbDevices in
      let prologConstraintsDebug = Lib.string_of_list Trans.printConstraint Lib.newline prologConstraints in
      let arithmeticConstraintsDebug = Lib.string_of_list Trans.stringOfArithmeticConstraint Lib.newline arithmeticConstraints in
      let lbsProgDebug = Trans.lbsProgToStr sol.lbsProgram [] in // is empty list OK here for varDefs?
      let rateDecsDebug = Lib.string_of_list (fun (r,f) -> r + " |==> " + (Lib.display_float f)) Lib.newline sol.rateDecs in
      let varAssDebug = sol.getVarAssString() in 
      let databaseDebug = Database.display (getDatabase g) in
      let logDebug = Lib.string_of_list Lib.id Lib.newline log in
      let directivesDebug = match ds with [] -> "" | ds -> Lib.string_of_list Ast.stringOfDirective Lib.newline ds in
      "bbTemplates:" + Lib.newline +
      "============" + Lib.newline +
      bbTemplatesDebug + Lib.newline + Lib.newline +
      "rateDecs:" + Lib.newline +
      "=========" + Lib.newline +
      rateDecsDebug + Lib.newline + Lib.newline +
      "varAss:" + Lib.newline +
      "=======" + Lib.newline +
      varAssDebug + Lib.newline + Lib.newline +
      "Arithmetic constraints:" + Lib.newline +
      "=======================" + Lib.newline +
      arithmeticConstraintsDebug + Lib.newline + Lib.newline +
      "Database:" + Lib.newline +
      "=========" + Lib.newline +
      databaseDebug + Lib.newline + Lib.newline +
      "lbsProg:" + Lib.newline +
      "========" +
      lbsProgDebug + Lib.newline + Lib.newline +
      "directives:" + Lib.newline +
      directivesDebug + Lib.newline + Lib.newline +
      "PROLOG Constraints:" + Lib.newline +
      "============" + Lib.newline +
      prologConstraintsDebug + Lib.newline + Lib.newline +
      "log:" + Lib.newline +
      "====" + Lib.newline +
      logDebug + Lib.newline

(* LBS program text. *)
let getGECProgramText (g:t) = Options.getGECProgramText (getOptions g)
let setGECProgramText (s:string) (g:t) = setOptions (Options.setGECProgramText s (getOptions g)) g

(* Option to enable simulation-only reactions. *)
let getSimulationOnlyReactionsOption (g:t) = Options.getSimulationOnlyReactions (getOptions g)
let setSimulationOnlyReactionsOption (b:bool) (g:t) = setOptions (Options.setSimulationOnlyReactions b (getOptions g)) g

(******************************************************************************)
(* *** Access other information from the datatypes. *) 

(* Have the options changed enough to warrant recompilation? *)
let identicalOptions (gold:t) (gnew:t) =
  ((getGECProgramText gold) = (getGECProgramText gnew)) &&
  ((getSimulationOnlyReactionsOption gold) = (getSimulationOnlyReactionsOption gnew)) &&
  ((getDatabase gold) = (getDatabase gnew))

(* Maybe prefix some directives onto an LBS program. *)
let prefixDirectives (ds:Ast.directive list) (body:string) =
  match ds with
  | [] -> body
  | ds -> (Lib.string_of_list Ast.stringOfDirective Lib.newline ds) + Lib.newline + Lib.newline + body

(* Get the "default" LBS program, i.e. with general reactions (no substitution applied). *)
let getDefaultLBSProgram (g:t) =
  match (getSolution g) with
  | Some(ds,sol,_,_,_) -> Some(prefixDirectives ds (sol.getProgramDefault()))
  | None -> None

(* Get a particular LBS program instance, corresponding to a particular set of variable instantiations. *)
let getLBSProgramInstance (num:int) (g:t) =
  match (getSolution g) with
  | Some(ds,sol,_,_,_) ->
      let ds = List.map (Subst.applyToDirective (List.item num sol.substs)) ds in
      Some(prefixDirectives ds (sol.getProgramInstance(num)))
  | None -> None

(* Get a particular species instance. *)
let getSpeciesAssignment (num:int) (g:t) =
  match (getSolution g) with
  | Some(_,sol,_,_,_) -> sol.getSpecAss(num)
  | None -> ""

(* Get a particular devices instance. *)
let getDevicesInstance (num:int) (g:t) =
  match (getSolution g) with
  | Some(_,sol,_,_,_) -> sol.getDevicesInstance(num)
  | None -> ""

(* Get a particular devices instance as a string list list *)
let getDevicesInstanceStructured (num:int) (g:t) =
  match (getSolution g) with
  | Some(_,sol,_,_,_) -> sol.getDevicesInstanceStructured(num)
  | None -> List.toArray([])

(* Get the number of solutions. *)
let getNumSolutions (g:t) =
  match (getSolution g) with
  | Some(_,sol,_,_,_) -> sol.numSolutions
  | None -> 0

(* Get the directives text (if any). *)
let getDirectives (g:t) =
  match (getSolution g) with
  | Some(ds,_,_,_,_) -> Some ds
  | None -> None

(******************************************************************************)
(* Process a GEC program. *)

let reduce_gecprog (prog:Ast.prog) = 
  let rec reduce  (p:Ast.prog)= 
    match p with 
    | Ast.Par(p1,p2) ->
      let rp1 = reduce p1
      let rp2 = reduce p2
      match rp1 with 
      | Ast.Nil ->
        match rp2 with 
        | Ast.Nil -> Ast.Nil
        | _ -> rp2
      | _ -> 
        match rp2 with 
        | Ast.Nil -> rp1
        | _ -> Ast.Par(rp1,rp2)
    | Ast.Seq(p1,p2) -> 
      let rp1 = reduce p1
      let rp2 = reduce p2
      match rp1 with 
      | Ast.Nil ->
        match rp2 with 
        | Ast.Nil -> Ast.Nil
        | _ -> rp2
      | _ -> 
        match rp2 with 
        | Ast.Nil -> rp1
        | _ -> Ast.Seq(rp1,rp2)  
    | Ast.Comp(c,p1) -> Ast.Comp(c,reduce p1)
    | Ast.New(n,p1) -> Ast.New(n,reduce p1)
    | Ast.TemplateDef(tname,targs,p1,p2) -> Ast.TemplateDef(tname,targs,reduce p1, reduce p2)
    | Ast.Copy(i,p1,b1,b2) -> Ast.Copy(i,reduce p1,b1,b2)        
    | _ -> p
  reduce prog 
  

let unroll_gecprog (gecprog:ClassicProgram) = 
  let template_prog = gecprog.templates
  let rec unroll_templates (prog:Ast.prog) = 
    match prog with 
    | Ast.TemplateDef(tempname,tempargs,body,next) -> 
      match next with 
      | Ast.TemplateDef(_) -> [(tempname,tempargs,body)]@(unroll_templates next)
      | Ast.Nil -> [(tempname,tempargs,body)]
      | _ -> failwith "Top Templates should not have anything other than TemplateDef and Nil in the recursive structure."
    | Ast.Nil -> []
    | _ -> failwith "Top Templates should not have anything other than TemplateDef and Nil in the recursive structure."
  
  let templates = unroll_templates template_prog
  
  let find_key (map) key= 
    map |> List.tryFind(fun (x,y:Ast.abstractComplex) -> x = key)
  
  let rec sub_exp (e:Ast.aexp) (map) =
    match e with 
    | Ast.FloatAExp _ -> e
    | Ast.IdAExp(s) -> 
      match (find_key (map) s) with 
      | Some(mkey,mval_list) -> 
        match mval_list with 
        | [mval] -> 
          match mval with 
          | Ast.IdVal(id) -> Ast.IdAExp(id)
          | Ast.FloatVal(f) -> Ast.FloatAExp(f)
          | Ast.AlgebraicExp(exp) -> exp
          | _ -> failwith "Shouldn't really see a wild card in template invocation?"
        | _ -> failwith "Improper Template Invocation"
      | None -> e
    | Ast.PlusAExp(p1,p2) -> Ast.PlusAExp(sub_exp p1 map,sub_exp p2 map)
    | Ast.MinusAExp(p1,p2) -> Ast.MinusAExp(sub_exp p1 map,sub_exp p2 map)
    | Ast.MulAExp(p1,p2) -> Ast.MulAExp(sub_exp p1 map,sub_exp p2 map)
    | Ast.DivAExp(p1,p2) -> Ast.DivAExp(sub_exp p1 map,sub_exp p2 map)
    | Ast.PowAExp(p1,p2) -> Ast.PowAExp(sub_exp p1 map,sub_exp p2 map)
  
  let sub_value (v:Ast.value) (map) = 
    match v with 
    | Ast.IdVal (x) -> 
      match (find_key map x) with 
      | Some(mkey,mval_list) -> 
        match mval_list with 
        | [mval] -> mval
        | _ -> failwith "Improper Template Invocation"
      | None -> v
    | Ast.FloatVal f -> v
    | Ast.WildCardVal -> v
    | Ast.AlgebraicExp exp -> Ast.AlgebraicExp(sub_exp exp map)
  
  let sub_abstractcomplex (ac:Ast.abstractComplex) (map) =
    match ac with 
    | [Ast.IdVal(id)] -> 
      match (find_key map id) with 
      | Some(mkey,mval) -> mval 
      | None -> ac
    | _ -> ac

  let sub_string (s:string) (map) = 
    match (find_key map s) with 
    | Some(mkey,mval) -> 
      match mval with 
      | [Ast.IdVal(id)] -> id
      | _ -> failwith "Improper Template Invocation - Compartment name"
    | None -> s

  let rec sub_prog (prog:Ast.prog) (map) =
    match prog with
    | Ast.Brick(v,btype,props) -> 
      let sv = sub_value v map
      let sprops = 
        props |> List.map (fun (x,y) -> 
          let sy = y |> List.map (fun z -> 
            match z with 
            | [Ast.IdVal(id)] -> 
              match (find_key (map) id) with 
              | Some(mkey,mval) -> mval
              | None -> z              
            | _ -> z)
          (x,sy))
      Ast.Brick(sv,btype,sprops)
    | Ast.Reac(enz,reac,prod,rate,sim) ->
      let senz = enz |> List.map (fun x -> sub_abstractcomplex x map)
      let sreac = reac |> List.map (fun x -> sub_abstractcomplex x map)
      let sprod = prod |> List.map (fun x -> sub_abstractcomplex x map)
      Ast.Reac(senz,sreac,sprod,sub_value rate map,sim)
    | Ast.Trans(reac,prod,comp,rate,sim,dir) -> Ast.Trans(sub_abstractcomplex reac map,sub_abstractcomplex prod map,sub_string comp map,sub_value rate map,sim,dir)
    | Ast.TemplateInv(tname,targs) -> //This maybe a corner case
      let sargs = targs |>  List.map (fun x -> sub_abstractcomplex x map) 
      Ast.TemplateInv(tname,sargs)
    | Ast.Seq(s1,s2) -> Ast.Seq(sub_prog s1 map,sub_prog s2 map)
    | Ast.Par(s1,s2) -> Ast.Par(sub_prog s1 map,sub_prog s2 map)
    | Ast.Comp(s,cprog) -> Ast.Comp(sub_string s map,sub_prog cprog map)
    | Ast.New(n,nprog) -> Ast.New(sub_string n map,sub_prog nprog map)
    | Ast.Constraint(c1,op,c2) -> Ast.Constraint(sub_exp c1 map,op,sub_exp c2 map)
    | Ast.Rate(v,rate) -> Ast.Rate(sub_value v map,rate)
    | Ast.Copy(i,cprog,b1,b2) -> Ast.Copy(i,sub_prog cprog map,b1,b2)
    | _ -> prog
 
  let rec unroll_prog (prog:Ast.prog) (templates)= 
    match prog with 
    | Ast.TemplateInv(tempname,args) -> 
      match (templates |> List.tryFind (fun(x,y,z) -> x = tempname)) with 
      | Some(tname,targs,tprog) -> 
        let subs = List.zip targs args
        let sprog = sub_prog tprog subs
        unroll_prog sprog templates
      | None -> 
        failwith ("Template " + tempname + " not defined")
    | Ast.TemplateDef(name,args,body,next) -> 
      let unroll_body = unroll_prog body templates //This maybe optional?
      unroll_prog next ((name,args,unroll_body)::templates)
    | Ast.Seq(s1,s2) -> 
      let u_s1 = unroll_prog s1 templates
      let u_s2 = unroll_prog s2 templates
      Ast.Seq(u_s1,u_s2)
    | Ast.Par(p1,p2) -> 
      let u_p1 = unroll_prog p1 templates
      let u_p2 = unroll_prog p2 templates
      Ast.Par(u_p1,u_p2)
    | Ast.Comp (name,prog) -> 
      let u_prog = unroll_prog prog templates
      Ast.Comp(name,u_prog)
    | Ast.New(n,prog) -> 
      let u_prog = unroll_prog prog templates
      Ast.New(n,u_prog)
    | Ast.Copy(i,prog,b1,b2) -> 
       let u_prog = unroll_prog prog templates
       Ast.Copy(i,u_prog,b1,b2)
    | _ -> prog
      
  let unrolled_top = reduce_gecprog (unroll_prog gecprog.prog templates)
  let unrolled_systems = gecprog.systems |> List.map(fun x -> {x with prog = reduce_gecprog (unroll_prog x.prog templates)})
  {gecprog with prog = unrolled_top; systems = unrolled_systems}
   

let translate_systems_to_prog (gecprog:ClassicProgram) = 
  let systems = gecprog.systems |> List.map (fun x -> Ast.Comp(x.name,x.prog))
  let parsystems = Program.fold_parallel systems
  let top_prog = gecprog.prog
  
  match parsystems with 
  | Ast.Nil -> top_prog
  | _ -> 
    match top_prog with 
    | Ast.Nil -> parsystems
    | _ -> Ast.Par(top_prog,parsystems)



exception CompileException of string * exn

let modify_crn (crn:Crn) (settings:Gec_settings) crnSettings=
  let instructions = settings.crn
  let overrideCrn = settings.overrideCrn
  if overrideCrn then 
    (None,instructions)
    //Crn.create_from_instructions crnSettings instructions
  else
    Some(crn),instructions
    (*let basecrn = {crn with settings=crnSettings}.saturate_initials()
    let instCrn = Crn.create_from_instructions crnSettings instructions
    let reactions' = basecrn.reactions@instCrn.reactions
    //let attributes = instCrn.attributes 
    let attributes' = instCrn.attributes
                      |> Map.toList       
                      |> List.fold (fun (acc:Stringmap.t<Attributes>) (x,y) -> acc.Add(x,y)) basecrn.attributes  
    let initials' = basecrn.initials@instCrn.initials
    {basecrn with reactions=reactions';attributes=attributes';initials=initials'}*)     

type solve_result = { solution : t
                    ; graph : InferenceSiteGraph.IGraph
                    ; sbol : SBOLDocument 
                    ; crnString: string}

let getLBSSystems (gecprog:ClassicProgram) (lbs:tLBSProg) = 
  let systems = gecprog.systems
  let system_names = systems |> List.map (fun x -> x.name)

  let rec lbsSystem lbs = 
    match lbs with 
    | LBSComp(comp,prog) -> 
      if (system_names |> List.contains comp) then 
        [lbs]
      else 
        []
    | LBSPar(p1,p2) -> 
      (lbsSystem p1)@(lbsSystem p2)
    | LBSReacAbstraction(_,prog) -> lbsSystem prog
    | LBSCompDec(comp,prog) ->  lbsSystem prog
    | LBSCopy(i,prog) -> lbsSystem prog
    | _ -> []
  
  let rec lbsTop lbs = 
    match lbs with 
    | LBSComp(comp,prog) -> 
      if (system_names |> List.contains comp) then 
        Trans.LBSNil
      else 
        lbs
    | LBSPar(p1,p2) -> 
      LBSPar(lbsTop p1,lbsTop p2)
    | LBSReacAbstraction(x,prog) -> LBSReacAbstraction(x,lbsTop prog)
    | LBSCompDec(comp,prog) ->  LBSCompDec(comp,lbsTop prog)
    | LBSCopy(i,prog) -> LBSCopy(i,lbsTop prog)
    | _ -> lbs

  (lbsTop lbs,lbsSystem lbs)

let create_inference_graph (crnSettings:Crn_settings<Functional>) (gecprog:ClassicProgram) (top_crn:(Crn option * crnInstructions)) (system_crn_map) (db)= 
  let dir_str = Program.crnSettings_to_string crnSettings
  let modules_str = Program.modules_to_string crnSettings.simulation.initial gecprog.modules
  let devices_str = Lib.string_of_list (fun x -> Program.deviceDefinition_to_string x) "\n" gecprog.devices
  let top_prog_str = Program.crn_contents_to_string crnSettings.simulation.initial top_crn
  
  let system_strings = gecprog.systems 
                       |> List.map (fun sys -> 
                         match (system_crn_map |> List.tryFind(fun (x,crn) -> x = sys.name)) with 
                         | Some(_,crn) -> Program.system_to_string crn gecprog.modules gecprog.devices sys db
                         | None -> failwith ("Unexpected error. System name not found in to_string method for: " + sys.name)
                       )
  let system_str = Lib.string_of_list (fun x -> x) "\n" system_strings 
  
  let ig_str = 
    match gecprog.graph with 
    | [] -> 
      match system_crn_map with 
      | [] -> "" //"node node0 { }"
      | _ -> 
        let system_names = system_crn_map |> List.map (fun (x,y) -> x)
        "node node0 { systems = [" + (Lib.string_of_list (fun x -> x) ";" system_names) + "] }"
    | _ -> Lib.string_of_list (fun x -> Program.igElement_to_string x) "\n" gecprog.graph
  
  
  dir_str + modules_str + devices_str + top_prog_str + system_str + ig_str


let getSBOLAssignment (bbTemplates:Trans.tBbDevices) (table:Database.t) (substs:Subst.t list) (index:int)= 
    let assignment = substs.Item(index)
    
    let findPart (id:string) =
        match Stringmap.tryFind id table.parts with
        | Some entry -> Some entry.value
        | None -> None
    
    let IdExists (id:string) = 
        match findPart id with 
        | Some(_) -> true
        | None -> false

    let createTUAssignment (tu:string list) = 
        let createCDAssignment (p:string) = 
            let partName = 
                match (IdExists p) with 
                | true ->
                    p
                | false ->
                    match assignment.Item(p) with 
                    | Subst.PART(partVal) -> partVal
                    | _ -> failwith "Can't find the part in the substitution. Some error occurred."
            let part = 
                match Stringmap.tryFind partName table.parts with
                | Some entry -> entry
                | None -> failwith "Can't find the part in the database. Some error occurred."
               
            let cd = Database.partTypeToSBOL partName part
            (cd,(partName,part))
        let cdList = tu |> List.map (fun x -> (createCDAssignment x))
        cdList
    let urlPrefix = "http://www.microsoft.com/gec/db"
    let tuList = [0..(bbTemplates.Length-1)] |> 
                    List.map (fun indx -> 
                        let a = (createTUAssignment (bbTemplates.Item(indx)))
                        let cdList = a |> List.map (fun (x,y) -> x)
                        let partEntryList = a |> List.map (fun(x,y) -> y)
                        let tu =
                            let tuName = ("tu" + indx.ToString())
                            let perId = urlPrefix + "/" + tuName
                            let version = "1"
                            GECHelper.createHigherFunction tuName perId version cdList
                        (cdList,tu,partEntryList)
                        )
    let partCDList = tuList |> List.map(fun (x,y,z) -> x) 
    let partCDs = match partCDList.Length with 
                  | 0 -> []
                  | 1 -> partCDList.Head
                  | _ -> partCDList |> List.reduce (fun a b -> a@b)

    let tuCDs = tuList |> List.map(fun (x,y,z) -> y)
    
    let partsUsedList = 
        let a = tuList |> List.map (fun (x,y,z) -> z)
        match a.Length with 
        | 0 -> []
        | 1 -> a.Head
        | _ -> a |> List.reduce (fun b c  -> b@c)
    
    
    let partIdList = partsUsedList |> List.map(fun (x,y) -> x) |> Seq.ofList |> List.ofSeq
    let partEntryList = 
        partIdList |> List.map(fun (x) -> 
            partsUsedList |> List.find (fun (a,b) -> a=x)
            )
    
    (*let protList = partEntryList 
                   |> List.map (fun (x,y) -> y) 
                   |> List.map (fun entry -> entry.value)
                   |> List.filter (fun partEntry -> 
                        match partEntry with 
                        | Database.PCR(_) -> true
                        | _ -> false
                        )
                   |> List.map (fun (Database.PCR(x))-> x)*)

    let protCDs = Database.createProteinCDs partEntryList

    let mdList = Database.createModuleDefinitions (partCDs@protCDs) partEntryList //|> List.map (fun x -> TopLevel.ModuleDefinition(x))
    
    let device = 
        let name = "device"
        let perid = urlPrefix + "/" + name
        let version = "1"
        //GECHelper.createHigherFunction name perid version tuCDs
        GECHelper.createHigherFunction name perid version partCDs

    //let allCDs = List.rev(device::( List.rev(tuCDs) @ List.rev(partCDs@protCDs))) 
    let allCDs = List.rev(device::( (*List.rev(tuCDs) @*) List.rev(partCDs@protCDs))) 

    let s = SBOLDocument(
             (allCDs |> List.map (fun x -> x :> TopLevel))
             //@ (mdList  |> List.map (fun x -> x :> TopLevel))
             )
    s


let solveGEC (cancel_flag:bool ref) (program:string) (dbParts:string) (dbReactions:string) : solve_result =
    let db_from_string (s:string) = Parser.from_string Database.parse s
    let partstable =
      try
        db_from_string dbParts
      with e ->
        raise (CompileException ("parts", e))

    let reactionListParser = Parser.sepBy Gecreaction.parseReaction Parser.newline
    let reactiondb_from_string (s:string) = Parser.from_string reactionListParser s

    let createReactionEntry reaction = 
        let (reactionEntry:Gecreaction.t Database.entry) ={value=reaction;enabled=true;comments=""}
        reactionEntry

    let reactiondb =
      try
        reactiondb_from_string dbReactions |> List.map (fun(x) -> createReactionEntry(x))
      with e ->
        raise (CompileException ("reactions", e))

    try
      let table = {partstable with reactions = reactiondb}
      let options = Options.setGECProgramText program Options.default_options 
      let guioptions = setOptions options empty 
      let gui = setDatabase table guioptions
      match Main.parse (getGECProgramText gui) with 
      | LogicGec _ -> failwith "Logic GEC program not supported yet."
      | ClassicGec gecprog ->
        let ugecprog = unroll_gecprog gecprog
        let prog = translate_systems_to_prog ugecprog
        let ds = ugecprog.settings.directives |> Settings.convert_crn_to_gec_directives
        let (bbTemplates, prologConstraints, lbsProg, rateDecs, substitutions, arithmeticConstraints, log) =
            Trans.translate0 prog (getSimulationOnlyReactionsOption gui) (getDatabase gui)
        
        let varAss = List.map Cssubst.mkVarAss substitutions 
        let substs = List.map Cssubst.getSubst substitutions 
        (* Put the results into a Main.tSolution data structure. *)
        let sol = { Main.bbDevices    = bbTemplates;
                    Main.lbsProgram   = lbsProg;
                    Main.rateDecs     = rateDecs;
                    Main.varAss       = varAss;
                    Main.substs       = substs;
                    Main.error        = None;
                    Main.numSolutions = List.length varAss }
        
        let solution = setSolution ds sol prologConstraints arithmeticConstraints log gui
        
        
        let (toplbs,lbsSystems) = getLBSSystems ugecprog lbsProg
        
        let crnSettings = Crn_settings.defaults.from_default_directive_list ugecprog.settings.directives  
        
        let top_crn = TransCrn.create toplbs |> (fun x -> modify_crn x ugecprog.settings crnSettings)
        let system_crn_map = lbsSystems 
                          |> List.map (fun x ->                           
                            let systems  = ugecprog.systems
                            match x with 
                            | Trans.LBSComp(compname,prog) -> 
                              let crn = TransCrn.create x
                              match (systems |> List.tryFind (fun y -> y.name = compname)) with 
                              | Some(res) -> (compname,modify_crn crn res.settings crnSettings)
                              | None -> failwith ("Unexpected error. System " + compname + " not found.")
                            | _ -> failwith "Unexpected LBS Comparment encountered")
        
        let crnString = create_inference_graph crnSettings ugecprog top_crn system_crn_map table
        let igraph = Parser.from_string InferenceSiteGraph.parse crnString
        
        let (varAss',subst') = 
          match sol.numSolutions with 
          | 0 ->
            match igraph.nodes.Count with 
            | 0 -> failwith "This should never happen"
            | 1 ->  
              let model = igraph.nodes |> Map.toSeq |> Seq.head |> snd
              match (model.top.initials.IsEmpty) && (model.systems.IsEmpty) with 
              | true -> ([],[])
              | false -> ([[],[],[]],[Map.empty])
            | _ -> ([[],[],[]],[Map.empty])
          | _ -> (sol.varAss,sol.substs)
          
        let solution' = 
          match solution.solution with 
          | Some(a,sol,b,c,d) -> 
              let sol' = {sol with Main.varAss = varAss'; Main.numSolutions = varAss'.Length; Main.substs = subst'}
              Some(a,sol',b,c,d)
          | None -> None
        
        
        let sbol = 
          match solution' with
          | Some(dir,sol,gecConst,arthConst,_) -> getSBOLAssignment sol.bbDevices table sol.substs 0
          | None -> Database.convertTableToSBOLDocument table
        { solution = {solution with solution = solution'} 
        ; graph = igraph
        ; sbol = sbol 
        ; crnString = crnString}


    with e ->
      raise (CompileException ("code", e))

(******************************************************************************)

let rec evaluateExpression (exp:Expression.t<_>) (smap:Subst.t) = 
    match exp with 
    | Expression.Key(key) -> 
        if key = "RMRNADeg" then 
            0.001
        else
            if not (smap.ContainsKey(key)) then 
                try 
                    float key
                with e ->
                    failwith "Key not found in substitution"
                    raise(CompileException ("code", e))
            else 
                match smap.Item(key) with 
                    | Subst.NUMBER(fl) -> fl
                    | _ -> failwith "Unexpected format in the map"
    | Expression.Float(fl) -> fl
    | Expression.Times(times) -> 
        times |> List.fold (fun acc x -> acc*(evaluateExpression x smap)) 1.0
    | Expression.Divide(divide) -> 
        let d1 = evaluateExpression divide.div1 smap
        let d2 = evaluateExpression divide.div2 smap
        if d2 = 0.0 then 
            failwith "Divide by 0 error."
        (d1)/(d2)
    | Expression.Power(pow) -> 
        let b = evaluateExpression pow.base_ smap
        let pow = evaluateExpression pow.exponent smap
        Math.Pow(b,pow)
    | Expression.Plus(plus) -> 
        plus |> List.fold (fun acc x -> acc+(evaluateExpression x smap)) 0.0
    | Expression.Minus(minus) -> 
        let sub1 = evaluateExpression minus.sub1 smap
        let sub2 = evaluateExpression minus.sub2 smap
        sub1 - sub2
    | Expression.Absolute(abs) ->
        let vabs = evaluateExpression abs smap
        if vabs < 0.0 then
            (vabs * (-1.0))
        else 
            vabs
    | Expression.Log(log) ->
        let l = evaluateExpression log smap 
        Math.Log(l)
    | Expression.Modulo(modulo) -> 
        let div = evaluateExpression modulo.div smap
        let modul = evaluateExpression modulo.modulo smap
        div%modul
    | Expression.If(bexp1,bexp2,bexp3) -> 
        failwith "unexpected expression type"


let assignReverseRate (rxn:Reaction<Species,Value,Functional>) (smap:Subst.t) = 
    
    match rxn.reverse with 
    | Some(Rate.MassAction(exp:Expression.t<string>)) -> 
        match exp with 
        | Expression.Key(rate) -> 
            if rate = "RMRNADeg" then 
                {rxn with reverse = Some(Rate.MassAction(Expression.Float(0.001)))}
            else 
                if not (smap.ContainsKey(rate)) then 
                    let mfloat = ref 0.0f
                    match Single.TryParse(rate, mfloat) with 
                    | true -> {rxn with reverse = Some(Rate.MassAction (Expression.Float(float !mfloat)))}
                    | false -> rxn
                    (*try 
                        let mfloat = float rate
                        {rxn with reverse = Some(Rate.MassAction (Expression.Float(mfloat)))}
                    with e ->
                        failwith "Key not found in substitution"
                        raise(CompileException ("code", e))*)
                else 
                    match smap.Item(rate) with 
                    | Subst.NUMBER(fl) -> 
                      {rxn with reverse = Some(Rate.MassAction(Expression.Float(fl)))}
                    | _ -> failwith "Unexpected format in the map"

        | _ -> 
            try 
                let value = evaluateExpression exp smap
                {rxn with reverse = Some(Rate.MassAction(Expression.Float(value)))}
            with e -> 
                raise(CompileException ("code", e))
    | Some(Rate.Function(e)) -> rxn //This case is encountered when userdefined CRNs are substituted.
    | None -> rxn

    
let assignReaction (rxn:Reaction<Species,Value,Functional>) (smap:Subst.t) =
    match rxn.rate with 
    | Rate.MassAction(exp:Expression.t<string>) -> 
        match exp with 
        | Expression.Key(rate) -> 
            if rate = "RMRNADeg" then 
                let newRxn = {rxn with rate = (Rate.MassAction (Expression.Float(0.001)))}
                assignReverseRate newRxn smap
            else 
                if not (smap.ContainsKey(rate)) then
                    let mfloat = ref 0.0f
                    match Single.TryParse(rate, mfloat) with 
                    | true -> 
                        let newRxn = {rxn with reverse = Some(Rate.MassAction (Expression.Float(float !mfloat)))}
                        assignReverseRate newRxn smap
                    | false -> rxn
                    
                    (*try 
                        let mfloat = float rate
                        let newRxn = {rxn with rate = (Rate.MassAction (Expression.Float(mfloat)))}
                        assignReverseRate newRxn smap
                    with e ->
                        failwith "Key not found in substitution"
                        raise(CompileException ("code", e))*)
                else 
                    let newRate = match smap.Item(rate) with 
                                  | Subst.NUMBER(fl) -> fl
                                  | _ -> failwith "Unexpected format in the map"
                        
                    let newRxn = {rxn with rate = (Rate.MassAction (Expression.Float(newRate)))}
                    assignReverseRate newRxn smap
        | _ -> 
            try 
                let value = evaluateExpression exp smap
                let newRxn = {rxn with rate = (Rate.MassAction(Expression.Float(value)))}
                assignReverseRate newRxn smap
            with e -> 
                raise(CompileException ("code", e))
    | Rate.Function(e) -> rxn //This case is encountered when userdefined CRNs are substituted.

    
type solution_result = { model : InferenceSiteGraph.IGraph
                       ; sbol : SBOLDocument }

let getCrnAssignment (igraph:InferenceSiteGraph.IGraph) (gecSol:t) (index:int) : solution_result = 
    if gecSol.solution.IsNone then 
        failwith "No solution found"

    let (dirlist,sol,constraints,aritconstraints,d) = gecSol.solution.Value
    let substitution = sol.substs.Item(index)
    
    //SBOL
    let sbol = getSBOLAssignment sol.bbDevices gecSol.database sol.substs index
    
    //CRN
    let assign_crn (crn:Crn) = 
        let assignedReactions = crn.reactions |> List.map(fun x -> assignReaction x substitution)
        {crn with reactions = assignedReactions}
    
    let nodes' = igraph.nodes |> Map.map (fun k node -> 
        {node with top = (assign_crn node.top); systems = (node.systems |> List.map (fun x -> assign_crn x))})

    
    { model = {igraph with nodes = nodes'} ; sbol = sbol }
        