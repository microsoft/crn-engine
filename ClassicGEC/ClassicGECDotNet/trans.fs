(*
Defines the main translation function for LSB programs.

Author: Michael Pedersen.
Copyright © Microsoft Research, 2008-2009.
*)

#light


[<JavaScript>]
module Microsoft.Research.GEC.Trans

open System
open Microsoft.Research.GEC.Ast
open System.Text.RegularExpressions
//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine


// define the type of lbs programs that we translate to:
type tLBSProg =
    | LBSReacAbstraction of tLBSReac * tLBSProg // needed for promoters which are possibly characterised by hill functions, but this is only known after a substitution is applied.
    | LBSReac of tLBSReac 
    | LBSDegReac of string list list * string // needed in order to put degradation reactions in all compartments.
    | LBSTrans of (string list) * (string list) * string * string * direction
    | LBSComp of string * tLBSProg
    | LBSPar of tLBSProg * tLBSProg
    | LBSInitPop of string list * float
    | LBSCompDec of string * tLBSProg
    | LBSCopy of int * tLBSProg
    | LBSNil
    | LBSDevice of string

// a reactions consists of enzymes; reactants; products; a rate; and a boolean indicating if the rate is mass-action:
and tLBSReac = string list list * string list list * string list list * string * bool

 
// lets now define the type of return objects.
// first there are "translation functions" -- these are used to
// produce reactions representing the genetic translation process.
// the first function is parameterised on both mrna and the target protein (complex).
// the following two functions are parameterised on only one of these.
type tTranslFunc  = (string -> string list -> tLBSProg) list
type tTranslFunc1  = (string -> tLBSProg) list
type tTranslFunc2  = (string list -> tLBSProg) list

// for the translation to reactions we also need to keep track of the "current"
// mrna and target protein -- these will be passed to the above functions when appropriate.
// they are lists in order to cope with parallel composition, but only
// singleton/empty lists are supported by the sequential composition operation for now.
type tCurrentMRNA = string list
type tCurrentProt = string list list

// rate declarations:
type tRateDecs    = (string * float) list

// now to the targets of the genetic translation.
// first we have a 2d list of brick ids/vars:
type tBbDevices   = string list list



// Workaround for issues with TextBox in Silverlight
let newline = "\n"

// the following family converts translated structures to prolog representations:
let lstToProlog absComp =
    "[" + (Lib.string_of_list Lib.id "," absComp) + "]"

let lst2dToProlog absCompLst =
    let absCompLst' = absCompLst |> List.map (fun absComp -> lstToProlog absComp)
    "[" + (Lib.string_of_list Lib.id "," absCompLst') + "]"

let propToProlog p =
  match p with
  | Some (pname, absCompLst) ->
    // note that we can't use lst2dToProlog for the body of the properties, since
    // the outer brackets of the 2d list shouldn't be included in this case.
    let lstStr = absCompLst |> List.map (fun absComp -> lstToProlog absComp)
    pname + "(" + (Lib.string_of_list Lib.id "," lstStr) + ")"
  | None -> "_"

(* These are the only kinds of constraint that are produced by the translation... *)
type gecConstraint = IS_EMPTY_LIST of string
                   | PROM of string * (string * (string list list)) option
                   | RBS of string * (string * (string list list)) option
                   | PCR of string * (string * (string list list)) option
                   | TER of string * (string * (string list list)) option
                   | UNION of string * string * string // These always involve VARIABLES
                   | EXCLUSIVE_NAMES of string * string * string list list * string // These always go VARIABLE-VARIABLE-LITERAL-VARIABLE
                   | NAMES_DISJOINT of string list list * string * string list list * string // These always go LITERAL-VARIABLE-LITERAL-VARIABLE
                   | NO_DUPLICATES of string list // These always involve LIST LITERALS
                   | ARITHMETIC of aexp * Ast.op * aexp
                   | REACTION of string list list * string list list * string list list * string
                   | TRANSPORT of string list * string list * Ast.direction * string

(* Produce a string representation of a constraint. *)
let printConstraint (c:gecConstraint) =
  let printClause (r:string) (xs:string list) =
    r + "(" + (Lib.string_of_list Lib.id "," xs) + ")"
  in
  match c with
  | IS_EMPTY_LIST(x) -> x + "=[]"
  | PROM(x,y) -> printClause "prom" [x;(propToProlog y)]
  | RBS(x,y) -> printClause "rbs" [x;(propToProlog y)]
  | PCR(x,y) -> printClause "pcr" [x;(propToProlog y)]
  | TER(x,y) -> printClause "ter" [x;(propToProlog y)]
  | UNION(x,y,z) -> printClause "union" [x;y;z]
  | EXCLUSIVE_NAMES(w,x,y,z) -> printClause "exclusiveNames" [w;x;(lst2dToProlog y);z]
  | NAMES_DISJOINT(w,x,y,z) -> printClause "namesDisjoint" [(lst2dToProlog w);x;(lst2dToProlog y);z]
  | NO_DUPLICATES(x) -> printClause "noDuplicates" [lstToProlog x]
  | ARITHMETIC(x,op,y) -> (Ast.stringOfAExp x) + (match op with Ast.Lt -> "<" | Ast.Eq -> "=" | Ast.Gt -> ">") + (Ast.stringOfAExp y)
  | REACTION(es,rs,ps,r) -> printClause "reac" [(lst2dToProlog es);(lst2dToProlog rs);(lst2dToProlog ps);(lstToProlog [r])]
  | TRANSPORT(x,y,Ast.In,r) -> printClause "transport" [(lstToProlog x); (printClause "compartment" [lstToProlog y]); (lstToProlog [r])]
  | TRANSPORT(x,y,Ast.Out,r) -> printClause "transport" [(printClause "compartment" [lstToProlog x]); (lstToProlog y); (lstToProlog [r])]

(* Given a brick type, return a constructor for the gecConstraint type. *)
let getConstraintConstructor (t:string) =
  match t with
  | "prom" -> PROM
  | "rbs" -> RBS
  | "pcr" -> PCR
  | "ter" -> TER
  | _ -> failwith ("\nType error: unexpected brick type: " + t + "\n")

// a variable which in Plolog will unify with a list of exclusive species:
type tExcSpecVar  = string

// a list of species names occurring in a given program:
type tSpecNames   = string list list

// a list of constraints for PROLOG: (NB - delete these soon??)
type tPrologConstraints = gecConstraint list
//type tPrologConstraints = string list

// a type for arithmetic constraints collected for solving at the end
type tArithmeticConstraints = (aexp * op * aexp) list
let stringOfArithmeticConstraint ((a1,op,a2):aexp*op*aexp) =
  (Ast.stringOfAExp a1) + " " + (stringOfOp op) + " " + (Ast.stringOfAExp a2)

// and a set of context-sensitive substitutions.
type tSubstitutions = Cssubst.t list

// all of the above are collected into a semantic object record:
type tSemObj = {
                translFunc : tTranslFunc; 
                translFunc1 : tTranslFunc1; 
                translFunc2 : tTranslFunc2; 
                currentMRNA : tCurrentMRNA;
                currentProt : tCurrentProt;
                bbDevices : tBbDevices; 
                prologConstraints : tPrologConstraints;
                arithmeticConstraints : tArithmeticConstraints;
                substitutions : tSubstitutions;
                exclusiveSpecVar : tExcSpecVar;
                specNames : tSpecNames;             
                lbsProg : tLBSProg;
                rateDefs : tRateDecs;
                log : string list
             }

// now to the definition of environment types (a parameter to the translation function).
// first define the type of substitutions (formal/actual pars) and module environments:
type tsubst = Map<string, string list>
type tenvm  = Map<string, string list list -> tSemObj>

// collect the latter two into an environment record for convenience --
// also add simulation-only flag:
type tenv   = {envm : tenvm; subst : tsubst; simOnlyReacs : bool}

// a closure for generating unique numbers (from Expert F# p. 75):
let private newStr = // ref (fun () -> "Fixme")
  let count = ref 0
  (fun () -> count := !count + 1; string !count)

// Reset the "newStr" counter, so we start numbering from scratch each time we compile
//let resetCounter () =
//  let count = ref 0
//  newStr := (fun () -> count := !count + 1; Int32.to_string !count)



// use the above for generating new variables:
let rec newVar(existing: string list) = 
    let a  = "X_" + newStr() 
    if List.contains(a) existing then
        newVar(existing)
    else
        a 
        

// define an empty semantic object:


   
// Note that mRNA degradation rates do not associate naturally with any part.
// So for now, we use a globally defined variable with the following default value.
let RMRNADeg_string = "RMRNADeg"  
let default_RMRNADeg = 0.001

// Expand a set of rateDecs to include the default RMRNADeg string, if necessary
let expandRateDecs (rateDecs:tRateDecs) : tRateDecs = if (List.exists (fun (x,_) -> x=RMRNADeg_string) rateDecs) then rateDecs
                                                      else (RMRNADeg_string, default_RMRNADeg)::rateDecs

// Turn a set of rateDecs into a substitution
let substOfRateDecs (rateDecs:tRateDecs) : Subst.t = Lib.fold_left (fun theta (r,f) -> Stringmap.add r (Subst.NUMBER f) theta) Subst.empty rateDecs

// Apply a set of rateDecs to an arithmetic constraint
let applyRateDecs2ArithmeticConstraints (rateDecs:tRateDecs) (acs:tArithmeticConstraints) : tArithmeticConstraints =
  let theta = substOfRateDecs rateDecs in
  List.map (fun (a1,op,a2) -> ((Subst.applyToArithmeticExpression theta a1),op,(Subst.applyToArithmeticExpression theta a2))) acs

// flag for ignoring consistency constraints:
let ignoreConsistencyConstraints = ref false
   
// flag specifying whether constants or variables should be used when introducing 
// e.g. new facts in constraints; the former is for translation of human-readable
// databases to prolog databases:
let useConstants = ref false

let rec findExpVars (exp:aexp) = 
    match exp with 
    | FloatAExp(_) -> []
    | IdAExp(str) -> [str]
    | PlusAExp(e1,e2) -> 
        (findExpVars e1)@(findExpVars e2)
    | MinusAExp(e1,e2) -> 
        (findExpVars e1)@(findExpVars e2)
    | MulAExp(e1,e2) -> 
        (findExpVars e1)@(findExpVars e2)
    | DivAExp(e1,e2) -> 
        (findExpVars e1)@(findExpVars e2)
    | PowAExp(e1,e2) -> 
        (findExpVars e1)@(findExpVars e2)
        

let findValueExp (v:Ast.value) = 
    match v with 
    | IdVal(str) -> [str]
    | AlgebraicExp(exp) -> findExpVars exp
    | _ -> []

let findAbstractComplexExp (a:Ast.abstractComplex) = 
    let alist = a |> List.map(fun (value) -> findValueExp value) 
    
    match alist.Length with 
    | 0  -> []
    | 1 ->  alist.Head
    | _ -> alist |> List.reduce (fun a b -> (a@b))
   

let findAbstractComplexListExp (a: Ast.abstractComplex list) = 
    let alist = a|>  List.map(fun x -> findAbstractComplexExp x)
    match alist.Length with 
    | 0  -> []
    | 1 ->  alist.Head
    | _ -> alist |> List.reduce (fun a b -> (a@b))

let findPropExp (p:Ast.prop) = 
    let (str,acomplex) = p
    let alist = acomplex |> List.map(fun x -> findAbstractComplexExp x) 
    match alist.Length with 
    | 0  -> []
    | 1 ->  alist.Head
    | _ -> alist |> List.reduce (fun a b -> (a@b))

let findPropListExp (propList:Ast.prop list) = 
    let alist = propList |> List.map(fun x-> findPropExp x)
    match alist.Length with 
    | 0  -> []
    | 1 ->  alist.Head
    | _ -> alist |> List.reduce (fun a b -> (a@b))

let rec findExistingVariables (program:prog) = 
    match program with 
    | Nil -> []
    | Brick(brickId,_,propList) -> 
        (findValueExp brickId)@(findPropListExp propList)
    | Reac(enz,react,prod,v,_) -> 
        (findAbstractComplexListExp enz) @
        (findAbstractComplexListExp react) @
        (findAbstractComplexListExp prod) @
        (findValueExp v)
    | Trans (react,prod,str,v,_,_) -> 
        str ::
        (findAbstractComplexExp react) @
        (findAbstractComplexExp prod) @
        (findValueExp v)
    | TemplateInv (str,args) -> 
        str ::
        (findAbstractComplexListExp args) 
    | Seq(s1,s2) -> 
        (findExistingVariables s1)@(findExistingVariables s2)
    | Par(p1,p2) -> 
        (findExistingVariables p1)@(findExistingVariables p2)
    | Comp(comp,p) -> 
        comp::(findExistingVariables p)
    | New(var,p) -> 
        var::(findExistingVariables p)
    | TemplateDef(modName,args,p1,p2) -> 
        modName::args@(findExistingVariables p1)@(findExistingVariables p2)
    | Constraint (exp1,_,exp2) -> 
        (findExpVars exp1)@(findExpVars exp2)
    | Ast.Rate(value,_) -> findValueExp value
    | InitPop(var,_) ->  findAbstractComplexExp var
    | Copy (_,p,_,_) -> findExistingVariables p
    | Device (d) -> [d]
        
// top-level translation function.
// parameterised on a programme, and a boolean flag indicating simulation only reactions.
//let translate_base (p:prog) (simOnlyReacs:bool) (db:Database.t) (mrnadeg:float) =
let rec translate0 (p:prog) (simOnlyReacs:bool) (db:Database.t) =
      // create empty environments:
      let subst = Map.empty : tsubst
      let envm  = Map.empty : tenvm
      let env   = { subst = subst; envm = envm; simOnlyReacs = simOnlyReacs }
      let existing = (findExistingVariables p) |> Set.ofList |> List.ofSeq
      //Check in Database and GECReaction as well?
      let excNv = newVar(existing)
  
      let emptySemObj = 
            {
                translFunc = []; 
                translFunc1 = []; 
                translFunc2 = []; 
                currentMRNA = [];
                currentProt = [];
                bbDevices = []; 
                prologConstraints = [IS_EMPTY_LIST(excNv)];
                arithmeticConstraints = [];
                substitutions = [];
                exclusiveSpecVar = excNv;
                specNames = [];        
                lbsProg = LBSNil;     
                rateDefs = [];
                log = []
             }
  
      // invoke main translation function:
      let sobj = translate db p env emptySemObj existing
      
      // filter out those substitutions which don't satisfy the arithmetic constraints we have accumulated
      let sobj = {sobj with substitutions = List.filter (fun cs -> Cssubst.satisfiesConstraints cs (applyRateDecs2ArithmeticConstraints sobj.rateDefs sobj.arithmeticConstraints)) sobj.substitutions}
  
      // append the final set of context-sensitive substitutions to the log
      let sobj = {sobj with log = sobj.log@["FINAL SET OF CONTEXT-SENSITIVE SUBSTITUTIONS:" + Lib.newline + Lib.string_of_list Cssubst.display Lib.newline sobj.substitutions]}
  
      // return relevant fields of semantic object:
      (sobj.bbDevices, sobj.prologConstraints, sobj.lbsProg, sobj.rateDefs, sobj.substitutions, sobj.arithmeticConstraints, sobj.log)
  
  // main translation function.
  // parameterised on a program and environment.
  and translate (db:Database.t) (p:prog) (env:tenv) (emptySemObj:tSemObj) (existing:string list)= 
    match p with
    | Nil -> 
        emptySemObj
    
    | Device(d) -> 
      {emptySemObj with lbsProg=LBSDevice(d)} 
    
    | Brick(v, t, propLst) ->
        // preprocess property list, replacing derived properties (non-quantitative) with their defined counterparts:
        let prepProp (prop:prop) =

            match (t, prop) with
            | ("prom", ("pos", [tf])) -> 
                let (r1, r2, r3) =
                    if !useConstants then
                        ("0", "0", "0")
                    else 
                        (newVar(existing), newVar(existing), newVar(existing))
                ("pos", [tf; [IdVal(r1)]; [IdVal(r2)]; [IdVal(r3)]])

            | ("prom", ("neg", [tf])) -> 
                let (r1, r2, r3) =
                    if !useConstants then
                        ("0", "0", "0")
                    else 
                        (newVar(existing), newVar(existing), newVar(existing))

                ("neg", [tf; [IdVal(r1)]; [IdVal(r2)]; [IdVal(r3)]])

            | ("pcr", ("codes", [prot])) -> 
                let r = if !useConstants then "0" else newVar(existing)
                ("codes", [prot; [IdVal(r)]])

            | _ -> prop    
        
        // invoke: 
        let propLst''' = List.map prepProp propLst

        // if promoter does not have a constitutive property, then add one:
        let conRate = if !useConstants then "0" else newVar(existing)
        let propLst'' = if (t <> "prom" || List.exists (fun (pname, lst) -> pname = "con") propLst''') then
                            propLst'''
                        else
                            ("con", [[IdVal(conRate)]])::propLst'''

        // if promoter does not have a functional rate property, then add one:
        (*let funRate = if !useConstants then "0" else newVar()
        let propLst'' = if (t <> "prom" || List.exists (fun (pname, lst) -> pname = "frate") propLst'') then
                            propLst''
                        else
                            ("frate", [[IdVal(funRate)]])::propLst''*)
        
        // if ribosome binding site does not have a rate property, then add one:
        let transcRate = if !useConstants then "1" else newVar(existing)
        let propLst' = if (t <> "rbs" || List.exists (fun (pname, lst) -> pname = "rate") propLst'') then
                            propLst''
                       else
                            ("rate", [[IdVal(transcRate)]])::propLst''

        // check that brick type is recognised:
        if not (t = "prom" || t = "pcr" || t = "rbs" || t = "ter") then
            failwith ("\nType error: unexpected brick type: " + t + "\n")
    

        // process brick value:
        let brick = match transVal v env existing with
                    | [b] -> b
                    | _ -> failwith("\nType error: non-atomic value used for brick identifier.\n")
        
        // translate properties:
        let propLst2 = (transPropLst propLst' env existing)
        
        // get a constraint for each property:
        let prologConstraints' = List.map (fun prop -> (getConstraintConstructor t) (brick, (Some prop))) propLst2

        // in case there are no properties, or if we are translating in order to generate
        // prolog database, add a constraint for the existence of an arbitrary biobrick:
        let prologConstraints = if prologConstraints'.Length = 0 || !useConstants
                                then ((getConstraintConstructor t) (brick, None))::prologConstraints'
                                else prologConstraints'
   
        // get species names and build constraints for exclusive names.
        // first define a function which finds the species names of properties:
        let getSpecNames (prop : string * string list list) =
            match prop with
            | (pname, x::xs) when pname = "pos" || pname = "neg" || pname = "pcr" || pname = "codes" -> [x]
            | _ -> []

        // and invoke:
        let specNames = propLst2 |> Seq.collect getSpecNames |> Set.ofSeq |> Set.toList 

        // old version of above, where we take all elements of property, including numbers (rates).
        // this works and is more general than the above, where we must know the properties, but it clutteres
        // the prolog output for presentation and is probably less effifient.
        //let specNames = propLst2 |> List.map (fun (pname, absCompLst) -> specNames absCompLst) |> List.flatten

        let nv = newVar(existing) 
        let exclusiveNamesConstraint' = [EXCLUSIVE_NAMES(t,brick,specNames,nv)]
        let exclusiveNamesConstraint = if !ignoreConsistencyConstraints then [] else exclusiveNamesConstraint'
        
        // update semantic object with information gathered so far, i.e. concerning the translation to biobricks:
        let newPrologConstraints = prologConstraints@exclusiveNamesConstraint

        // Compute a list of substitutions for this brick
        // NB: default database hard-coded in here...
        let substitutions,logEntries = Solver.matchParts db brick t propLst2

        // Produce some log output for debugging...
        let newLog = ("Searched for " + brick + " in the database:")::logEntries in

        
        let sobj = {emptySemObj with prologConstraints = emptySemObj.prologConstraints@newPrologConstraints; 
                                      substitutions = substitutions;
                                      bbDevices = [[brick]];
                                      specNames = specNames;
                                      exclusiveSpecVar = nv;
                                      log = emptySemObj.log @ newLog }
        
        // now update the semantic object with information needed for translation to reactions:        
        let sobj' = 
            match t with
            | "prom" ->   
                // create new names for gene and mrna:
                let gene = "g" + newStr()
                let mrna = "mrna" + newStr()

                // create a degradation reaction for mrna 
                let mrnaDegReac = LBSReac([],[[mrna]],[[]], RMRNADeg_string, true)

                // create initial population statement for gene:
                let initPop = LBSInitPop([gene], 1.0) 

                // function for processing properties; ignores frate properties
                let rec reacsFromProps prop =
                    match prop with
                    // a constitutive expression:
                    | ("con", [[rate]]) ->
                        let r1 = LBSReac([],[[gene]],[[gene]; [mrna]], rate, true)
                        [r1]
                      
                    // a "quantitative" expression (either pos or neg):
                    | (_, [tf; [rBind]; [rUnbind]; [rTranscr]]) ->

                        let r1 = LBSReac( [],[[gene]; tf],[gene::tf], rBind, true)
                        let r2 = LBSReac( [],[gene::tf], [[gene]; tf], rUnbind, true)
                        let r3 = LBSReac( [],[gene::tf], [gene::tf; [mrna]], rTranscr, true)
                        [r1;r2;r3]
                        
                    // if pos/neg property with no rates, just create fresh variables for rates:
                    | (pname, [tf]) when (pname="pos" || pname="neg") ->
                        reacsFromProps (pname, [tf; [newVar(existing)]; [newVar(existing)]; [newVar(existing)]])
                       
                    | _ -> [] 
                
                // apply the above function to get reactions:
                let reactions' = 
                    propLst2 |> List.collect reacsFromProps

                // get the frate property and create a functional rate reaction from this:
                let fRateProp = 
                    List.tryFind 
                        (fun prop ->
                            match prop with
                            | ("frate", [[rateStr]]) -> true
                            | _ -> false
                        )
                        propLst2
                in
                let fRateReac =
                    match fRateProp with
                    | Some("frate", [[rateStr]]) when (rateStr <> "0.0" && rateStr <> "0") ->
                        ( [],[[gene]], [[gene];[mrna]], rateStr, false)                        
                    | _ ->
                        ( [],[[gene]], [[gene];[mrna]], "0.0", false)                        

                                        
                // put reactions in parallel:
                
                let lbsProg'' = match reactions' with 
                                | [] -> LBSNil
                                | [r] -> r
                                | r1::[r2] -> LBSPar(r1,r2)
                                | first::second::remaining -> 
                                  let firstPar = LBSPar(first,second)
                                  List.fold (fun p1 p2 -> LBSPar(p1,p2)) firstPar remaining

                //let lbsProg'' = Lib.fold_left (fun p1 p2 -> LBSPar(p1, p2)) LBSNil reactions'

                // create abstraction construct with full reactions and the single functional rate reaction:
                let lbsProg' = LBSReacAbstraction(fRateReac, lbsProg'')

                // add mRNA degradation and init population:
                let lbsProg = LBSPar(lbsProg', LBSPar(mrnaDegReac, initPop))

                {sobj with lbsProg = lbsProg; currentMRNA = [mrna]}

            | "rbs"  ->
                // declare a function which, given mrna and protein, returns a reaction:
                let f mrna prot =
                    // find the rate in the first "rate" property encountered,
                    // or a fresh variable (or constant) if no such property is given.
                    let translRate = if !useConstants then "1" else newVar(existing)
                    let rec getRate propLst2 =
                        match propLst2 with
                        | [] -> translRate
                        | ("rate", [[r]])::rest -> r
                        | _::rest -> getRate rest
                
                    let rate = getRate propLst2
                    let r1 = LBSReac([],[[mrna]],[[mrna]; prot], rate, true)
                    r1
                    
                {sobj with translFunc = [f]}
                        
            | "pcr"  ->    
                // function for finding the protein coded by this pcr:      
                let rec protFromProps (props : (string * string list list) list) =
                    match props with
                    | [] -> 
                        None
                    | ("codes", [prot; [degRate]])::rest -> 
                        Some(prot, degRate)
                    | _::rest -> 
                        protFromProps []
   
                // invoke the above function and record the protein in return record:
                match (protFromProps propLst2) with
                    | None -> sobj
                    | Some(prot, rate) ->
                        let protDegReac = LBSDegReac([prot], rate)
                        {sobj with currentProt = [prot]; lbsProg = protDegReac }        
        
            | _ -> 
                sobj                
        
        sobj'
        
    | Reac(absCompLst1, absCompLst2, absCompLst3, rate, simOnly) ->
        // translate abstract complex lists:
        let absCompLst1' = transAbstractComplexLst absCompLst1 env existing
        let absCompLst2' = transAbstractComplexLst absCompLst2 env existing
        let absCompLst3' = transAbstractComplexLst absCompLst3 env existing
        
        // translate rate:
        let rate' = match transVal rate env existing with
                    | [r] -> if (!useConstants && isVar r) then "1" else r
                    | _   -> failwith ("Type error: complex value used as rate.\n")

        // create reaction and constraints:
        let lbsReac   = LBSReac(absCompLst1', absCompLst2', absCompLst3', rate', true)
        let prologConstraint = REACTION(absCompLst1', absCompLst2', absCompLst3', rate')

        // get species names:
        let specNames1 = specNames absCompLst1'
        let specNames2 = specNames absCompLst2'
        let specNames3 = specNames absCompLst3'

        // gather constraints and species names, but not if simulation-only flag is true:
        let prologConstraints = if (simOnly || env.simOnlyReacs) then [] else [prologConstraint]
        let specNames = if (simOnly || env.simOnlyReacs) then [] else specNames1@specNames2@specNames3

        // get all substitutions which unify the reaction with one from the database (unless it's a sim-only reaction...)
        let substitutions,logEntries =
          if (simOnly || env.simOnlyReacs) then [Cssubst.empty],[]
          else
            Solver.matchNormalReactions db absCompLst1' absCompLst2' absCompLst3' rate'
        let logEntries =
            let tempReac = Gecreaction.makeNormal absCompLst1' absCompLst2' absCompLst3' -1.0
            let reacStr = (tempReac |> Gecreaction.display |> Lib.quote) + " at rate " + rate'
            if (simOnly || env.simOnlyReacs) then ["Reaction " + reacStr + " is simulation only..."] else
            ("Looking for a normal reaction of the form " + reacStr)::logEntries 
        
        {emptySemObj with prologConstraints = prologConstraints@emptySemObj.prologConstraints;
                          substitutions = substitutions;
                          lbsProg = lbsReac;
                          specNames = specNames;
                          log = logEntries
        }
        
    | Trans(absComp1, absComp2, compartment, rate, simOnly, direction) ->
        // translate compartment value (may be under scope of new or formal par)
        let compartment = match transVal (IdVal(compartment)) env existing with
                            | [c] -> c
                            | _ -> failwith ("\nType error: non-atomic value used for compartment identifier.\n")

        let absComp1' = transAbstractComplex absComp1 env existing
        let absComp2' = transAbstractComplex absComp2 env existing
        
        // translate rate:
        let rate' = match transVal rate env existing with
                    | [r] -> if (!useConstants && isVar r) then "1" else r
                    | _   -> failwith("Type error: complex value used as rate.\n")

        // get species names:
        let specNames1 = specNames [absComp1']
        let specNames2 = specNames [absComp2']

        // create reaction and new constraint:
        let lbsTrans = LBSTrans(absComp1', absComp2', compartment, rate', direction)
        let prologConstraint = TRANSPORT(absComp1',absComp2',direction,rate')
         
        // check if simulation-only:
        let prologConstraints = if (simOnly || env.simOnlyReacs) then emptySemObj.prologConstraints 
                                else prologConstraint::emptySemObj.prologConstraints

        // get all substitutions which unify the reaction with one from the database (unless it's a sim-only reaction...)
        let substitutions,logEntries =
          if (simOnly || env.simOnlyReacs) then [Cssubst.empty],[]
          else
            Solver.matchTransportReactions db absComp1' absComp2' rate' compartment direction
        let logEntries =
            let tempReac = Gecreaction.makeTransport absComp1' absComp2' -1.0 compartment direction
            let reacStr = (tempReac |> Gecreaction.display |> Lib.quote) + " at rate " + rate'
            if (simOnly || env.simOnlyReacs) then ["Reaction " + reacStr + " is simulation only..."] else            
            ("Looking for a transport reaction of the form " + reacStr)::logEntries
        
        {emptySemObj with prologConstraints = prologConstraints;
                          substitutions = substitutions;
                          lbsProg = lbsTrans;
                          specNames = specNames1@specNames2;
                          log = logEntries
        }
        
    | TemplateDef(mid, fParLst, body, p') ->
        // define the semantic function for the body of this module:
        let f aParLst =
        
            // check that length of formals and actuals match:
            if not (List.length aParLst = List.length fParLst) then

                let error = "Length of formal and actual parameter lists do not match (" + mid + "/" + (List.length fParLst).ToString() + " <> " + (List.length aParLst).ToString() + ").\n"
                failwith error
            
            // update substitution:
            let subst' = Lib.fold_left (fun (map : tsubst) (fPar, aPar) -> map.Add(fPar, aPar) ) env.subst (List.zip fParLst aParLst)      
                
            // translate body of module in updated environment:    
            translate db body { env with subst = subst' } emptySemObj existing
            
        // update the module environment and translate the program following module declaration:
        let envm' = env.envm.Add(mid, f)    
        translate db p' { env with envm = envm' } emptySemObj  existing 
        
    | TemplateInv(mid, aParLst) ->
        // check that module has been declared:
        if not (env.envm.ContainsKey(mid)) then
            failwith ("Template " + mid + " has not been defined.\n")
            
        // translate actual parameter list:
        let aParLst' = transAbstractComplexLst aParLst env existing

        // look up the semantic function recorded for this module and invoke it with the evaluated formal parameters:
        let f = env.envm.[mid]
        f aParLst'
                
    
    | Par(p1, p2) ->
        // translate recursively:
        let sobj1 = translate db p1 env emptySemObj existing
        let sobj2 = translate db p2 env emptySemObj existing
        
        // find the a list of names and variables from both components, but with no duplicates:
        let namesVarsFlat = (List.concat sobj1.specNames)@(List.concat sobj2.specNames)
        let namesVarsFlatNoDups = namesVarsFlat |> Set.ofList |> Set.toList  

        // constraint saying that names for instantiating variables must be fresh, i.e.
        // the namesVarsNoDups list must remain a set when the variables are instantiated:
        let freshSubstPrologConstraint = NO_DUPLICATES(namesVarsFlatNoDups)
              
        // constraint saying that names of programs are disjoint from the respective exclusive names:
        let specNames1' = sobj1.specNames |> Set.ofList |> Set.toList
        let specNames2' = sobj2.specNames |> Set.ofList |> Set.toList
        let disjointPrologConstraint = NAMES_DISJOINT(specNames1', sobj1.exclusiveSpecVar, specNames2', sobj2.exclusiveSpecVar)

        // find the new exclusive names as the union of exclusive names from each component:
        let exNamesVar = newVar(existing)
        let exNamesUnionPrologConstraint = UNION(sobj1.exclusiveSpecVar, sobj2.exclusiveSpecVar, exNamesVar)

        // collect new constraints:
        let prologConstraints = if !ignoreConsistencyConstraints then [] else [freshSubstPrologConstraint; disjointPrologConstraint; exNamesUnionPrologConstraint]

        // combine the two sets of context-sensitive substitutions
        let substitutions = Cssubst.compose sobj1.substitutions sobj2.substitutions

        // produce a log entry to record the substitution composition
        let newLog = ["ABOUT TO DO A PARALLEL COMPOSITION OF SUBSTITUTIONS...";
                      "First one: " + Lib.newline + Lib.string_of_list Cssubst.display Lib.newline sobj1.substitutions;
                      "Second one: " + Lib.newline + Lib.string_of_list Cssubst.display Lib.newline sobj2.substitutions;
                      "Result: " + Lib.newline + Lib.string_of_list Cssubst.display Lib.newline substitutions]

        // assemble result and return:
        let sobj = {
                translFunc = sobj1.translFunc @ sobj2.translFunc; 
                translFunc1 = sobj1.translFunc1 @ sobj2.translFunc1;
                translFunc2 = sobj1.translFunc2 @ sobj2.translFunc2; 
                currentMRNA = sobj1.currentMRNA @ sobj2.currentMRNA;
                currentProt = sobj1.currentProt @ sobj2.currentProt;
                bbDevices = sobj1.bbDevices @ sobj2.bbDevices; 
                prologConstraints = sobj1.prologConstraints @ sobj2.prologConstraints @ prologConstraints;
                arithmeticConstraints = sobj1.arithmeticConstraints @ sobj2.arithmeticConstraints;
                substitutions = substitutions;
                exclusiveSpecVar = exNamesVar;
                specNames = sobj1.specNames @ sobj2.specNames;        
                lbsProg = LBSPar(sobj1.lbsProg, sobj2.lbsProg);     
                rateDefs = sobj1.rateDefs @ sobj2.rateDefs;
                log = sobj1.log @ sobj2.log @ newLog
             } 
    
        sobj
                
    | Seq(p1, p2) ->
        // translate recursively:
        let sobj1 = translate db p1 env emptySemObj existing
        let sobj2 = translate db p2 env emptySemObj existing
        
        // find the a list of names and variables from both components, but with no duplicates:
        let namesVarsFlat = (List.concat sobj1.specNames)@(List.concat sobj2.specNames)
        let namesVarsFlatNoDups = namesVarsFlat |> Set.ofList |> Set.toList  

        // constraint saying that names for instantiating variables must be fresh, i.e.
        // the namesVarsNoDups list must remain a set when the variables are instantiated:
        let freshSubstPrologConstraint = NO_DUPLICATES(namesVarsFlatNoDups)
              
        // constraint saying that names of programs are disjoint from the respective exclusive names:
        let specNames1' = sobj1.specNames |> Set.ofList |> Set.toList
        let specNames2' = sobj2.specNames |> Set.ofList |> Set.toList
        let disjointPrologConstraint = NAMES_DISJOINT(specNames1', sobj1.exclusiveSpecVar, specNames2', sobj2.exclusiveSpecVar)

        // find the new exclusive names as the union of exclusive names from each component:
        let exNamesVar = newVar(existing)
        let exNamesUnionPrologConstraint = UNION(sobj1.exclusiveSpecVar, sobj2.exclusiveSpecVar, exNamesVar)

        // collect new constraints:
        let prologConstraints' = if !ignoreConsistencyConstraints then []
                                 else [freshSubstPrologConstraint; disjointPrologConstraint; exNamesUnionPrologConstraint]
        let prologConstraints = sobj1.prologConstraints @ sobj2.prologConstraints @ prologConstraints'

         // combine the two templates list (what is the technical word for this operation?)
        let bbDevices2D = List.map (fun x -> List.map (fun y -> x@y) sobj2.bbDevices) sobj1.bbDevices
        let bbDevices   = List.concat(bbDevices2D)

 
        // ********** END OF BIOBRICK TRANSLATION *************
        // ********** START OF REACTION TRANSLATION *********
 
        // check for information about current mrna and update second semantic object accordingly: 
        let sobj2' = 
            match sobj1.currentMRNA with
            | [] -> 
                sobj2
            | [mrna] ->
                // apply partially evaluated translation functions to this mrna to get reactions:
                let newReacs = sobj2.translFunc1 |> List.map (fun f -> (f mrna)) 
                // create lbs parallel prog from list of reactions:
                let lbsProg = match newReacs with 
                               | [] -> LBSNil
                               | [r] -> r
                               | r1::[r2] -> LBSPar(r1,r2)
                               | first::second::remaining -> 
                                 let firstPar = LBSPar(first,second)
                                 List.fold (fun p1 p2 -> LBSPar(p1,p2)) firstPar remaining
                             
                //let lbsProg = List.fold (fun p1 p2 -> LBSPar(p1,p2)) LBSNil newReacs
                
                // apply non-evaluated translation functions to this mrna to obtain new functions:
                let newFunc2 = sobj2.translFunc  |> List.map (fun f -> (f mrna))
                                   
                // and return:
                {sobj2 with lbsProg = LBSPar(sobj2.lbsProg, lbsProg);
                           translFunc  = [];
                           translFunc1 = [];
                           translFunc2 = newFunc2;
                }
                
            | _ -> failwith ("Sequential composition is only supported for singleton results, aborting translation [1].\n")
                
        // check for information about current protein and update first semantic object appropriately:
        let (sobj1', sobj2'') =
            match sobj2'.currentProt with
            | [] ->
                (sobj1, sobj2')
            | [prot] ->
                // apply partially evaluated translation functions to this protein to get reactions:
                let newReacs = sobj1.translFunc2 |> List.map (fun f -> f prot)
                // create lbs parallel prog from list of reactions:
                let lbsProg = match newReacs with 
                               | [] -> LBSNil
                               | [r] -> r
                               | r1::[r2] -> LBSPar(r1,r2)
                               | first::second::remaining -> 
                                 let firstPar = LBSPar(first,second)
                                 List.fold (fun p1 p2 -> LBSPar(p1,p2)) firstPar remaining
                //let lbsProg = Lib.fold_left (fun p1 p2 -> LBSPar(p1,p2)) LBSNil newReacs
                
                // apply non-evaluated translation functions to this protein to obtain new functions:
                let newFunc1 = sobj1.translFunc  |> List.map (fun f -> (fun mrna -> (f mrna prot)))
                 
                // and return:
                let sobj1' =  { sobj1 with lbsProg = LBSPar(sobj1.lbsProg, lbsProg);
                                           translFunc  = [];
                                           translFunc1 = newFunc1;
                                           translFunc2 = []; }
                                          
                let sobj2'' = {sobj2' with currentProt = []}
                (sobj1', sobj2'')
    
            | _ -> failwith("sequential composition is only supported for singleton results, aborting translation [2].\n")

    
        // propagate the current mrna from the left (this component) to the composite if
        // none is defined for the right component: 
        let currentMRNA = if sobj2''.currentMRNA.Length = 0 then sobj1'.currentMRNA else sobj2''.currentMRNA
 
        // propagate the current protein from the right (this component) to the composite if
        // none is defined for the left component:                                  
        let currentProt = if sobj1'.currentProt.Length = 0 then sobj2''.currentProt else sobj1'.currentProt
    
        // combine the two sets of context-sensitive substitutions
        let substitutions = Cssubst.compose sobj1'.substitutions sobj2'.substitutions

        // combine all results (both biobrick and reaction translation) into a semantic object and return:
        let sobj = {
                translFunc = sobj1'.translFunc @ sobj2'.translFunc; 
                translFunc1 = sobj1'.translFunc1 @ sobj2'.translFunc1; 
                translFunc2 = sobj1'.translFunc2 @ sobj2'.translFunc2; 
                currentMRNA = currentMRNA;
                currentProt = currentProt;
                bbDevices = bbDevices; 
                prologConstraints = prologConstraints;
                arithmeticConstraints = sobj1'.arithmeticConstraints @ sobj2.arithmeticConstraints;
                substitutions = substitutions;
                exclusiveSpecVar = exNamesVar;
                specNames = sobj1'.specNames @ sobj2'.specNames;        
                lbsProg = LBSPar(sobj1'.lbsProg, sobj2'.lbsProg);     
                rateDefs = sobj1.rateDefs @ sobj2.rateDefs;
                log = sobj1.log @ sobj2.log
                 } 
    
        sobj
    

    | Comp(cid, p') ->
        // translate compartment value (may be under scope of new or formal par)
        let cid = match transVal (IdVal(cid)) env existing with
                  | [cid'] -> cid'
                  | _ -> failwith("\nType error: non-atomic value used for compartment identifier.\n")

        // translate contained program:
        let sobj = translate db p' env emptySemObj existing

        // remove the "context" from the context-sensitive substitutions
        let substitutions = List.map Cssubst.eraseContext sobj.substitutions

        // "forget" exclusive names by creating a fresh variable which unifies with the
        // empty list, and create an lbs compartment program:
        let newVar = newVar(existing)
        {sobj with prologConstraints = sobj.prologConstraints @ [IS_EMPTY_LIST(newVar)];
                   substitutions = substitutions;
                   exclusiveSpecVar = newVar;
                   specNames = [];
                   lbsProg = LBSComp(cid, sobj.lbsProg);
        }
        
        
    | New(id, p') ->
        // create a fresh id and update substitution in environment:
        let freshId = id + newStr()
        let subst'  = env.subst.Add(id, [freshId])
        let env'    = { env with subst = subst' }
    
        // translate:
        let sobj = translate db p' env' emptySemObj existing
        
        // add a compartment declaration to lbs prog if the id is lower-case, i.e. a name. if
        // the id is not used as a compartment, this will introduce a redundant
        // compartment declaration but no harm is done.

        //Websharper compatiblity
        let isLowerChar (c:char) = (((int) c >= 97) && ((int) c <= 122))

        let lbsProg' = if (isLowerChar(id.Chars 0)) then LBSCompDec(freshId, sobj.lbsProg)
                       else sobj.lbsProg
                        
        // and return.
        {sobj with lbsProg = lbsProg'}            
    

    | Constraint(a1,op,a2) ->
        // translate values:
        let a1' = transAExp a1 env 
        let a2' = transAExp a2 env 
        
        let prologConstraint = ARITHMETIC(a1', op, a2')
        let arithmeticConstraint = (a1', op, a2')
        let newVar = newVar(existing)
        {emptySemObj with prologConstraints = prologConstraint::emptySemObj.prologConstraints;
                          arithmeticConstraints = arithmeticConstraint::emptySemObj.arithmeticConstraints;
                          substitutions = [Cssubst.empty]} // Is this OK in the case where the program is "just" constraints? Do we care?
(*
    | Constraint(a1,op,a2) ->
        // translate values:
        let v1' = transVal v1 env
        let v2' = transVal v2 env
        
        // check that values are simple and extract them from lists:
        let (v1'', v2'') = match (v1', v2') with
                           | ([x],[y]) -> (x,y)
                           | _ -> raise (LBS.Error.CompilerExPos("Type error: complex values are not allowed in constraints.\n", None))
        let prologConstraint = ARITHMETIC(v1'', op, v2'')
        let arithmeticConstraint = (v1'', op, v2'')
        let newVar = newVar()
        {emptySemObj with prologConstraints = prologConstraint::emptySemObj.prologConstraints;
                          arithmeticConstraints = arithmeticConstraint::emptySemObj.arithmeticConstraints;
                          substitutions = [Cssubst.empty]} // Is this OK in the case where the program is "just" constraints? Do we care?
*)
        
    | InitPop(absComp, v) ->
        // translate initial population statementsn direcly to lbs:
        let lbsProg = LBSInitPop(transAbstractComplex absComp env existing, v)
        {emptySemObj with substitutions = [Cssubst.empty];
                          lbsProg = lbsProg}
        
        
    | Ast.Rate(var, v) -> 
        // translate var and add rate to semantic object:
        let varStr = match transVal var env existing with
                     | [varStr] -> varStr
                     | _ -> failwith("Type error: use of complex expression where value expected.\n")
                     
        {emptySemObj with substitutions = [Cssubst.empty];
                          rateDefs = [(varStr, v)]}

    
    | Copy(num, p, isPar, simOnly) ->
        if (num < 1) then 
            failwith("Type error: copy operator must be given a positive argument.\n")
    
        // define a function for putting p in par or seq a given number of times:
        let rec build num' =
            if num' = 1 then
                p
            else 
                if isPar then
                    Par(p, (build (num' - 1)))
                else
                    Seq(p, (build (num' - 1))) 
            
        // build composition num times of prog, but only if simulation-only flag false:
        let p' = if simOnly then p else build num

        // translate result:
        let sobj = translate db p' env emptySemObj existing

        // if simulation-only, then contruct a LBSCopy program:
        // (this avoids copying constraints).
        let lbsProg' = if simOnly then
                            LBSCopy(num, sobj.lbsProg)
                       else 
                            sobj.lbsProg
    
        {sobj with lbsProg = lbsProg'}
  
  
  // the following family of functions simply translate values in the given structure
// to their string representations.
  and transPropLst propLst (env : tenv) (existing:string list)=
    propLst |> List.map (fun prop -> transProp prop env existing)
  
  and transProp prop (env : tenv) (existing:string list)=
    let (propName, absCompLst) = prop
    let absCompLst' = transAbstractComplexLst absCompLst env existing
    (propName, absCompLst')
      
  and transAbstractComplexLst absCompLst env existing= 
    absCompLst |> List.map (fun absComp -> transAbstractComplex absComp env existing)
      
  and transAbstractComplex absComp env existing =
    // translate each value in the abstract complex:
    let absComp2 = absComp |> List.map (fun v -> transVal v env existing) |> List.concat    
    absComp2
  
  and transVal v (env : tenv) (existing:string list) = 
    match v with
    // check if an id is to be substituted (either an actual par or a fresh name):
    | IdVal(id) -> 
        if (env.subst.ContainsKey(id)) then
            env.subst.[id]
        else
            [id]
     
    | FloatVal(f) -> 
        [Lib.display_float f]
        
    // wild cards are just short cuts for fresh vars:    
    | WildCardVal -> 
        [newVar(existing)]
    | AlgebraicExp _ -> failwith "Unexpected AlgebraicExp"
  
  and transAExp (a:Ast.aexp) (env : tenv) : Ast.aexp =
    match a with
    | FloatAExp v -> FloatAExp v
    | IdAExp id ->
        // first check if our environment contains this ID; this is either
        // for formal -> actual mappings, or for fresh renaming.
        if (env.subst.ContainsKey(id)) then
            match env.subst.[id] with
            | [x] -> 
                // result depends on whether x is a float.
                (*let f = ref 0.0
                let isFloat = System.Double.TryParse(x, f)
                
                if isFloat then
                    FloatAExp(!f)
                else
                    IdAExp(x)*)
                NumUtil.case_double FloatAExp (IdAExp x) x
                (*
                try
                    FloatAExp(Double.Parse(x))
                with
                    | _ -> IdAExp(x)
                *)

               
            | _ -> failwith "Cannot pass a complex expression in module invocations."

        // otherwise check if called in the context of constraint solving; then
        // the identifier should be in the Subst environment. if it isn't, just
        // create an identifier expression
        else 
            match Subst.getFloat id with
            | Some(f) -> FloatAExp(f)
            | None -> IdAExp id 
         
            
    | PlusAExp (a1,a2) -> PlusAExp(transAExp a1 env, transAExp a2 env)
    | MinusAExp (a1,a2) -> MinusAExp(transAExp a1 env, transAExp a2 env)
    | MulAExp (a1,a2) -> MulAExp(transAExp a1 env, transAExp a2 env)
    | DivAExp (a1,a2) -> DivAExp(transAExp a1 env, transAExp a2 env)
    | PowAExp (a1,a2) -> PowAExp(transAExp a1 env, transAExp a2 env)
  
  // get species names from an abstract complex list:
  and specNames absCompLst =
    absCompLst |> List.filter (fun absComp -> absComp |> List.forall (fun s -> not(isNum s)))
  
  // boolean functions for testing types of strings:    
  and isName (s:string) = Char.IsLower s.[0]
  and isVar  (s:string) = Char.IsUpper s.[0] || s.[0] = '_'
  and isNum  (s:string) = (*let d = ref 0.0 in (Double.TryParse(s, d))*)
                        NumUtil.isNum s
  (*
                          try
                              Double.Parse(s) |> ignore
                              true
                          with
                              | _ -> false
  *)
    
  // concatenates the elements of the string list with seperating commas:
  and concatWith symbol lst =
    Lib.fold_left (fun s1 s2 -> if (s1 <> "") then s1 + symbol + s2 else s2) "" lst
     
  // create a lbs string representing degradation reations from a list of 
// LBS degradation reactions and a list of compartments where these should
// be put.
  and createDegReacs degReacs comps varDefs =
    // function to convert degradation reactions to strings:
    let toStr degReac = 
        match degReac with
        | LBSDegReac(complex, rate) -> 
            let lbsReac = LBSReac([],complex,[],rate, true)
            let (str, _, _) = lbsProgToStr' lbsReac varDefs None // note that parent comp (None) will never be relevant here.
            str
        | _ -> ""

    // invoke on list:
    let degReacStrings = degReacs |> List.map toStr 

    // remove duplicates from degradation reactions:
    let degReacStrings' = degReacStrings |> Set.ofList |> Set.toList 

    // remove duplicates from compartment list:
    let comps' = comps |> Set.ofList |> Set.toList 

    // compose into single string seperated by par:
    let degReacsStr = concatWith " | " degReacStrings'

    // insert degradation reactions into each compartment, concatenate compartements in par:
    let str = comps' |> List.map (fun c -> newline + c + " [" + degReacsStr + newline + "]")  |>  (concatWith ("|" + newline))

    // add degradation in no compartment (world) and return:
    if str <> "" then (str + "|" + newline + degReacsStr) else degReacsStr
  
   
  // translate an lbs abstract syntax tree to a concrete syntax string.
  // takes as parameters an LBS AST and variable definitions.
  and lbsProgToStr lbsProg (varDefs : (string * string) list) =
    // get a standard program string, and degradation + compartments seperately:
    let (progStr, degReacs, comps) = lbsProgToStr' lbsProg varDefs None

    // get a string with degradation reactions for each compartment in parallel:
    let degReacsStr = createDegReacs degReacs comps varDefs 

    // append program string to deg string (if any) and return:
    if (degReacsStr <> "") then
        progStr + " | " + degReacsStr
    else
        progStr
  
  // translate an lbs program to a string, returning seperately a list of compartments 
// which are not under the scope of any compartment declarations and
// degradation reactions (non-strings, since we need to remove duplicates later). 
// The latter need to be added to all compartments seperately for
// systems such as the predator-prey to work -- this
// unfortunately is non-compositional and not very neat.
// apart from an lbs prog, the function takes variable definitions and
// the current parent compartment(a string) as parameters. the former is needed
// to substitute variables for their solutions, and the latter is needed to
// translate transport reactions, such as c[s] -> s, to c[s] -> c[s] -> c'[s] where
// c' is the containing reaction; this respects the LBS compartment semantics.
  and lbsProgToStr' lbsProg (varDefs : (string * string) list) parentComp =
    
    // function for substituting vars for defined values in list of value/vars:
    let substVarVal lst = 
        lst |> List.map (fun x -> match Lib.try_assoc x varDefs with | Some y -> y | None -> x)


    match lbsProg with
    | LBSDevice(d) -> 
        (d,[],[])
    | LBSReac(enzymes, reactants, products, rate, isMassAction) ->
        
        let lst2dToLBSSum lst2d = 
            // replace vars for their definitions:
            let lstVarsReplaced = lst2d |> List.map (fun lst -> lst |> substVarVal)
            let lst = lstVarsReplaced |> List.map (concatWith "::")
            (concatWith " + " lst)
            
        let sum1 = lst2dToLBSSum enzymes
        let sum2 = lst2dToLBSSum reactants
        let sum3 = lst2dToLBSSum products
        
        // replace rate for defined value if necessary:
        let rate' = match Lib.try_assoc rate varDefs with Some x -> x | None -> rate
        
        // determine which rate brackets to use:
        let (bracLeft, bracRight) = if isMassAction then ("{", "}") else ("[", "]")

        // don't print the ~ symbol if there are no enzymes:
        let prefix = if enzymes = [] then "" else sum1 + " ~ "
        
        //let str = newline + sprintf "%s%s ->%s%s%s %s " prefix sum2 bracLeft rate' bracRight sum3
        let str = newline + prefix + sum2 + " ->" + bracLeft + rate' + bracRight + " " + sum3 + " "
        (str, [], [])

    | LBSReacAbstraction(absReac, program) ->
        // if abstract reaction has non-0 rate defined in substitution, use this; otherwise use the detailed reactions in program:
        match absReac with
        | ( _, _, _, rateVarStr, _) ->
            let rateStr = match Lib.try_assoc rateVarStr varDefs with Some x -> x | None -> "0.0"

            if (rateStr <> "0.0" && rateStr <> "0") then
                let p = LBSReac(absReac) in
                lbsProgToStr' p varDefs parentComp
            else
                lbsProgToStr' program varDefs parentComp


    | LBSDegReac(complex, rate) ->
        ("", [lbsProg], [])
        
    | LBSTrans(complex1, complex2, compartment, rate, direction) ->
        // replace vars for their defined values:
        let complex1' = substVarVal complex1 
        let complex2' = substVarVal complex2
 
        // convert lists to LBS complexes:
        let complex1Str = concatWith "::" complex1'
        let complex2Str = concatWith "::" complex2'

        // replace rate for defined value if necessary:
        let rate' = match Lib.try_assoc rate varDefs with Some x -> x | None -> rate

        // creates a string representation of spec s inside the current parent comp, taking into accound whether 
        // or not this is the empty world compartment:
        let specInParentComp s =
            (match parentComp with
            | None -> s
            | Some(c') -> c' + "[" + s + "]")

        let str = if direction = In then
                    // we could use a more complex diffusion-style rate (see commented-out def below),
                    // but doing so makes the simulation go slow and weird. don't know why.
                    let arrow = "->{" + rate' + "} " in
                    //let arrow = "->[" + rate' + " * (" + complex1Str + " -- " + compartment + "[" + complex2Str + "])] "
                    newline + complex1Str + arrow + compartment + "[" + complex2Str + "]"
                  else
                    let arrow = "->{" + rate' + "} " in
                    //let arrow = "->[" + rate' + " * (" + compartment + "[" + complex1Str + "] -- " + complex2Str + ")] " 
                    newline + compartment + "[" + complex1Str + "] " + arrow + complex2Str
 
        (str, [], [])
         
    | LBSComp(cname, p) ->
        let (pStr, pDeg, pComps) = lbsProgToStr' p varDefs parentComp
        let cStr = newline + newline + cname + " [" + pStr + newline + "]"
        (cStr, pDeg, cname::pComps)
        
    | LBSPar(p1, p2) ->
        let (p1Str, p1Deg, p1Comps) = lbsProgToStr' p1 varDefs parentComp
        let (p2Str, p2Deg, p2Comps) = lbsProgToStr' p2 varDefs parentComp

        let isAllWhiteSpace text =
            not (Seq.exists (fun x -> not (Char.IsWhiteSpace x)) text)

        // don't want to print a lot of empty Nil programs, so ignore parallel
        // composition sym if one component is Nil (translates to empty string):
        let parSym = if (isAllWhiteSpace p1Str || isAllWhiteSpace p2Str) then "" else "|"
        
        //let str = sprintf "%s %s %s" p1Str parSym p2Str
        let str = p1Str + " " + parSym + " " + p2Str
        (str, p1Deg@p2Deg, p1Comps@p2Comps)
        
    | LBSInitPop(lst, pop) ->
        // creates a string representation of spec s inside the current parent comp, taking into account whether 
        // or not this is the empty world compartment:
        let specInParentComp s =
            match parentComp with
            | None -> s
            | Some(c') -> c' + "[" + s + "]"

        let lst' = substVarVal lst
        let str = concatWith "::" lst'
        let pStr = newline + "init " + str + " " + string pop
        (pStr, [], [])

    | LBSCompDec(cname, p) ->
        let (pStr, pDeg, pComps) = lbsProgToStr' p varDefs parentComp

        // create degradation reactions for this compartment 
        // (its important to have them within scope of the compartment declaration):
        let degReacsStr = createDegReacs pDeg [cname] varDefs
        
        let str = newline + "comp " + cname + "; " + pStr + " | " + degReacsStr + newline

        // filter the compartment name out of the return compartment list;
        // we don't want degradation reactions to be created for this compartment
        // since we already created them locally:
        let pComps' = List.filter (fun c -> c <> cname) pComps

        // and return:
        (str, pDeg, pComps')
        
    | LBSCopy(num, p) ->
        let (pStr, pDeg, pComps) = lbsProgToStr' p varDefs parentComp
        let str = newline + "copy " + (string num) + " { " + newline + pStr + " " + newline + "}" + newline
        (str, pDeg, pComps)

    | LBSNil -> ("", [], [])


// returns to lists containing the species variables and rate variables contained in a reaction:
let rec getVarsFromLBSProg p =
  
      match p with
      | LBSReac(enzymes, reactants, products, rate, _) ->
          let vars1 = getVarsFromLst2d enzymes
          let vars2 = getVarsFromLst2d reactants
          let vars3 = getVarsFromLst2d products
          let varsRate = if isVar rate then [rate] else []
          (vars1@vars2@vars3, varsRate)
  
      | LBSDegReac(reactants, rate) ->
          let p = LBSReac([], reactants, [], rate, true)
          getVarsFromLBSProg p
          
      | LBSTrans(complex1, complex2, compartment, rate, direction) ->
          let vars1 = getVarsFromLst2d [complex1]
          let vars2 = getVarsFromLst2d [complex2]
          let varsRate = if isVar rate then [rate] else []
          (vars1@vars2, varsRate)            
          
      | LBSComp(cname, p) ->
          getVarsFromLBSProg p
          
      | LBSPar(p1, p2) ->
          let (vars1, rateVars1) = (getVarsFromLBSProg p1)
          let (vars2, rateVars2) = (getVarsFromLBSProg p2)
          (vars1@vars2, rateVars1@rateVars2)
  
      | LBSCompDec(c, p) ->
          getVarsFromLBSProg p
  
      | LBSCopy(num, p) ->
          getVarsFromLBSProg p
  
      | _ -> ([], [])
      
  
  // returns the variables in a 2d list of strings as a list
  and getVarsFromLst2d lst2d =
      lst2d |> List.concat |> List.filter isVar //|> Set.ofList |> Set.toList
  
//  translate0 p simOnlyReacs db
    



(* NOTES *)
   
(* OBS: we cannot translate rate declarations directly to LBS rate declarations.
The reason is that reactions which should be in the scope of such rate declarations
cannot always be generated locally, specifically for translation reactions. 

Here is an example illustrating the problem:

---
module transl(out) { 
       sim-rate RTransl 0.1; rbs<rate(RTrans)>
};

transl(O1); transl(O2)
---

This will result in the following output (since nil programs are not printed):

rate RTransl;   |
rate RTransl;

This is certainly not what we want.

So, the current solution is to substitute rates directly into lbs programs or, alternatively,
collect all rate declarations and prefix the main LBS program with these.
*)
    