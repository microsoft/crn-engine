[<JavaScript>]
module Microsoft.Research.GEC.Solver

open Microsoft.Research.GEC
//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

(* ************************************************************************************************************ *)

(* Some helper functions. *)

(* NB: copied from trans.fs... NB: altered to handle complexes! *)
//let getComplexNamesFromStrings (prop:string * (string list list)) : string list = match prop with
//  | (pname, x::xs) when pname = "pos" || pname = "neg" || pname = "pcr" || pname = "codes" -> [Ast.complexString x]
//  | _ -> []

(* Get the COMPLEXES which exist in a list of part properties. *)
let getComplexesFromStrings (prop:string * (string list list)) : string list list =
  match prop with
  | (pname, x::xs) when pname = "pos" || pname = "neg" || pname = "pcr" || pname = "codes" -> [x]
  | _ -> []

(* Compare a string and a partType to see if they match... *)
let typesAgree (t:string) (partTy:Database.partType) =
  match t,partTy with
  | "pcr", Database.PCR _ | "prom", Database.PROM _ | "rbs", Database.RBS _ | "ter", Database.TER _ -> true
  | _,_ -> false

(* Find all substitutions that can unify two complexes, in any order... *)
let unifyComplexes ((*variables*)xs:string list) ((*species*)zs:string list) : Subst.t list =
  let zs = List.map (fun z -> Subst.SPECIES [z]) zs in
  if not(List.length xs = List.length zs) then [] else
  let assocLsts = List.map (fun perm -> List.zip xs perm) (Lib.permutations zs) in
  Lib.maybemap Subst.multiple assocLsts

(* Unify two (possibly empty) lists of complexes. *)
let unifyComplexLists ((*variables*)xss:string list list) ((*species*)zss:string list list) : Subst.t list =
  match xss,zss with
  | [],[] -> [Subst.empty]
  | _::_,_::_ ->
      if not(List.length xss = List.length zss) then [] else
      let rec loop (thetas:Subst.t list) ((*variables*)qss:string list list) ((*species*)wss:string list list) =
        match qss,wss with
        | [],[] -> thetas
        | (qs::qss, ws::wss) ->
            let thetas' = Lib.collect (fun theta -> let qs' = Subst.applyToComplex theta qs in
                                                    let ws' = Subst.applyToComplex theta ws in
                                                    let thetas' = unifyComplexes qs' ws' in
                                                    let thetas' = Lib.maybemap (Subst.union theta) thetas' in
                                                    let str =
                                                      "Trying to unify " + (Ast.complexString qs) + " with " + (Ast.complexString ws) + 
                                                      " when theta = " + (Subst.display theta) + " *** "+
                                                      " got thetas' = " + (Lib.string_of_list Subst.display " ~~ " thetas')
                                                    in
                                                    thetas') thetas
            in
            //let newLog = [] in
            loop thetas' qss wss
        | _,_ -> failwith "unifyComplexLists: outer lists must be the same length"
      in
      Lib.collect (fun zss -> loop [Subst.empty] xss zss) (Lib.permutations zss)
  | _,_ -> []

(* ************************************************************************************************************ *)

(* Scan the database for matching parts. *)

(* Try to find substitutions which make a string-based promoter property match a ground one from the database. *)
let tryFindPromoterSubsts (theta_original:Subst.t) (bp:string * (string list list)) (pp:Database.promProperty) : Subst.t list =
  let bp = Subst.applyToPartTypeStrings theta_original bp in
  let unifyPP (xs,q1,q2,q3) (zs,r1,r2,r3) =
    match Subst.multiple [(q1,Subst.NUMBER r1);(q2,Subst.NUMBER r2);(q3,Subst.NUMBER r3)] with
    | None -> []
    | Some theta' ->
        match Subst.union theta_original theta' with
        | None -> []
        | Some theta''' -> Lib.maybemap (fun theta'' -> Subst.union theta'' theta''') (unifyComplexes xs zs)
  in
  match bp, pp with
  | ("frate", [[q]]), Database.FRATE(a) -> 
      begin match (Subst.unify q (Subst.ALGEBRAIC_EXPRESSION a)) with
      | None -> []
      | Some theta ->
          begin match Subst.union theta_original theta with
          | None -> []
          | Some theta' -> [theta']
          end
      end
  | ("pos", [xs;[q1];[q2];[q3]]), Database.POS(zs,r1,r2,r3) //
  | ("neg", [xs;[q1];[q2];[q3]]), Database.NEG(zs,r1,r2,r3) -> unifyPP (xs,q1,q2,q3) (zs,r1,r2,r3)
  | ("con", [[q]]), Database.CON(r) ->
      begin match (Subst.unify q (Subst.NUMBER r)) with
      | None -> []
      | Some theta ->
          begin match Subst.union theta_original theta with
          | None -> []
          | Some theta' -> [theta']
          end
      end
  | _,_ -> []

(* Try to find substitutions which make a string-based PCR property match a ground one from the database. *)
let tryFindPCRSubsts (theta_original:Subst.t) (bp:string * (string list list)) (pp:Database.pcrProperty) : Subst.t list =
  let bp = Subst.applyToPartTypeStrings theta_original bp in
  match bp, pp with
  | ("codes", [xs;[q]]), Database.CODES(zs,r) ->
      begin match (Subst.unify q (Subst.NUMBER r)) with
      | None -> []
      | Some theta ->
          begin match Subst.union theta_original theta with
          | None -> []
          | Some theta' -> Lib.maybemap (fun theta'' -> Subst.union theta'' theta') (unifyComplexes xs zs)
          end
      end
  | _,_ -> []

(* Try to find substitutions which make a string-based RBS property match a ground one from the database. *)
let tryFindRBSSubsts (theta_original:Subst.t) (bp:string * (string list list)) (rp:Database.rbsProperty) : Subst.t list =
  let bp = Subst.applyToPartTypeStrings theta_original bp in
  match bp, rp with
  | ("rate", [[q]]), Database.RATE(r) ->
      begin match (Subst.unify q (Subst.NUMBER r)) with
      | None -> []
      | Some theta ->
          begin match Subst.union theta_original theta with
          | None -> []
          | Some theta' -> [theta']
          end
      end
  | _,_ -> []

(* Try to find substitutions which make a string-based RBS property match a ground one from the database.
   NB: this always returns an empty list because terminators don't have properties associated with them
   (at least the one in the default database doesn't...) *)
let tryFindTerSubsts (theta_original:Subst.t) (bp:string * (string list list)) : Subst.t list = []

(* Compute a set of context-sensitive substitutions for a brick, relative to a database. *)
let matchParts (db:Database.t) (brick:string) (t:string) (props:(string * (string list list)) list) : Cssubst.t list * string list =

  (* Recursive function for searching the parts database. *)
  let find ((res,log):Cssubst.t list * string list) (partId:string) (entry:Database.partType Database.entry) : Cssubst.t list * string list =
    let partTy = entry.value in
    if not entry.enabled then (res,log) else
    // NB: If we limited the input, could we make this simpler???
    // NB: Should we avoid converting everything to strings in trans.fs???
    // NB: Should we convert the "database" to use a string-based system so it's more flexible for the future?
    let init = if not(typesAgree t partTy) then [] else
               match (Subst.unify brick (Subst.PART partId)) with Some init -> [init] | None -> []
    in
    let rec expand ((thetas,log):Subst.t list * string list) (bps:(string * (string list list)) list) : Subst.t list * string list =
      match bps with
      | [] -> (thetas,log)
      | (bp::bps) -> 
          let rec substLoop (new_thetas:Subst.t list) (thetas:Subst.t list) =
            match thetas with
            | [] -> new_thetas
            | (theta::thetas) -> 
                let extra_thetas = 
                  begin match partTy with
                  | Database.PROM(props) -> Lib.collect (fun pp -> tryFindPromoterSubsts theta bp pp) props
                  | Database.PCR(prop) -> tryFindPCRSubsts theta bp prop
                  | Database.RBS(prop) -> tryFindRBSSubsts theta bp prop
                  | Database.TER -> tryFindTerSubsts theta bp (* NB: no property to put here as terminators don't have any... *)
                  end
                in
                substLoop (new_thetas@extra_thetas) thetas
          in
          let new_thetas = substLoop [] thetas in
          let extra_log = ["Thetas are:\n" + (Lib.string_of_list Subst.display"\n" thetas);
                           "New_thetas are:\n" + (Lib.string_of_list Subst.display"\n" new_thetas)] in
          let new_log = log@extra_log in
          expand (new_thetas,new_log) bps
    in
    let initString = "Initial subst: " + (match init with [init] -> Subst.display init | _ -> "*NO MATCH*") in
    let propsString = 
      let inner (zs:string list) = "[" + (Lib.string_of_list Lib.id ";" zs) + "]" in
      "Properties: " + (Lib.string_of_list (fun (x,zss) -> x + "(" + (Lib.string_of_list inner "," zss) + ")") "; " props)
    in
    let thetas,expand_log = expand (init,[]) props in
    // Eliminate any duplicate results from the list of substitutions
    let thetas = Lib.remove_duplicates Subst.eq thetas in
    let mkCSSubst (theta:Subst.t) : (Cssubst.t * string) option =
      let rho = Subst.speciesDomain theta in
      let ground_props = List.map (Subst.applyToPartTypeStrings theta) props in
      let sigma = Lib.collect_union Ast.complexesEqual getComplexesFromStrings ground_props in
      let tau = Lib.difference Ast.complexesEqual (Database.speciesInPartType partTy) sigma in
      let cs = Cssubst.make theta rho sigma tau in
      let log = "Producing a CSSubst: " + Lib.newline +
                Cssubst.display cs + Lib.newline +
                "...where FS(Q_i) = " + Lib.string_of_list Ast.complexString ", " (Database.speciesInPartType partTy) + Lib.newline +
                "...where ground_props = " + Lib.string_of_list (fun (x,ps) -> x+"("+(Lib.string_of_list Ast.complexString ", " ps)+")") ", " ground_props in
      if Cssubst.isOK cs then Some (cs,log) else None
    in
    let new_csSubstsLogs = Lib.maybemap mkCSSubst thetas in
    let new_csSubsts, new_CsLogs = Lib.unzip new_csSubstsLogs in
    let new_log = propsString::new_CsLogs in
    ((res@new_csSubsts),(log@new_log))
  in
  Stringmap.fold find ([],[]) db.parts

(* Compute a set of context-sensitive substitutions for a normal reaction, relative to a database. *)
let matchNormalReactions (db:Database.t) (catalysts:string list list) (reactants:string list list)
                   (products:string list list) (rate:string) : Cssubst.t list * string list =

  (* Recursive function for searching the reactions database. *)
  let find ((res,log):Cssubst.t list * string list) (entry:Gecreaction.t Database.entry) : Cssubst.t list * string list =
    if not entry.enabled then (res,log) else
    let reac = entry.value in
    match Gecreaction.isNormal reac with
    | None -> (res,log)
    | Some(r_catalysts,r_reactants,r_products,r_rate) ->
      let log = log@["Trying reaction " + Lib.quote (Gecreaction.display reac)] in
      // All substitutions that unify the catalysts
      let thetasE = unifyComplexLists catalysts r_catalysts in
      let log = log@["thetasE = " + Lib.string_of_list Subst.display " ~~ " thetasE] in
      // Each substitution in thetasE may be expanded to multiple substitutions that also unify the reactants
      let thetasER = Lib.collect (fun theta -> let reactants = List.map (Subst.applyToComplex theta) reactants in
                                               let thetas' = unifyComplexLists reactants r_reactants in
                                               Lib.maybemap (Subst.union theta) thetas') thetasE
      in
      let log = log@["thetasER = " + Lib.string_of_list Subst.display " ~~ " thetasER] in
      // Each substitution in thetasER may be expanded to multiple substitutions that also unify the products
      let thetasERP = Lib.collect (fun theta -> let products = List.map (Subst.applyToComplex theta) products in
                                                let thetas' = unifyComplexLists products r_products in
                                                Lib.maybemap (Subst.union theta) thetas') thetasER
      in
      let log=log@["thetasERP = " + Lib.string_of_list Subst.display " ~~ " thetasERP] in
      // Each substution in thetasERP may be extended to also unify the rates
      let thetasERPr = Lib.maybemap (fun theta -> match Subst.unify rate (Subst.NUMBER r_rate) with
                                                  | None -> None
                                                  | Some theta' -> Subst.union theta theta') thetasERP
      in
      // Eliminate any duplicate results from the list of substitutions
      let thetasERPr = Lib.remove_duplicates Subst.eq thetasERPr in
      let log = log@["thetasERPr = " + Lib.string_of_list Subst.display " ~~ " thetasERPr] in
      let mkCSSubst (theta:Subst.t) : Cssubst.t option =
        let ground_reaction = Gecreaction.applySubst theta reac in
        let rho = Subst.speciesDomain theta in
        let sigma = Gecreaction.species ground_reaction in
        let cs = Cssubst.make theta rho sigma [] in
        if Cssubst.isOK cs then Some cs else None
      in
      let new_csSubsts = Lib.maybemap mkCSSubst thetasERPr in
      ((res@new_csSubsts),log)
  in
  Lib.fold_left find ([],[]) db.reactions

(* Compute a set of context-sensitive substitutions for a transport reaction, relative to a database. *)
let matchTransportReactions (db:Database.t) (reactant:string list) (product:string list) (rate:string)
                            (compartment:string) (direction:Ast.direction) : Cssubst.t list * string list =

  (* Recursive function for searching the reactions database. *)
  let find ((res,log):Cssubst.t list * string list) (entry:Gecreaction.t Database.entry) : Cssubst.t list * string list =
    if not entry.enabled then (res,log) else
    let reac = entry.value in
    match Gecreaction.isTransport reac with
    | None -> (res,log)
    | Some (r_reactant,r_product,r_rate,r_compartment,r_direction) ->
      let log = log@["Trying reaction " + Lib.quote (Gecreaction.display reac)] in
      if not(r_direction = direction) then (res,log@["...Directions don't match!"]) else
      // All substitutions that unify the reactant
      let thetasR = unifyComplexes reactant r_reactant in
      let log = log@["thetasR = " + Lib.string_of_list Subst.display " ~~ " thetasR] in
      // Each substitution in thetasR may be expanded to multiple substitutions that also unify the product
      let thetasRP = Lib.collect (fun theta -> let product = Subst.applyToComplex theta product in
                                               let thetas' = unifyComplexes product r_product in
                                               Lib.maybemap (Subst.union theta) thetas') thetasR
      in
      let log = log@["thetasRP = " + Lib.string_of_list Subst.display " ~~ " thetasRP] in
      // Each substution in thetasRP may be extended to also unify the rates
      let thetasRPr = Lib.maybemap (fun theta -> match Subst.unify rate (Subst.NUMBER r_rate) with
                                                 | None -> None
                                                 | Some theta' -> Subst.union theta theta') thetasRP
      in
      // Eliminate any duplicate results from the list of substitutions
      let thetasRPr = Lib.remove_duplicates Subst.eq thetasRPr in
      let log = log@["thetasRPr = " + Lib.string_of_list Subst.display" ~~ " thetasRPr] in
      let mkCSSubst (theta:Subst.t) : Cssubst.t option =
        let ground_reaction = Gecreaction.applySubst theta reac in
        let rho = Subst.speciesDomain theta in
        let sigma = Gecreaction.species ground_reaction in
        let cs = Cssubst.make theta rho sigma [] in
        if Cssubst.isOK cs then Some cs else None
      in
      let new_csSubsts = Lib.maybemap mkCSSubst thetasRPr in
      ((res@new_csSubsts),log)
  in
  Lib.fold_left find ([],[]) db.reactions
