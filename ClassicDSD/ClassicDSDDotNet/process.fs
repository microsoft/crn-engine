// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Process
open Microsoft.Research.DNA
open System.Diagnostics
open Microsoft.Research.CRNEngine
open Microsoft.Research.DNA.LogicDSD

type EngineRate<'a,'b> = Microsoft.Research.CRNEngine.Rate<'a,'b>
(*
module Expression = Microsoft.Research.CRNEngine.Expression
type EngineRate<'a,'b> = Microsoft.Research.CRNEngine.Rate<'a,'b>

type Key<'a> when 'a:equality = Microsoft.Research.CRNEngine.Key<'a>
module Lib = Microsoft.Research.CRNEngine.Lib
module Stringmap = Microsoft.Research.CRNEngine.Stringmap
module Hashtable = Microsoft.Research.CRNEngine.Hashtable
module Mset = Microsoft.Research.CRNEngine.Mset
*)
type value = Value.t
type value_env = Value.t_env
type domain = Domain.t
type strand = Strand.t
type gate = Gate.t
type origami = Origami.t
type species = Species.t
type species_env = Species.t_env
type parameter = Pattern.t

type prim = Microsoft.Research.CRNEngine.Expression.t<string> // Microsoft.Research.CRNEngine.Crn.value
type expression = Microsoft.Research.CRNEngine.Expression.t<Key<species>>
type sreaction = Microsoft.Research.CRNEngine.Reaction<species,prim,expression>
type spatialInitial = Microsoft.Research.CRNEngine.Spatial_initial.t

type dom_data =
  | DNA of string * Types.range
  | Rate of value
  | Colour of string * Types.range
  | Subdomains of (string list * bool) * Types.range
type dom_spec = string * dom_data
[<DebuggerDisplay("{DisplayMe()}")>] // displays a process in debug mode
type t = 
  | Repeat of value * bool * t * value option * Spatial_initial.t option (* amount, constant, p, time, spatial *)
  | Strand of strand * value option
  | Gate of gate * value option (* Should be deprecated for below, once at least solo reactions completed *)
  //| Complex of branch * value
  | Origami of t list * value option (*origami*)
  | Instance of string * Types.range * value list * value option (* module name, ?, module parameters, time *)
  | Parallel of t list
  | New of (string * dom_spec list) * t
  | Chemical of t Dsdreaction.mass_action_reaction
  | LogicDsdProcess of RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT> * value option
  member this.DisplayMe() = 
    let newline = "\n"
    let rec displays (p:t list) =
      match p with
      | [] -> ""
      | [p] -> p.DisplayMe()
      | p::ps -> p.DisplayMe() + "\n| " + displays ps 
    let displayTime (t : Value.t option) = 
      match t with 
      | Some time -> " @ " + Value.to_string time
      | None      -> ""
    match this with 
    | Repeat(i,b,p,t,sp) -> 
      match sp with 
      | None -> (if b then "constant " else "") 
                + Value.to_string i + p.DisplayMe() 
                + displayTime t
      | Some spInitial -> p.DisplayMe () + " with {"
                            + (["value = " + Value.to_string i
                                "constant = " + if b then "true" else "false"
                                "spatial = " + Spatial_initial.to_string spInitial]
                                 |> String.concat "; ")
                          + match t with Some time -> "; time = " + Value.to_string time | None -> ""                          
                          + "}"
    | Strand(s,t) -> Strand.display s + displayTime t
    | Gate(g,t) -> Gate.display g + displayTime t
    | Origami(o,t) -> (o |> Lib.string_of_list (fun x -> x.DisplayMe()) "|" |> Lib.brack) + displayTime t
    | Instance(x,_,vs,t) -> 
      match t with 
      | Some time -> x + Value.to_strings vs + " @ " + Value.to_string time 
      | None      -> x + Value.to_strings vs
    | Parallel([]) -> "()"
    | Parallel(ps) -> newline + "( " + displays ps + newline + ")" 
    | New((n,specs),p) -> 
      let display_spec s =
        match s with
        | (n, Rate v) ->  n + " = " + Value.to_string v
        | (n, DNA (s,_)) -> n + " = " + s
        | (n, Colour (c,_)) -> n + " = \"" + c + "\""
        | (n, Subdomains ((ds,false),_)) -> n + " = \"(" + Lib.string_of_list Lib.id "," ds + ")\""
        | (n, Subdomains ((ds,true),_)) -> n + " = \"^(" + Lib.string_of_list Lib.id "," ds + ")\"" in
      let rec display_specs slist =
        match slist with
        | [] -> ""
        | [s] -> display_spec s
        | s::specs -> (display_spec s) + (display_specs specs) in
      newline + "new " + n + (match specs with [] -> "" | specs -> "{" + (display_specs specs) + "}") +
      newline + p.DisplayMe()
    | Chemical mar ->
      let catalysts_str = Mset.to_string (fun (x:t) -> x.DisplayMe()) " + " mar.ma_catalysts in
      let reactants_str = Mset.to_string (fun (x:t) -> x.DisplayMe())" + " mar.ma_reactants in
      let products_str = Mset.to_string (fun (x:t) -> x.DisplayMe()) " + " mar.ma_products in
      let rate_to_string = function
        | Dsdreaction.Crn_ma v -> "{" + Expression.to_string id v + "}"
        | Dsdreaction.Crn_func e -> "{" + Expression.to_string (Key.to_string (fun (x:t) -> x.DisplayMe())) e + "}" in
      let rate_str = match mar.ma_reverseRate with
                     | None -> "->" + rate_to_string mar.ma_rate + "}"
                     | Some r -> "<->" + rate_to_string mar.ma_rate + rate_to_string r in
      catalysts_str + " ~ " + reactants_str + rate_str + products_str
    | LogicDsdProcess (p, t) -> sprintf "[%s] @ %s" (RulesDSD.Syntax.Process.ToStringWith Microsoft.Research.DNA.LogicDSD.engine p) (displayTime t)

type t_bind =
  Def of string * parameter list * (value list -> bool) * t
type t_env = t_bind list

type sim_species = species * float * bool

(* Function to map over the rates in a spec list *)
let dom_spec_map f (specs: dom_spec list) = 
  List.map (fun s -> match s with (n,Rate(v)) -> (n,Rate(f v)) | s->s) specs

(* Erase the position information from a process (i.e. set them all to "empty") .*)
let rec erasePosns = function
| Repeat(i,b,p,t,sp) -> Repeat(i,b,(erasePosns p),t,sp)
| Strand (s,t) -> Strand(Strand.erasePosns s,t)
| Gate (g,t) -> Gate(Gate.erasePosns g,t)
//| Complex (c,t) -> Complex(Branch.erasePosns c,t)
| Origami (o,t) -> (o |> List.map erasePosns, t) |> Origami
| Instance(x,_,vs,t) -> Instance(x,Types.emprange,(List.map Value.erasePosns vs),t)
| Parallel ps -> Parallel(List.map erasePosns ps)
| New((n,specs),p) -> New((n,
                           (List.map (fun s ->
                                           match s with 
                                           | (n,Rate(v)) -> (n,Rate(Value.erasePosns v))
                                           | (n,DNA(s,_)) -> (n,DNA(s,Types.emprange))
                                           | (n, Colour (c,_)) -> (n, Colour (c,Types.emprange))
                                           | (n, Subdomains (ds,_)) -> (n, Subdomains (ds,Types.emprange)))
                              specs)),
                          (erasePosns p))
| Chemical mar -> Chemical (Dsdreaction.map_mass_action_reaction erasePosns mar)
| LogicDsdProcess (p,t) -> LogicDsdProcess (p, t)

(* Replace the position info of certain variables with the supplied information.
 * This allows us to track positions correctly across module instantiations. *)
let rec replacePosns (mrs:Types.range Stringmap.t) = function
  | Repeat(i,b,p,t,sp) -> Repeat((Value.replacePosns mrs i),b,(replacePosns mrs p),t,sp)
  | Strand(s,t) -> Strand( Strand.replacePosns mrs s,t)
  | Gate(g,t) -> Gate(Gate.replacePosns mrs g,t)
  // | Complex(c,t) -> Complex(Branch.replacePosns mrs c,t)
  | Origami(o,t) -> (o |> List.map (replacePosns mrs),t) |> Origami
  | Parallel(ps) -> Parallel(List.map (replacePosns mrs) ps)
  | New((s,specs),p) ->
      let specs = (List.map (fun s ->
                               match s with 
                               | (n,Rate(v)) -> (n,Rate(Value.replacePosns mrs v)) 
                               | (n,DNA(s,p)) -> (n,DNA(s,p))
                               | (n, Colour (c,p)) -> (n, Colour (c,p))
                               | (n, Subdomains (ds,p)) -> (n, Subdomains (ds,p)))
                            specs)
      match (Stringmap.tryFind s mrs) with
      | Some _ -> let mrs = Stringmap.remove s mrs
                  New((s,specs),(replacePosns mrs p))
      | None   -> New((s,specs),(replacePosns mrs p))

  | Instance(x,rng,vs,t) ->
      let vs = List.map (Value.replacePosns mrs) vs in
      begin match (Stringmap.tryFind x mrs) with
        Some rng -> Instance(x,rng,vs,t)
      | None -> Instance(x,rng,vs,t) end
  | Chemical mar -> Chemical (Dsdreaction.map_mass_action_reaction (replacePosns mrs) mar)
  | LogicDsdProcess (p,t) -> LogicDsdProcess (p,t)

(** Produce a string representation of a process (or list of processes). *)
let rec display (p:t) = 
  let newline = "\n"//Lib.newline in
  let displayTime (t : Value.t option) = 
    match t with 
    | Some time -> " @ " + Value.to_string time
    | None      -> ""
  match p with
  | Repeat(i,b,p,t,sp) -> 
    match sp with 
    | None -> (if b then "constant " else "") + Value.to_string i + " * " + display p + displayTime t
    | Some spInitial -> 
      display p + " with {"
      + (
        [ "value = " + Value.to_string i
          "constant = " + if b then "true" else "false"
          "spatial = " + Spatial_initial.to_string spInitial ]
      |> String.concat "; ")
      + match t with Some time -> "; time = " + Value.to_string time | None -> ""
      + "}"

  | Strand(s,t) -> Strand.display s + displayTime t
  | Gate(g,t) -> Gate.display g + displayTime t
  | LogicDsdProcess (p,t) -> sprintf "[%s] %s" (RulesDSD.Syntax.Process.ToStringWith Microsoft.Research.DNA.LogicDSD.engine p) (displayTime t)
  // | Complex(b,t) -> Branch.display b + " @ " + Value.to_string t
  | Origami(o,t) -> "[[ " + String.concat " | " (List.map display o) + "]]" + displayTime t
  | Instance(x,_,vs,t) -> x + Value.to_strings vs + displayTime t
  | Parallel([]) -> "()"
  | Parallel(ps) -> "( " + displays ps + ")" 
  | New((n,specs),p) -> 
      let display_spec s =
        match s with
        | (n, Rate v) ->  n + " = " + Value.to_string v
        | (n, DNA (s,_)) -> n + " = " + s
        | (n, Colour (c,_)) -> n + " = \"" + c + "\""
        | (n, Subdomains ((ds,false),_)) -> n + " = \"(" + Lib.string_of_list Lib.id "," ds + ")\""
        | (n, Subdomains ((ds,true),_)) -> n + " = \"^(" + Lib.string_of_list Lib.id "," ds + ")\"" in
      let rec display_specs slist =
        match slist with
        | [] -> ""
        | [s] -> display_spec s
        | [("bind",   Rate b);("unbind", Rate u)] 
        | [("unbind", Rate u);("bind",   Rate b)] -> " @ " + Value.to_string b + ", " + Value.to_string u
        | _ -> "{" + String.concat "; " (List.map display_spec slist) + "}"
      "new " + n + (match specs with [] -> "" | specs -> display_specs specs) +
      newline + display p
  | Chemical mar ->
      let catalysts_str = if mar.ma_catalysts.IsEmpty
                            then ""
                            else Mset.to_string display " + " mar.ma_catalysts + " ~ " in
      let reactants_str = Mset.to_string display " + " mar.ma_reactants in
      let products_str = Mset.to_string display " + " mar.ma_products in
      let rate_to_string = function
        | Dsdreaction.Crn_ma v -> "{" + Expression.to_string id v + "}"
        | Dsdreaction.Crn_func e -> "[" + Expression.to_string (Key.to_string display) e + "]" in
      let rate_str = match mar.ma_reverseRate with
                     | None -> " ->" + rate_to_string mar.ma_rate// + "}"
                     | Some r -> " <->" + rate_to_string mar.ma_rate + rate_to_string r in
      "rxn " + catalysts_str + reactants_str + rate_str + " " + products_str
and displays (p:t list) =
  match p with
  | [] -> ""
  | [p] -> display p
  | p::ps -> display p + "\n| " + displays ps 

(* Add value/parameter bindings to the value environment for eval *)
let rec extend_env ms vs ev =
  match (ms,vs) with
  | ([],[]) -> ev
  | (Pattern.Name(x)::ms,v::vs) -> Value.Val(x,v)::(extend_env ms vs ev)
  | (Pattern.Pat(ps)::ms,Value.Tuple(ts,_)::vs) -> (extend_env ps ts (extend_env ms vs ev))
  | (Pattern.Pat(_)::_,_::_) -> failwith "Pattern parameter not given tuple"
  | ([],_::_) -> failwith "More values than parameters given to a module instance"
  | (_::_,[]) -> failwith "More parameters than values given to a module instance"

(** Find a module in the environment *)
let rec lookup (x:string) (e:t_env) =
  match e with
  | [] -> None
  | Def(x',ms,monitored,p)::e -> if (x=x') then Some(Def(x',ms,monitored,p)) else lookup x e

(* Generate a domain from a system*)
(* Assumes type system has checked that bind and unbind are floats *)
let eval_system (opts:Options.t) n c specs =
  let r = match (Lib.try_assoc "bind" specs) with 
          | Some (Rate v) -> Value.to_engineValue v
          | _ -> Expression.Float (Options.get_toehold_bind_rate opts) in
  let v = match (Lib.try_assoc "unbind" specs) with
          | Some (Rate v) -> Value.to_engineValue v
          | _ -> Expression.Float (Options.get_toehold_unbind_rate opts) in
  let d = match (Lib.try_assoc "seq" specs) with Some (DNA(s,_)) -> Some s | _ -> None in
  match d with 
    Some s -> Value.DomainS(n,c,s,r,v,Types.emprange)
  | None -> Value.Domain(n,c,r,v,Types.emprange) 

(** Evaluate all values within a process and modules *)
let rec eval (e:t_env) (ev:value_env) (p:t) =
  let eval_option env = Option.map (Value.eval env)
  match p with
  | Repeat(i,b,p,t,sp) -> Repeat(Value.eval ev i,b,eval e ev p,eval_option ev t, sp)
  | Strand(s,t) -> Strand(Strand.eval ev s,eval_option ev t)
  | Gate(g,t) -> Gate(Gate.eval ev g,eval_option ev t)
  // | Complex(b,t) -> Complex(Branch.eval ev b,Value.eval ev t)
  | Origami(o,t) -> (o |> List.map (eval e ev),eval_option ev t) |> Origami
  | Parallel(ps) -> Parallel(List.map (eval e ev) ps) 
  | New((n,specs),p) -> 
    let eval_specs = dom_spec_map (Value.eval ev) specs
    let domain = Value.Variable(n,Types.emprange)
    let p'= eval e (Value.Sys(n,domain)::ev) p
    New((n,eval_specs),p')
  | Instance(x,rng,vs,t) -> 
    let vs = List.map (Value.eval ev) vs
    let proc = lookup x e
    match proc with
    | Some(Def(_,ms,_,p)) -> eval e (extend_env ms vs ev) p
    | None -> Instance(x,rng,vs,eval_option ev t)
  | Chemical mar -> Chemical (Dsdreaction.map_mass_action_reaction (eval e ev) mar)
  | LogicDsdProcess (p, t) -> LogicDsdProcess (p, eval_option ev t) 

(** Compute the free names of a process .*)
let rec free_names (p:t) =
  let free_names_option topt = match topt with Some t -> Value.free_names t | None -> []
  match p with 
  | Repeat(i,_,p,t,_) -> (Value.free_names i) @ (free_names p) @ (free_names_option t)
  | Strand(s,t) -> Strand.free_names s @ (free_names_option t)
  | Gate(g,t) -> Gate.free_names g @ (free_names_option t)
  // | Complex(b,t) -> Branch.free_names b @ (Value.free_names t)
  | Origami(o,t) -> (o |> List.collect free_names) @ (free_names_option t)
  | Parallel(ps) -> List.collect free_names ps
  | New((s,specs),p) -> (List.collect (fun s -> match s with (_,Rate v) -> (Value.free_names v) | _ -> []) specs) @
                        (List.filter (fun (n,_) -> n<>s) (free_names p))
  | Instance(_,_,vs,t) -> List.append (free_names_option t) (List.collect Value.free_names vs)
  | Chemical mar -> List.concat ((Mset.collect free_names mar.ma_reactants) @ (Mset.collect free_names mar.ma_products) @ (Mset.collect free_names mar.ma_catalysts))
  | LogicDsdProcess (p, v) -> 
      p 
      |> RulesDSD.Syntax.Process.ToList
      |> List.collect (List.map (fun x -> 
          match x with 
          | Microsoft.Research.DNA.LogicDSD.Site (Microsoft.Research.DNA.LogicDSD.Bound (Microsoft.Research.DNA.LogicDSD.Dom d, _))
          | Microsoft.Research.DNA.LogicDSD.Site (Microsoft.Research.DNA.LogicDSD.Unbound (Microsoft.Research.DNA.LogicDSD.Dom d)) -> d.name, None
          | _ -> failwith "Unexpected uninstantiated Logic DSD process.")) 
      |> List.distinct

(** Parameterized definition of a function to check the specifications in a system
    Parameterized to avoid duplicating code between the two ways of type checking values **)
let system_checker check_val n specs fresh _ =
    let spec_ok = function
        | (("seq",DNA(_,r)),(Some _,_,_,_,_)) -> Errors.type_error r (Errors.RepeatedSpec (n,"seq"))
        | (("seq",DNA(s,r)),(None,b,u,c,ds)) -> if not fresh then (Some(DNA(s,r)),b,u,c,ds) else Errors.type_error r (Errors.NoSeq n)
        | (("bind",Rate(v)),(_,Some _,_,_,_)) -> Errors.type_error (Value.get_range v) (Errors.RepeatedSpec (n,"bind"))
        | (("bind",Rate(v)),(s,None,u,c,ds)) -> if check_val v "binding rate" then (s,Some(v),u,c,ds) else failwith ""
        | (("unbind",Rate(v)),(_,_,Some _,_,_)) -> Errors.type_error (Value.get_range v) (Errors.RepeatedSpec (n,"bind"))
        | (("unbind",Rate(v)),(s,b,None,c,ds)) -> if check_val v "unbinding rate" then (s,b,Some(v),c,ds) else failwith ""
        | (("colour", Colour (_, r)), (_,_,_,Some _,_)) -> Errors.type_error r (Errors.RepeatedSpec (n,"colour"))
        | (("colour", Colour (c, _)), (s,b,u,None,ds)) -> (s, b, u, Some c,ds)
        | (("subdomains", Subdomains (_, r)), (_,_,_,_,Some _)) -> Errors.type_error r (Errors.RepeatedSpec (n,"subdomains"))
        | (("subdomains", Subdomains (ds, _)), (s,b,u,c, None)) -> (s, b, u, c, Some ds)
        | ((d,_),_) -> Errors.type_error Types.emprange (Errors.UnknownSpec (n,d)) in
    let rec extract = function
        | [] -> (None,None,None,None,None)
        | s::specs -> spec_ok (s, extract specs) in
    extract specs

(* Check that a system is using proper specifications, while potentially updating the env *)
let type_check_system n specs fresh env = 
  let check_val v desc = 
   let t = Value.inferType env v in
    match !t with 
     | Types.FloatT _ -> true
     | Types.BottomT -> begin t:= (Types.FloatT (Value.get_range v)); true end
     | Types.TiedT(ts,_,_) -> let nt = Types.FloatT(Value.get_range v) in
                                 begin List.iter (fun t -> t:= nt) ts; t:= nt; true end
     | t -> Errors.type_error (Value.get_range v) (Errors.WrongType(t,[Types.FloatT Types.emprange],desc))
  in ignore ((system_checker check_val) n specs fresh env )

(** Infer types in a process, with checking instance calls at call site *)
let inferTypes in_origami (env:Types.type_env) (p:t) =
  let infer_time e v =
    match v with 
    | Some v' -> 
      let t = Value.inferType e v' in
      match !t with
      | Types.IntT _ | Types.FloatT _ | Types.BottomT -> ()
      | t -> let descr = "time of a species" in
             Errors.type_error (Value.get_range v') (Errors.WrongType(t,[(Types.IntT Types.emprange); (Types.FloatT Types.emprange)],descr))
    | None -> ()

  let rec infer in_origami (env:Types.type_env) = function
  | Repeat(i,_,p,_,_) ->
      let t = Value.inferType env i in 
      match !t with
        | Types.IntT _
        | Types.FloatT _
        | Types.TiedT(_,Some(true),_) ->
          infer in_origami env p
        | Types.BottomT ->
          t := Types.TiedT([],Some(true),[Value.get_range(i)])
          infer in_origami env p
        | Types.TiedT(ts,_,_) ->
          let nt = Types.TiedT(ts,Some(true),[Value.get_range(i)])
          List.iter (fun t -> t:= nt) ts
          t := nt
          infer in_origami env p
        | t ->
          let descr = "population of a species"
          Errors.type_error (Value.get_range i) (Errors.WrongType(t,[(Types.IntT Types.emprange);(Types.FloatT Types.emprange)],descr))

  | Strand(s,t) -> Strand.inferType in_origami env s; infer_time env t
  | Gate(g,t) -> Gate.inferType in_origami env g; infer_time env t
  // | Complex(b,t) -> Branch.inferType env b; infer_time env t
  | Origami(o,t) -> o |> List.iter (infer (Some true) env); infer_time env t
  | Parallel(ps) -> List.iter (infer in_origami env) ps
  | New((s,specs),p) ->
    type_check_system s specs false env |> ignore
    infer in_origami ((s,(ref (Types.BottomT)))::env) p
  | Instance(x,rng,vs,t) -> 
      let mod_t = Value.inferType env (Value.Variable(x,rng)) in
      let ts = List.map (Value.inferType env) vs in
      begin match !mod_t with
        | Types.ModuleT(p_types)  -> Pattern.check_parameter_types env x rng p_types ts
        | t -> let descr = "module name " + x in
               Errors.type_error rng (Errors.WrongType(t,[Types.ModuleT ts],descr))
      end;
      infer_time env t
  | Chemical mar ->
      let i ss = Mset.iter (infer in_origami env) ss in
      i mar.ma_reactants; i mar.ma_products; i mar.ma_catalysts
  | LogicDsdProcess _ -> ()
  in
  infer in_origami env p


(** Evaluate a process to a list of species *)
let eval_to_species (opts:Options.t) (names:string list, counter:int) (mod_env:t_env) (env:value_env) (p:t) = 
  let mt = Species.empty_env 0
  let t_default = None

  let rec eval fresh_names env p population constant species_acc reactions_acc monitored m_name t_init spatial =

    let single t st =
      let time = 
        match t_init with 
        | Some ti -> ti |> Value.eval env |> Value.to_engineValue |> Some
        | None    -> Option.map (Value.eval env >> Value.to_engineValue) t
      let add_env m_name e = Species.add_env opts m_name (st,population,constant,monitored,time,spatial) e
      (fresh_names, add_env m_name species_acc, reactions_acc)

    let fix s =
      let ss = Hashtable.empty () in
      let add ((x, _, _, _,_,_): Species.t_map) = Hashtable.add ss x () in
      Stringmap.iter (fun _ sps -> List.iter add sps) (fst species_acc);
      let r = Species.rotate opts s in
      match Hashtable.tryFind ss r with
      | None -> s
      | Some () -> r

    match p with
    | Repeat(i,b,p,t,sp) -> 
        let pval = Value.eval env i in (* Needed to evalue parameters to process definitions *)
        let repeat_pop = Value.to_engineValue pval in
(*        match pval with
                         | Value.Int(i,_) -> Expression.Float (float_of_int i)
                         | Value.Float(f,_) -> Expression.Float f
                         | Value.Variable (v, _) -> Expression.Variable v
                         | _ -> failwith "typecheck failed" in *)
        (* --- NB! This disables repeats of restricted species (which should not happen) ---
                          --- TODO: Update Process.t to refelct this ---
        *)
        let constant_n = constant || b in    
        eval fresh_names env p (Expression.mul population repeat_pop) constant_n species_acc reactions_acc monitored m_name t sp
    | Strand(s,t) -> 
        let st = Species.STRAND (Strand.standard_form (Strand.eval env s)) |> fix in
        single t st
    | Gate(g,t) -> 
        let gt = Species.GATE (Gate.standard_form opts (Gate.eval env g)) in
        single t gt
    | LogicDsdProcess(p, t) -> 
        let p' = Species.LogicDsdProcess p in
        single t p'
(*  
    | Complex(b,t) -> let bt = Species.COMPLEX (Branch.standard_form opts (Branch.eval env b)) in
                    single t bt
*)  
    | Origami(o,t) -> 
        let (_,(s_env,_),_) = Lib.fold_left (fun (fresh_names,species_acc,reactions_acc) p -> 
          (eval fresh_names env p population constant species_acc reactions_acc monitored m_name t_default spatial)) (fresh_names,mt,[]) o in
        let m = (Stringmap.fold (fun l _ sps -> sps@l) [] s_env) in
        let to_content = function
          | Species.STRAND s -> Origami.C_strand s
          | Species.GATE g -> Origami.C_gate g
          | _ -> failwith "Invalid origami content" in
        let content = m |> List.map (fun (s,_,_,_,_,_) -> s |> to_content) in
        content |> Origami.standard_form opts
                |> Species.ORIGAMI
                |> single t

    | Parallel(ps) -> 
        Lib.fold_left (fun (fresh_names,species_acc,reactions_acc) p -> 
          (eval fresh_names env p population constant species_acc reactions_acc monitored m_name t_init spatial)
        ) (fresh_names,species_acc,reactions_acc) ps  
    | New((n,specs),p) -> 
      let eval_specs = dom_spec_map (Value.eval env) specs in
      let (names,counter) = fresh_names in
      let (names,counter,c) = if List.contains n names then (names,counter+1,counter+1) else ((n::names),counter,0) in
      let domain = eval_system opts n c eval_specs in
      eval (names,counter) (Value.Sys(n,domain)::env) p population constant species_acc reactions_acc monitored m_name t_init spatial

    | Instance(x,rng,vs,t) ->
      let rec find_proc name rng vs env monitored = 
        let vs = List.map (Value.eval env) vs in
        let proc = lookup name mod_env in
        match proc with
        | Some(Def(_,ms,monitor_check,Instance(y,rng,yvs,_))) -> (* def A(x) = B(x,1-x) should result in A(3) being named A rather than B *)        
          let monitor = monitor_check vs in
          find_proc y rng yvs (extend_env ms vs env) (monitor || monitored)
        | Some(Def(_,ms,monitor_check,p)) -> 
          let monitor = monitor_check vs in
          p, (extend_env ms vs env), monitor || monitored
        | None -> Errors.type_error rng (Errors.UnknownModule (x, Value.to_strings vs)) in
      let p, env, monitored = find_proc x rng vs env monitored in
      eval fresh_names env p population constant species_acc reactions_acc monitored x t spatial
(*  
      let vs = List.map (Value.eval env) vs in
      let proc = lookup x mod_env in
      match proc with
      | Some(Def(x,ms,monitor_check,p)) -> 
        let monitor = monitor_check vs in
        eval fresh_names (extend_env ms vs env) p population constant species_acc reactions_acc (monitor || monitored) x 
      | None -> Errors.type_error rng (Errors.UnknownModule (x, Value.to_strings vs))
*)  

    | Chemical mar ->
      let eval_single s =
        let (_, (m, _), _) = eval fresh_names env s population constant mt [] monitored m_name t_init spatial in
        List.map (fun (x, _, _, _,_,_) -> fix (Species.standard_form opts x)) (Stringmap.fold (fun l _ x -> x@l) [] m) in
      let convert_mset m =
        let eq = Species.equal opts in
        let lm = Mset.map eval_single m in
        Mset.fold_left_m (fun acc {Mset.element = l; Mset.multiplicity = n} -> Lib.fold_left (Mset.add eq) acc (List.map (fun x ->{Mset.element = x; Mset.multiplicity = n}) l)) Mset.empty lm in
      let rec convert_rate = function
        | Expression.Key s ->
          begin
            let is_s = function
              | Value.Val (n,_) -> n = s
              | _ -> false in
            match List.tryFind is_s env with
            | None -> Expression.Key s
            | Some (Value.Val (_, v)) -> Value.to_engineValue v
            | Some _ -> Expression.Key s
          end
        | Expression.Plus fs -> fs |> List.map convert_rate |> Expression.Plus
        | Expression.Minus {sub1 = f1; sub2 = f2} -> Expression.sub (convert_rate f1) (convert_rate f2)
        | Expression.Times fs -> fs |> List.map convert_rate |> Expression.Times
        | Expression.Divide {div1 = f1; div2 = f2} -> Expression.div (convert_rate f1) (convert_rate f2)
        //| Expression.Floor f -> Expression.norm_floor (convert_rate f)
        | Expression.Power {base_ = f1; exponent = f2} -> Expression.power (convert_rate f1) (convert_rate f2)
        | Expression.Ceiling c -> Expression.Ceiling  (convert_rate c)
        | Expression.Floor c -> Expression.Floor (convert_rate c)
        | v -> v in
      let convert_rate = function
        | Dsdreaction.Crn_ma v -> convert_rate v |> EngineRate.MassAction
        | Dsdreaction.Crn_func e ->
          let f = function
                  | Key.Species s   -> s |> eval_single |> List.map (Key.Species >> Expression.Key) |> Expression.Plus
                  | Key.Parameter p -> Expression.Key (Key.Parameter p) 
                  | Key.Time        -> Expression.Key Key.Time
                  | Key.Rate r      -> Expression.Key (Key.Rate r)
                  in
          //let ft s v = s |> eval_single |> List.map (fun x -> Expression.TimedPopAExp (x, v)) |> Expression.SumAExp in
          let se = Expression.expand f e in
          EngineRate.Function (se) in (* RLP: handle variables for this case also. Blocked by DSD syntax for lower strands. *)
      let reaction = 
        Reaction.create(
          convert_mset mar.ma_catalysts, 
          convert_mset mar.ma_reactants,
          Lib.option_map2 convert_rate mar.ma_reverseRate,
          convert_rate mar.ma_rate,
          convert_mset mar.ma_products
        )
      (fresh_names, species_acc, reaction::reactions_acc) (* this adds no species *)

  eval (names,counter) env p (Expression.Float 1.0) false mt [] false "%" t_default None

let zero = Value.Float (0.0, Types.emprange)

let from_content = function
  | Origami.C_gate g -> Gate (g, None)
  | Origami.C_strand s -> Strand (s, None)
let from_species t = function
  | Species.GATE g -> Gate (g, t)
  | Species.STRAND s -> Strand (s, t)
  //| Species.COMPLEX c -> Complex (c, t)
  | Species.ORIGAMI o -> (o |> List.map from_content, t) |> Origami
  | Species.UNKNOWN u -> failwith ("Unknown species " + u + " cannot be converted to process")
  | Species.LogicDsdProcess p -> LogicDsdProcess (p,t)
(******************************************************************************)
