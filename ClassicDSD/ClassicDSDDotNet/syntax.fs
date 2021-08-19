[<JavaScript>]
module Microsoft.Research.DNA.Syntax
open Microsoft.Research.DNA

module Lib = Microsoft.Research.CRNEngine.Lib
//module Settings = Microsoft.Research.CRNEngine.Settings
//module Plottable = Microsoft.Research.CRNEngine.Plottable
//module ParseResult = Microsoft.Research.CRNEngine.ParseResult
module Expressions = Microsoft.Research.CRNEngine.Expression
type Key<'a> when 'a:equality = Microsoft.Research.CRNEngine.Key<'a>

type value = Value.t
type value_env = Value.t_env
type dom_assignment = (* add bind and unbind here *)
  { domain : string
  ; dom_colour : string option
  ; subdomains : (string list * bool) option } (* subdomains, is_short *)
type dom_env = dom_assignment list
type key = string
type parameter = Pattern.t
type proc = Process.t
type proc_env = Process.t_env
type spec = Process.dom_data
type species = Species.t

(* The type of DSD plot specifications, listing items to monitor *)
type plot_pat =
  | All
  | Pats of value list (* value list may contain wildcards denoted with _ *)

type t_plot_base = 
  | String of string
  | Molecule of Species.t 
  | Module of string * plot_pat
type t_plot = t_plot_base Expressions.t

//let t_dummy = Value.Float (0.0, Types.emprange)
let t_dummy = None
let t_plot_to_expr p =
  let f = function
  | Molecule s          -> Process.from_species t_dummy s
  | Module (m, Pats ps) -> Process.Instance (m, Types.emprange, ps, t_dummy)
  | _                   -> failwith "Can only convert species" in
  Expressions.map f p

(* The type of DSD program syntax. *)
type t = 
  | New         of ((string * (string * spec) list) * bool * t)
  | Definition  of (string * Types.range * parameter list * proc * t)
  | Value       of (string * value * t)
  | Process     of proc
//  | Predicate of string * t_plot_base Expressions.pred * t
//  | Query of string * (species ParseResult.settings_update list option * t_plot_base Expressions.pred) * t
(*
   t_plots have to be turned into plottables after parsing
   when the environment (declared domains) is known, so we
   gather them in this structure during parsing.
*)
type event =
  | Ev_species of Process.t * value * value (* species, amount, time *)
  //| Ev_param of SettingsConstants.filzparam * value * value (* parameter, new value, time *)
  | Ev_endtime of value

type parsed_plots =
  { plottables : t_plot list
  ; sweep_fits : (string * string * t_plot list) list
  ; events : event list }
  (* here we could add spatial plots *)
let empty_parsed_plots = { plottables = []; sweep_fits = []; events = [] }

(* Remove string keys from the supplied list of pairs. This is used in the "free names" function below. *)
let remove_pairlist (l:string list) (xs:(string * 'a) list) =
  match l,xs with
  | _,[] -> []
  | [],xs -> xs
  | l,xs -> List.filter (fun (x,_) -> not(Lib.contains x l)) xs

(* Compute the free names of a program. *)
let rec free_names (s:t) =
  match s with
  | New((n,specs),_,s) -> 
       (List.collect (fun s -> match s with (_,Process.Rate v) -> (Value.free_names v) | _ -> []) specs)
       @ (remove_pairlist [n] (free_names s))
  | Value(n,v,s) -> (Value.free_names v) @ (remove_pairlist [n] (free_names s)) 
  | Definition(x,_,ms,p,s) -> (remove_pairlist (x::(Pattern.toStringList ms)) (Process.free_names p)) @ (remove_pairlist [x] (free_names s))
  | Process(p) -> Process.free_names p

(* Infer types and return exceptions to report errors. *)
let rec inferTypes (env:Types.type_env) = function
  | New((n,specs),fresh,s) ->
      Process.type_check_system n specs fresh env
      inferTypes ((n,(ref (Types.DomainT (None,Types.emprange))))::env) s
  | Value(n,v,s) -> 
      let t = Value.inferType env v in
      inferTypes ((n,t)::env) s
  | Definition(x,_(*rng*),ms,p,s) ->
      let parm_types = Pattern.toTypes(ms) in
      let parm_list = Pattern.toTypeEnv(parm_types,ms) in
      Process.inferTypes None (parm_list @ env) p
      inferTypes ((x,(ref (Types.ModuleT parm_types)))::env) s
  | Process(p) -> Process.inferTypes (Some false) env p


(* A function to erase position information. *)
let rec erasePosns = function
    New((n,specs),fresh,s) -> New((n,Process.dom_spec_map Value.erasePosns specs),fresh,(erasePosns s))
  | Value(n,v,s) -> Value(n,(Value.erasePosns v),(erasePosns s))
  | Definition(x,_(*rng*),ms,p,s) -> Definition(x,Types.emprange,ms,(Process.erasePosns p),(erasePosns s))
  | Process(p) -> Process(Process.erasePosns p)

let rec extract_modules (n:string) = function
   [] -> []
 | Module(x,pat)::modules -> if x = n then pat::(extract_modules n modules) else extract_modules n modules
 | _::_ -> failwith "A non-module in module-only list"

(* A function that filters module plot patterns, generates a function to check the pattern, and add to the proc_env *)
let rec add_plot_monitors (env:value_env) (toplot:t_plot_base list) = function
    [] -> [] 
  | Process.Def(x,ps,_(*monitor*),p)::defs ->
    (let patterns = extract_modules x toplot in
     match List.tryFind (fun x -> match x with All -> true | _ -> false) patterns with
       Some _ -> Process.Def(x,ps,(fun _ -> true),p)
     | None ->
       let env = (Value.Val("_",Value.Variable("_",Types.emprange))::env) in
       let num_ps = List.length ps in
       let allIn fv env = Lib.fold_left (&&) true (List.map (Value.varInEnv env) fv) in
       let rec removeInvalidPats (ls:value list list) fvs =
         match (ls,fvs) with 
         | [],[] -> []
         | l::ls,fv::fvs -> 
           if (allIn fv env) && (List.length l = num_ps) then
             l::(removeInvalidPats ls fvs) 
           else (removeInvalidPats ls fvs) 
         | _,_ -> failwith "removeInvalidPats given strings of unequal length" in
        let pats = (List.map (fun a -> match a with Pats v -> v | All -> []) patterns) in
        let free_vars = List.map (fun l -> Lib.remove_duplicates (=) 
                                             (List.map fst (Lib.fold_left (fun sl v -> (Value.free_names v)@sl) [] l)))
                              pats in 
        let valid_pats = removeInvalidPats pats free_vars in
        let eval_plot_pats = List.map (fun l -> List.map (Value.eval env) l) valid_pats in
        Process.Def(x,ps,(fun vals ->
                       List.fold (||) false
                        (List.map (fun pat -> 
                        (List.fold (&&) true (List.map2 (fun v v2 -> (Value.matching (v,v2))) pat vals)))
                        eval_plot_pats)), p))
        ::add_plot_monitors env toplot defs



(* Expand syntax into the environments, type check, and remove position info *)
let expand (opts:Options.t) base_env (plot_modules: t_plot_base list) (s:t) = 
  (* A function to close the program with additional "new" declarations (at the default rate) to quantify out the free names. *)
  let quantify n s = New((n,[]),false,s) in
  (* Only quantify out the free names if the appropriate option is enabled. *)
  let s = if Options.getDeclareDomains opts 
            then s
            else let to_be_newed = Lib.remove_duplicates (=) (List.filter (fun n -> List.forall (fun (bn, _) -> n <> bn) base_env) (List.map fst (free_names s)))
                 Lib.fold_left (fun s n -> quantify n s) s to_be_newed
  (* Check for type errors now. This function raises an exception if it finds an error. *)
  inferTypes base_env s |> ignore
  (* Erase the position information.
   * THIS IS IMPORTANT SO STRUCTURAL EQUALITY CAN BE USED IN THE SIMULATOR! *)
  let s = erasePosns s
  (* The function which does the expanding. *)
  let rec do_expand (counter:int) (names:string list) (e:proc_env) (ev:value_env) (ce:dom_env) (s:t) (*ps qs*) =
    match s with
    | New((n,specs),_(*fresh*),s) -> 
        let (counter:int),(c:int),(names:string list) = 
          if (*fresh*) true then
            if List.contains n names then
              counter+1,counter+1,names 
            else counter,0,n::names
           else counter,0,names

        let especs = Process.dom_spec_map (Value.eval ev) specs in
        let dspec =
          { domain = n
          ; dom_colour = List.tryPick (function (_, Process.Colour (c, _)) -> Some c | _ -> None) especs
          ; subdomains = List.tryPick (function (_, Process.Subdomains (ds, _)) -> Some ds | _ -> None) especs }
        do_expand counter names e (Value.Sys(n,Process.eval_system opts n c especs)::ev) (dspec::ce) s (*ps qs*)
    | Value(n,v,s) -> do_expand counter names e (Value.Val(n, Value.eval ev v)::ev) ce s (*ps qs*)
    | Definition(x,_,ms,p,s) -> do_expand counter names (Process.Def(x,ms,(fun _ -> false),p)::e) ev ce s (*ps qs*)
    | Process(p) -> counter,names,p,e,ev,ce(*,ps,qs*)
    (*
    | Predicate (n, exp, s) -> expand counter names e ev ce s ((n,exp)::ps) qs
    | Query (n, exp, s) -> expand counter names e ev ce s ps ((n,exp)::qs)
    *)

  (* Actually call the expander function. *)
  List.map fst base_env |> ignore  // env_names
  let s_names = List.map fst (free_names s)
  let (counter,names,p,e,ev,ce(*,ps,qs*)) = do_expand 0 s_names [] [] [] s (*[] []*)
  (counter,names,p,(add_plot_monitors ev plot_modules e),ev,ce(*,ps,qs*))

let rec t_plot_base_to_species (eval_proc:Process.t -> Species.t_map list) = function
  | String s -> failwith ("Cannot map plottable string: " + s)
  | Molecule p -> p
  | Module (m, Pats vs) -> (*failwith "Module not removed from plot list"*)
    let p = Process.Instance (m, Types.emprange, vs, t_dummy) in
    let assert_one = function
      | Expressions.Float 1.0 -> ()
      | v -> failwith ("Multiplicity " + (Expressions.to_string id v) + " in plotted module " + m + (" (only 1.0 allowed).")) in
    (match List.map (fun (s, v, _:bool, _:bool, _, _) -> assert_one v; t_plot_base_to_species eval_proc (Molecule s)) (eval_proc p) with
    | [s] -> s
    | _ -> failwith ("Plotted module " + m + " produces more than one species"))
  | Module (m, _) -> failwith ("Module " + m + " not removed from plot list")

let rec plot_map eval_proc (p: t_plot ) : species Expressions.t =
  p |> Expressions.map (t_plot_base_to_species eval_proc >> Species.erasePos >> Species.universal_counters)

(* Separates modules to plot from molecules. Currently does not allow a Module within an operation *)
let rec extractModules (plots: t_plot list) =
  match plots with
  | [] -> [], []
  | Expressions.Key (Module (s, All))::plots ->
    let without, withm = extractModules plots
    without, Module(s, All)::withm
  | v::plots ->
    let without, withm = extractModules plots
    v::without, withm

let add_to_plots (sps : Species.t_env) (plts : t_plot list) = (List.map (Molecule >> Expressions.Key) (Species.env_to_plot_species sps)) @ plts

let rec display (syn : t ) = 
  match syn with
  | New         ((name, ps), isNew, syn') -> 
    let newPar (name, p) = 
      name + " = " + match p with 
                      | Process.DNA(s, _)    -> s
                      | Process.Colour(s, _) -> "\"" + s + "\""
                      | Process.Rate var     -> Value.to_string var
                      | Process.Subdomains ((d, s), _) -> "[ "+ String.concat "; " d + if s then "*" else "" + " ]"
    let parameters = 
      match ps with 
      | []      -> ""
      | [("bind",Process.Rate(b));("unbind",Process.Rate(u))] 
      | [("unbind",Process.Rate(u));("bind",Process.Rate(b))] -> " @ " + Value.to_string b + ", " + Value.to_string u
      | _ -> " = {" + String.concat "; " (List.map newPar ps) + "}"
    let newOrDom = if isNew then "new" else "dom"
    newOrDom + " " + name + " " + parameters + "\n" + display syn'
  | Definition  (name, _, ps, p, syn') -> 
    let pars = "(" + String.concat ", " (Pattern.toStringList ps) + ")"
    let innerProc = Process.display p 
    let pDisplay = if innerProc.Contains "\n"
                    then ("\n  (" + innerProc.Replace("\n", "\n  ")) + ")"
                    else innerProc
    "def " + name + pars + " = "+ pDisplay + "\n" + display syn'
  | Value       (var, value, syn') -> "def " + var + " = " + Value.to_string value + "\n" + display syn'
  | Process     p -> Process.display p