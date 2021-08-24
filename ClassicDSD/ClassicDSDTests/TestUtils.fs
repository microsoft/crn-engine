// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.TestUtils

// open FsUnit.Xunit
open FsUnit
open Xunit

open System.IO
//open System.Xml
//open System.Xml.XPath

open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Rng

(* This file has been copied over from DsdTests.TestUtils.fs in the DNA solution. *)
//module BMEUI = Microsoft.Research.ModellingEngine.Ui
module BMEValue = Microsoft.Research.CRNEngine.Expression//s.Value
//module Term = Microsoft.Research.ModellingEngine.Term
module Lib = Microsoft.Research.CRNEngine.Lib
module Hashtable = Microsoft.Research.CRNEngine.Hashtable
module Mset      = Microsoft.Research.CRNEngine.Mset

type value = Microsoft.Research.CRNEngine.Expression.t<string>

(**new part**)
/// Custom operator for combining paths (from: http://www.fssnip.net/1g/title/Working-with-paths)
let (+/) path1 path2 = Path.Combine(path1, path2)

let TEST_FOLDER = "testData"

let CLASSIC_DSD         = "classicDsd"
let CRN_DIR             = "crns"
let VISUAL_DSD          = "visualDsd"

(**/new part**)

//-------------
// Debug output
let debug s = //()
    System.Diagnostics.Debug.WriteLine s
    System.Console.WriteLine s

let debugm s = //()
    let col = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.DarkBlue
    System.Diagnostics.Debug.WriteLine s
    System.Console.WriteLine s
    System.Console.ForegroundColor <- col

//-------------
// IO

// Reads a file included as EmbeddedResource. If it has a path, only the file name should be given to this method.
let read_resource resourceName : string =
    use stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
    use reader = new System.IO.StreamReader(stream)
    reader.ReadToEnd ()
         
let get_file resourceName = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName+".txt")

let inline is_null x = x = Unchecked.defaultof<_>

//let get_example_node (node: XmlNode) =
//    let path_node = node.Attributes.GetNamedItem("Path")
//    if is_null path_node then node.InnerText
//    else
//      let path = System.IO.Path.GetFileName path_node.InnerText
//      read_resource path

// curry and uncurry a function, useful function combinator e.g. for List.zip
let curry   f a b   = f (a,b)
let uncurry f (a,b) = f a b

// checks whether all values in a Bool list are true (copied from Haskell's and)
let allTrue = List.fold (&&) true

// removes elements in xs from an input string (filter's implicit parameter )
let trim xs = String.filter (fun x -> List.exists ((=) x) xs)

let programText exampleName =
    (*let examplesXML = read_resource "Examples_inlined.xml"

    let doc = XmlDocument()
    doc.LoadXml examplesXML

    let exampleNode = doc.SelectSingleNode ("//Example[@Name='" + exampleName + "']")
    get_example_node exampleNode*)

    let projectDir = read_resource "projectDir.value" |> trim [' ';'\r';'\n';'\t']
    let examplesPath = Path.ChangeExtension(Path.Combine (projectDir, "../WebDNA/Examples/", exampleName), ".dna")

    if not (File.Exists examplesPath) then failwithf "Example '%s' not found at %s" exampleName examplesPath

    File.ReadAllText examplesPath

let forall_examples f =
    let projectDir = read_resource "projectDir.value" |> trim [' ';'\r';'\n';'\t']
    let examplesDir = Path.Combine (projectDir, "../WebDNA/Examples/")
    let dsd_programs = Directory.EnumerateFiles (examplesDir, "*.dna")
    debug (sprintf "Found %d DSD programs" (Seq.length dsd_programs))

    dsd_programs (*
        |> Seq.skip 51
        |> Seq.take 1*)
        |> Seq.iter (fun file_name -> let content = File.ReadAllText file_name
                                      f file_name content)

let all_examples =
    let l = ref []
    let append name text = l := (name, text)::!l
    forall_examples append;
    List.rev !l

let some_examples k =
    let n = List.length all_examples
    let rec get missing left acc = function
      | [] -> acc
      | x::xs ->
        if missing = 0 then acc
        else if left < missing then x::xs @ acc
        else if (new Random()).Next(left) < missing
        then get (missing-1) (left-1) (x::acc) xs
        else get missing (left-1) acc xs
    get k n [] all_examples


let check b m = 
  if not b 
    then failwith (m ())
    else ()

let check_permutation eq l1 l2 m n1 n2 =
  let check_in_l2 e1 = 
    let inSpec = List.exists (eq e1) l2
    check inSpec (m e1 n2)
  let check_in_l1 e2 = 
    let inImpl = List.exists (eq e2) l1
    check inImpl (m e2 n1)
    
  l1 |> List.iter check_in_l2
  l2 |> List.iter check_in_l1

//-------------
// Compare values
let delta = 0.001
let float_eq (f1 : float<'a>) (f2 : float<'a>) =
    let test a b = abs((a / b) - 1.0) <= delta
    if abs f1 > abs f2 then test f2 f1 // do not divide by 0.0
    elif abs f2 > abs f1 then test f1 f2 // do not divide by 0.0
    else f1 = f2 // 0.0 = 0.0 handled in this case

// structural equality
let rec value_eq e1 e2 =
  let e1', e2' = Expression.simplify e1, Expression.simplify e2 
  match e1', e2' with
          | BMEValue.Key   n, BMEValue.Key m   -> n  = m
          | BMEValue.Float n, BMEValue.Float m -> float_eq n m
          | BMEValue.Plus   ns, BMEValue.Plus ms
          | BMEValue.Times  ns, BMEValue.Times ms 
            -> try
                check_permutation value_eq ns ms (fun e1 e2 _ -> "Sum or multiplication mismatch") "implementation" "specification" 
                  |> ignore 
                true 
               with _ -> false
               
          | BMEValue.Minus  { sub1 = n1; sub2 = n2 }, 
            BMEValue.Minus  { sub1 = m1; sub2 = m2 }
          | BMEValue.Divide { div1 = n1; div2 = n2 }, 
            BMEValue.Divide { div1 = m1; div2 = m2 }
          | BMEValue.Power { base_ = n1; exponent = n2}, 
            BMEValue.Power { base_ = m1; exponent = m2}   
            -> value_eq n1 m1 && value_eq n2 m2
          | BMEValue.Absolute n, BMEValue.Absolute m
            -> value_eq n m
          | _, _ -> false

//let value_equal v1 v2 = value_eq (BMEValue.normalize v1) (BMEValue.normalize v2)

//-------------
// Compare populations
(*let pop_info_equal opts (pi1: Dsd.pop_info) (pi2: Dsd.pop_info) =
     (pi1.constant = pi2.constant)
  && (pi1.initial = pi2.initial)
  && (pi1.input = pi2.input)
  && (value_equal pi1.population pi1.population)
  && (Species.equal opts pi1.calculus_species pi2.calculus_species)
*)
let eqSpecies (s1 : Microsoft.Research.CRNEngine.Species) (s2 : Microsoft.Research.CRNEngine.Species) = s1.name = s2.name

// Ignores initial and input // TODO: why ignore the initial?
let eqInitials (in1: Initial<Microsoft.Research.CRNEngine.Species, value>) (in2: Microsoft.Research.CRNEngine.Initial<Species, value>) =
     in1.constant = in2.constant
  && value_eq in1.value in2.value
  && eqSpecies (in1.species) (in2.species)
  && match in1.time, in2.time with
     | Some t1, Some t2 -> value_eq t1 t2
     | None, None       -> true
     | _, _             -> false
  
let check_pops_equal_relaxed (c1 : Crn) (c2 : Crn) =
  let errorMsg (i:Initial<Species,  Expression.t<string>>) crnName (_ : Unit) = 
    i.species.name + " not found in " + crnName
  check_permutation eqInitials c1.initials c2.initials errorMsg c1.name c2.name  

//-------------
// Compare reactions
let rate_equal eq r1 r2 =
    match r1, r2 with
    | (Rate.MassAction v1), (Rate.MassAction v2) -> eq v1 v2
    | _ -> false

let opt_equal eq o1 o2 =
    match o1, o2 with
    | None, None -> true
    | Some v1, Some v2 -> eq v1 v2
    | _ -> false

let reaction_equal seq veq (r1: Reaction<'species,'value,'eq>) (r2: Reaction<'species,'value,'eq>) =
    let sameProducts      = Mset.is_perm seq r1.products  r2.products  
    let sameReactants     = Mset.is_perm seq r1.reactants r2.reactants
    let sameCatalysts     = Mset.is_perm seq r1.catalysts r2.catalysts
    let sameRates         = rate_equal veq r1.rate r2.rate
    let sameReverseRates  = opt_equal (rate_equal veq) r1.reverse r2.reverse
    in
    sameProducts 
    && sameReactants 
    && sameCatalysts
    && sameRates 
    && sameReverseRates
    
let reversible_reaction_equal seq veq r1 r2 =
    let isEq = reaction_equal seq veq r1 r2 
    let isReverseEq =
        match r1.reverse_reaction() with
        | None     -> false // the reaction is not reversible
        | Some r1' -> reaction_equal seq veq r1' r2
    isEq || isReverseEq

let reversibles_equal veq opts = Lib.is_permutation (reversible_reaction_equal veq opts)

let check_reversibles_equal (c1 : Crn) (c2 : Crn) =
  // check if c1 and c2 have the same number of reactions
  let diff         = c1.reactions.Length.CompareTo c2.reactions.Length
  let errorMsg a b = a + " has more reactions than " + b
  if   diff > 0 then failwith (errorMsg c1.name c2.name)
  elif diff < 0 then failwith (errorMsg c2.name c1.name)
  else 
    // check that each reaction in c1 has an equivalent in c2
    check_permutation
      (reversible_reaction_equal (fun (s1:Species) (s2:Species) -> s1.name = s2.name) (value_eq))
      c1.reactions 
      c2.reactions
      (fun r x _ -> let vPrinter = Expression.to_string id
                    let ePrinter = Expression.to_string (Key.to_string Species.to_string)
                    let r12r     = Reaction.map id Expression.simplify Expression.simplify r
                    let reaction = Reaction.to_string Species.to_string vPrinter ePrinter r
                    "\"" + reaction + "\" not found in " + x + "'s set of reactions" ) 
      c1.name
      c2.name
    
let single_reversibles_equal veq opts = Lib.is_permutation (reversible_reaction_equal veq opts)

//-------------
// Compare events
(*
let target_equal opts t1 t2 =
  match t1, t2 with
  | Event.Parameter (p1, v1), Event.Parameter (p2, v2) -> p1 = p2 && value_equal v1 v2
  | Event.Species p1, Event.Species p2 -> pops_equal opts p1 p2
  | Event.OutputPoint, Event.OutputPoint -> true
  | _ -> false

let event_equal opts (ev1: Dsd.event) (ev2: Dsd.event) =
     (value_equal ev1.time ev2.time)
  && (target_equal opts ev1.target ev2.target)

let events_equal opts evs1 evs2 =
  Lib.is_permutation (event_equal opts) evs1 evs2
*)

//-------------
// Compare terms
(*
let terms_equal (t1: Dsd.term) (t2: Dsd.term) =
  let m1 = (Term.get_metadata t1)
  let m2 = Term.get_metadata t2
  if m1 = m2 then
       (pops_equal m1.options (Term.get_populations t1) (Term.get_populations t2))
    && (reversibles_equal value_equal m1.options (Term.get_reversibles t1) (Term.get_reversibles t2))
    && (events_equal m1.options (Term.get_events t1) (Term.get_events t2))
  else false
*)

let checkSettingsEqual (c1: Crn) (c2: Crn) =
  // check structural equality for all settings except plots
  (* CS: replacing plots with the empty list and then checking for structural equality somehow doesn't work (not sure why).
         For example, writing:
           let c11 = {c1 with settings = {c1.settings with simulation = Simulation_settings.swapPlots c1.settings.simulation []}}
           let c21 = {c2 with settings = {c2.settings with simulation = Simulation_settings.swapPlots c2.settings.simulation []}}
         and then trying:
           c11 = c21 
         returns false, but checking each single field separately -as done below- and then taking their union returns true *) 
  let s1 = c1.settings
  let s2 = c2.settings
  let sameData          = s1.data                 = s2.data
  let sameDeterministic = s1.deterministic        = s2.deterministic
  let sameInference     = s1.inference            = s2.inference
  let sameParameters    = s1.parameters           = s2.parameters
  let sameFinal         = s1.simulation.final     = s2.simulation.final 
  let sameInitial       = s1.simulation.initial   = s2.simulation.initial
  let sameKinetics      = s1.simulation.kinetics  = s2.simulation.kinetics
  let sameMulticore     = s1.simulation.multicore = s2.simulation.multicore
  let samePoints        = s1.simulation.points    = s2.simulation.points
  let samePrune         = s1.simulation.prune     = s2.simulation.prune
  let sameTimes         = s1.simulation.times     = s2.simulation.times
  let sameSimulator     = s1.simulator            = s2.simulator
  let sameSpatial       = s1.spatial              = s2.spatial
  let sameStochastic    = s1.stochastic           = s2.stochastic
  let sameSweep         = s1.sweeps               = s2.sweeps
  let sameUnits         = s1.units                = s2.units

  check ( sameData 
          && sameDeterministic
          && sameInference
          && sameParameters
          && sameFinal
          && sameInitial
          && sameKinetics
          && sameMulticore
          && samePoints
          && samePrune
          && sameTimes
          && sameSimulator
          && sameSpatial
          && sameStochastic
          && sameSweep
          && sameUnits )
        (fun _ -> "check_crn_equal: Settings differs")

  let plots1 = List.map Expression.simplify c1.settings.simulation.plots
  let plots2 = List.map Expression.simplify c2.settings.simulation.plots
  check_permutation 
    value_eq
    plots1
    plots2 
    (fun _ _ _ -> "plot mismatch")
    "dsd" 
    "spec"
  

let check_crn_equal_relaxed (c1: Crn) (c2: Crn) =
  // check (c1.settings = c2.settings) "check_crn_equal: Settings differs"
  let stripNewlines (s:string) = s.Replace("\r\n", "").Replace("\n","").Replace("\t","")
  let renamer (s:Species) = Species.create (stripNewlines s.name)
  let c1', c2' = c1.map renamer, c2.map renamer
  checkSettingsEqual c1' c2'
  check_pops_equal_relaxed c1' c2'
  check_reversibles_equal  c1' c2'
  // check (events_equal m1.options (Term.get_events t1) (Term.get_events t2)) "check_terms_equal: Events differ"

(*
let term_to_string t = Term.to_string Species.display t
*)

//-------------
let get_populations (crn:Crn) = 
  // copied from crn_
  let e : Environment.t = crn.settings.parameters |> Parameters.to_env 
  let pops,_ = Initial<Species, float>.to_initialpops_events e crn.settings.simulation.initial crn.initials
  pops

// Compiling and parsing
let parse_species opts s =
  let term = Dsd.compile s
  let pop,_ =
    Initial<Species, float>.to_initialpops_events  
        Microsoft.Research.CRNEngine.Environment.empty
        term.settings.simulation.initial
        term.initials
  pop.get_calculus_species_list 



//-------------
// Interrogate terms and ui results (internal DSL for testing, for example use see GrandTest.fs)
type rev_reac_stats =
  { total : int
  ; reversibles : int }
type term_interrogation =
  | Num_species of int
  | Num_reactions of int
  | Num_rev_reactions of rev_reac_stats
  | Equals_term of Dsd.term

(*
let eval_term_interrogation t = function
  | Num_species n -> t |> Term.get_populations |> Populations.get_count |> should be (equal n)
  | Num_reactions n -> t |> Term.get_reactions |> List.length |> should be (equal n)
  | Num_rev_reactions rrs ->
    let rs = Term.get_reversibles t
    rs |> List.length |> should be (equal rrs.total)
    rs |> List.filter (fun (_,o) -> Option.isSome o) |> List.length |> should be (equal rrs.reversibles)
  | Equals_term et ->
    debugm "Comparing terms:"; debug (term_to_string t); debugm "and"; debug (term_to_string et);
    terms_equal t et |> should be (equal true)*)

type interrogation_action =
  | Do_expand
  | Do_simulate
  | Do_infer
  | Do_BME_compile

type phase_interrogations =
  { phase_actions    : interrogation_action list
  ; before_expansion : term_interrogation list
  ; after_expansion  : term_interrogation list }

type example_test =
  | Forall_examples of phase_interrogations
  | For_these_examples of (string * phase_interrogations) list

let empty_pi =
  { phase_actions    = []
  ; before_expansion = []
  ; after_expansion  = [] }

type test_state =
  { current_example : string ref
  ; stored_time     : System.DateTime ref
  ; current_program : string ref
  ; current_pi      : phase_interrogations ref }

let empty_test_state () =
  { current_example = ref ""
  ; stored_time     = ref System.DateTime.Now
  ; current_program = ref ""
  ; current_pi      = ref empty_pi }

let store_now state =
  state.stored_time := System.DateTime.Now

  (*
let dsd_actions state =
  let on_promotion () = debug "Promotion successful"
  
  let on_dsd_program = 
    { BMEUI.updated = (fun s -> state.current_program := s)
    ; BMEUI.outdated = (fun () -> ()) }

  let on_dsd_compilation = 
    { BMEUI.updated =
      (fun (s: Ui.dsd_compilation_result) ->
         debug ("Interrogating " + !state.current_example + " before expansion")
         List.iter (eval_term_interrogation s.graphics_term) (!state.current_pi).before_expansion
                  
         if List.exists ((=) Do_expand) (!state.current_pi).phase_actions then
           debug ("Expanding " + !state.current_example)
           ignore(s.expander.run_expansion (ref false))
    )
    ; BMEUI.outdated = (fun () -> ()) }

  let listeners = 
    { Ui.on_dsd_program = [on_dsd_program]
    ; Ui.on_dsd_compilation = [on_dsd_compilation]
    ; Ui.on_domains_export = [] }

  let parse_error (s, p) =
    let pos_string =
      match p with
      | None -> ""
      | Some (b: Text.Lexing.Position, e: Text.Lexing.Position) ->
          " at line " + string b.Line +
          " column " + string b.Column +
          " - line " + string e.Line +
          " column " + string e.Column + " "
    failwith ("Parse error for example " + !state.current_example
             + pos_string + ": " + s)

  let dsd_parse_error (s, p) = parse_error ("DSD: " + s, p)
  let bme_parse_error (s, p) = parse_error ("BME: " + s, p)
  
  let parser s = s |> String.trim ['\"'] |> parse_species opts |> List.head
  
  let opts = Options.default_options

  let species_count = ref 0
  let species_hash = Hashtable.empty ()
  let find_species_id name =
    match Hashtable.tryFind species_hash name with
    | Some i -> i
    | None ->
        let i = !species_count
        species_count := i+1
        Hashtable.add species_hash name i
        i
  let finder s = "species_" + string (find_species_id s) |> Species.UNKNOWN
  let parser s =
    try
      s |> trim ['\"'] |> parse_species opts |> List.head
    with _ -> finder s
  let namer s = "\"" + Species.display s + "\""

  let bme_listeners = { BMEUI.listeners = BMEUI.no_listeners () }
  let dsd_actions = Ui.create_dsd_actions on_promotion listeners dsd_parse_error parser namer bme_listeners bme_parse_error

  let bme_expanded_hash = Hashtable.empty ()
  let bme_expand_guard example =
    match Hashtable.tryFind bme_expanded_hash example with
    | None ->
      ( Hashtable.add bme_expanded_hash example ();
        true )
    | Some () -> false
  let on_term = 
    { BMEUI.updated =
      (fun t ->
        if bme_expand_guard !state.current_example then (* skip the first promotion of the unexpanded term *)
          debug ("Interrogating " + !state.current_example + " after expansion")
          List.iter (eval_term_interrogation t) (!state.current_pi).after_expansion
      )
    ; BMEUI.outdated = (fun () -> ()) }

  bme_listeners.listeners.on_compilation <- [on_term]

  let bme_compiled_hash = Hashtable.empty ()
  let bme_compile_guard example =
    match Hashtable.tryFind bme_compiled_hash example with
    | None ->
      ( Hashtable.add bme_compiled_hash example ();
        true )
    | Some () -> false
  let on_program =
    { BMEUI.updated =
      (fun s ->
         if List.exists ((=) Do_BME_compile) (!state.current_pi).phase_actions
            && bme_compile_guard !state.current_example
         then
           debug ("BME Compiling " + !state.current_example)

           dsd_actions.bme_actions.actions.promote_program s
      )
    ; BMEUI.outdated = (fun () -> ()) }

  bme_listeners.listeners.on_program <- [on_program]

  let ui_callback =
    { BMEUI.sim_output = (fun sweep_key time z e o -> ())
    ; BMEUI.species_output = (fun sweep_key time z e ss -> ())
    ; BMEUI.cancel_flag = ref false }

  let bme_simulated_hash = Hashtable.empty ()
  let bme_simulate_guard example =
    match Hashtable.tryFind bme_simulated_hash example with
    | None ->
      ( Hashtable.add bme_simulated_hash example ();
        true )
    | Some () -> false
  let on_simulation =
    { BMEUI.updated =
      (fun s ->
         if List.exists ((=) Do_simulate) (!state.current_pi).phase_actions
            && bme_simulate_guard !state.current_example
         then
           debug ("Simulating " + !state.current_example)
           let (thunk: 'species BMEUI.simulation_thunk) = s ui_callback
           ignore(thunk.run_simulator ())
      )
    ; BMEUI.outdated = (fun () -> ()) }

  bme_listeners.listeners.on_simulation <- [on_simulation]

  let on_inference =
    { BMEUI.updated =
      (fun (thunk: 'species BMEUI.inference_thunk) ->
         if List.exists ((=) Do_infer) (!state.current_pi).phase_actions then
           debug ("Running inference on " + !state.current_example)
           thunk.run_inference () get_file Filzbach.default_run
      )
    ; BMEUI.outdated = (fun () -> ()) }

  bme_listeners.listeners.on_inference <- [on_inference]

  dsd_actions

let run_example_test = function
  | Forall_examples pi ->
    let state = empty_test_state ()
    state.current_pi := pi
    let actions = dsd_actions state
    let test_example name text =
      let interval = System.DateTime.op_Subtraction (System.DateTime.Now, !state.stored_time)
      debug ("Time since last: " + (interval.TotalSeconds.ToString ()) + " seconds")
      state.current_example := name
      debug ("Processing example: " + name)
      store_now state
      actions.promote_dsd_program "" "" text
      if !state.current_program <> text then failwith ("Example " + name + " came back mangled")
    debug "Testing all examples"
    store_now state
    forall_examples test_example

  | For_these_examples epis ->
    let state = empty_test_state ()
    let actions = dsd_actions state
    let test_example (name, pi) =
      let interval = System.DateTime.op_Subtraction (System.DateTime.Now, !state.stored_time)
      debug ("Time since last: " + (interval.TotalSeconds.ToString ()) + " seconds")
      state.current_example := name
      debug ("Processing example: " + name)
      let text = programText name
      store_now state
      state.current_pi := pi
      actions.promote_dsd_program "" "" text
      if !state.current_program <> text then failwith ("Example " + name + " came back mangled")
    debug "Testing certain examples"
    store_now state
    List.iter test_example epis


let tryFind_pi test name =
  match List.tryFind (fun (n, _) -> n = name) test with
  | Some (_, pi) -> Some pi
  | None -> None
  *)