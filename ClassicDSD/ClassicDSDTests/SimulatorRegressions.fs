module Microsoft.Research.DNA.SimulatorRegressions

//open System.IO
//open FsUnit
//open Xunit

//open Microsoft.Research.CRNEngine

(**
  let debug s = //()
    System.Diagnostics.Debug.WriteLine s
    System.Console.WriteLine s
**)

/// Custom operator for combining paths (from: http://www.fssnip.net/1g/title/Working-with-paths)
//let (+/) path1 path2 = Path.Combine(path1, path2)
//
//let TEST_FOLDER = "testData"
//
//let CLASSIC_DSD         = "classicDsd"
//let CRN_DIR             = "crns"
//let VISUAL_DSD          = "visualDsd"
//let CRN_COMPARISON_DIR  = "crnComparison"
//let SIMULATION_DIR      = "simulations"
//let OSLO_DIR            = "oslo"
//let DETERMINISTIC_SIM   = "deterministic"
//let CME                 = "CME"
//let ALL_SIMULATIONS     = "long"
//let QUICK_SIMULATIONS   = "quick"
//
//
//let parseLegacyCrn crnText = 
//    let pname = Parser.(|>>) 
//                  (Parser.(.>>) Parser.name Parser.spaces) 
//                  (Species.create)
//    Parser.from_string (LegacyParser.parse_legacy_SL pname) crnText
//
//
//(**************** Simulation results comparison ********************)
//let compare (expected : Result.t<float>) (actual : Result.t<float>) (eventTimes : float list) =
//  (***** filter out rows with identical time stamps (keep the last one) *******)
//  (* CS: when a CRN events is simulated, the simulators may print multiple datapoints at the same time stamp.
//         For example, simulating "init X 1 @ 10" produce two datapoints at time 10 with Oslo, 
//         one with concentration 0 and one with concentration 1 (in this order).
//         We only keep the last datapoint when timestamps are duplicated. 
//         We also remove datapoints near events, when in the range of [evTime - 1e-7, evTime) 
//         (because Oslo currently produces datapoints at 1e-8 time units before an event, which causes
//         interpolation errors during testing).                                                                 *)
//  let evRanges = List.map (fun eTime -> (eTime - 1e-7, eTime)) eventTimes
//  let notInRange f (low, up) = f < low || up <= f
//
//  let formatTimestampsRev (rows : Row.t<float> list) = 
//    match rows with
//    | [] -> []
//    | r::rs -> let rec filterRowLoop (next : Row.t<float>) (rows : Row.t<float> list) acc =
//                match rows with 
//                | []    -> next::acc
//                | r::rs -> if next.time <> r.time && List.forall (notInRange r.time) evRanges
//                            then filterRowLoop r rs (next::acc) // order is reversed here
//                            else filterRowLoop next rs acc
//               in filterRowLoop r rs []
//  
//  let colName (c : Table.column<float>) = c.name
//  let mkTable (r:Result.t<float>) = r.table
//                                  |> Table.to_rows_reverse 
//                                  |> formatTimestampsRev
//                                  |> Table.from_rows (List.map colName r.table.columns)
//  (** CME hack: standard deviation columns are now called "StDev" in CRNEngine**)
//  let renameTableCols (f:string->string) (t:Table.t<float>):Table.t<float> =
//    let renameCol (c:Table.column<float>) = { c with name = f c.name }
//    { t with columns = List.map renameCol t.columns }
//  let cmeHack (colName:string) = if colName.EndsWith "Std" 
//                                  then colName.Replace("Std", "StDev") 
//                                  else colName
//  (** /CME hack**)
//  
//  let aTab = mkTable actual
//  let eTab = mkTable expected |> renameTableCols cmeHack
//
//  (***** data setup: interpolate eTab to match aTab's timestamps **************)
//  let aRows     = Table.to_rows aTab
//  let eTimes    = eTab.times        
//  let eColumns  = aTab.columns |> List.map (fun (c: Table.column<float>) -> c.name)
//  let iRowsRev  = Row.interpolate_reverse Row.interpolate_float aRows eTimes
//  let iTabDups  = Table.from_rows_reverse eColumns iRowsRev
//  let iResults  = Result.create Instance.empty iTabDups
//  let iTab      = mkTable iResults
//  
//  // for debugging purposes:
//  let printResult r = Table.to_string "\t" (fun x -> x.ToString()) r
//  let eTabString    = printResult expected.table
//  let iTabString    = printResult iTab
//  let aTabString    = printResult actual.table
//
//  (***** cell test setup: check that two cells match up to relative error *****)
//  // find the the actual data's absolute scale
//  let findMax (ns:float list)= 
//    match ns with 
//    | []    -> failwith "Max: unexpected empty list\n"
//    | initMax :: rest -> List.fold (fun max x -> if x > max then x else max) initMax rest
//  let scale = aTab.columns 
//              |> List.map (fun (c: Table.column<float>) -> c.values |> findMax) 
//              |> findMax
//  
//  // relative error function, w.r.t the absolute scale
//  let margin = 0.01
//  let fullScaleError = if scale <> 0.0
//                        then fun actual expected -> abs ((expected - actual) / scale)// * (expected / scale)
//                        else fun actual expected -> abs (expected - actual)
//  let isError actual expected =
//    let relErr = fullScaleError actual expected 
//    relErr > margin
//
//  // match up values up to the relative error
//  let errorMsg columnName rowIndex actual expected = 
//    let i   = rowIndex.ToString()
//    let a   = actual.ToString()
//    let e   = expected.ToString()
//    let s   = scale.ToString()
//    let err = (fullScaleError actual expected).ToString() 
//    let mar = margin.ToString()
//    "Simulation error detected in column \"" + columnName + "\" at row " + i + ":\n"  
//    + "Interpolated data:  " + a + "\n"
//    + "Expected data:      " + e + "\n"
//    + "Scale:              " + s + "\n"
//    + "Full Scale Error:   " + err + " > " + mar + "\n"
//    + "Expected simulation:\n"      + eTabString + "\n"
//    + "Interpolated simulation:\n"  + iTabString + "\n"
//    + "Actual simulation:\n"        + aTabString + "\n"
//  
//  (***** test loop setup: run isError for each cell in iTab and eTab **************)
//  let compareRow colName testFailure (idx, (iVal, eVal)) = 
//    match testFailure with
//    | None -> if isError iVal eVal
//                then Some (errorMsg colName idx iVal eVal)
//                else None
//    | _    -> testFailure
//
//  let findColumn (name:string) (tab: Table.t<float>) = List.tryFind (fun (c:Table.column<float>) -> c.name = name) tab.columns
//
//  let testICol (iCol : Table.column<float>) =
//    match findColumn iCol.name eTab with
//    | None      ->  Some ("Column \"" + iCol.name + "\" not found")
//    | Some eCol ->  List.zip iCol.values eCol.values  // pair rows up
//                    |> List.mapi (fun i x -> (i, x))  // add row index
//                    // compare rows; exit the test on the first occurrence of failure
//                    |> List.fold (compareRow iCol.name) None 
//      
//  let testLoop errorMsg (c : Table.column<float>) =
//    match errorMsg with 
//    | None -> testICol c
//    | _    -> errorMsg
//  
//  (***** check that columns length and names match, then test the data ****)
//  if aTab.columns.Length <> eTab.columns.Length
//    then Some "Column lengths differ.\n"
//    else 
//      (***** check that column names match up to reordering ****)
//      let getColumnName ( c : Table.column<float> ) = c.name
//      let aColNames = (List.map getColumnName aTab.columns |> Set.ofList)
//      let eColNames = (List.map getColumnName eTab.columns |> Set.ofList)
//      if eColNames <> aColNames
//        then Some "Column names differ.\n"
//        (***** check that simulation data sets match ****)
//        else List.fold testLoop None iTab.columns
//  
//let run_single expected actualCrn =
//    //debug ("Running simulator regression test: " + test_name)
//    let actuals  = Crn.simulate actualCrn
//    let actual   = if actuals.Length <> 1
//                    then failwith "Simulation testing for sweeps is not supported yet."
//                    else actuals.Head
//    let env        = Parameter.to_env actualCrn.settings.parameters
//    let eventTimes = actualCrn.initials
//                     |> List.choose (fun (i:Crn.initial) -> match Expression.eval (Environment.find env) i.time with
//                                                            | 0.0 -> None
//                                                            | i   -> Some i)
//
//    // hack to circumvent the fact that BME parses 1 + 2 + 3
//    // as ((1+2)+3) whereas CRNEngine parses it as (1+(2+3))
//    let stripParenthesesFromHeaders (actual:Result.t<float>) = 
//      { actual with table = 
//                      {actual.table with columns = List.map (fun (c:Table.column<float>) -> {c with name = String.filter (fun (ch : char) -> not (List.contains ch ['('; ')'])) c.name}) actual.table.columns }}
//    let actual'   = stripParenthesesFromHeaders actual
//    let expected' = stripParenthesesFromHeaders expected
//
//    compare expected' actual' eventTimes
//    
//let run testName (sim, crn : Crn.t) simKind isStiff : string option =
//  let simData = Parser.from_string Result.tsv_parser sim
//  let simCrn  = {crn with settings = { crn.settings with simulator     = simKind
//                                                         deterministic = {crn.settings.deterministic with stiff = isStiff;
//                                                                                                          reltolerance = 1e-5}}}
//  match run_single simData simCrn with
//  | None       -> None
//  | Some error ->
//            let stiff = if isStiff then " stiff" else ""
//            let kind  = Crn_settings.simulator_to_string simKind
//            let errorMsg = (sprintf "Test \"%s\" - %s:\n%s" testName (kind + stiff) error)
//            Some errorMsg
//            
//let trimEmpty = String.filter (fun c -> List.exists ((=) c) [' ';'\r';'\n';'\t'])
//
//let testSingleCrn testsDir errorMsgs crnFile : string list= 
//  let simDirName = Path.GetFileNameWithoutExtension crnFile + "_simulation"
//  let simDir     = testsDir +/ simDirName
//  if not (Directory.Exists simDir)
//    then  
//      // debug (sprintf "Skipping test \"%s\", no \"_simulation\" data folder found." crnFile)
//      errorMsgs
//    else  
//      let crn      = parseLegacyCrn (File.ReadAllText crnFile)
//      let simFiles = Directory.EnumerateFiles (simDir, "*.tsv") |> Seq.toList
//      let runSimulationTest simErrors simFile = 
//        let testName = Path.GetFileNameWithoutExtension crnFile
//        let test     = (File.ReadAllText simFile, crn)
//        let stiff    = true
//        let oslo     = Crn_settings.simulator.Oslo
//        let sundials = Crn_settings.simulator.Sundials
//        let lna      = Crn_settings.simulator.LNA
//          
//        let simKind  = Path.GetFileNameWithoutExtension simFile
//        // debug (sprintf "Testing %s on %s" testName simKind)
//        let maybeError = 
//          match simKind with
//          | "deterministic"       -> run testName test oslo      (not stiff)
//          | "sundials"            -> run testName test sundials  (not stiff)
//          | "lna"                 -> run testName test lna       (not stiff)
//          | "deterministicstiff"  -> run testName test oslo      stiff      
//          | "sundialsstiff"       -> run testName test sundials  stiff      
//          | "lnastiff"            -> run testName test lna       stiff      
//          | _ -> Some (sprintf "Skipping unrecognized simulation data \"%s\" in \"%s\"" simKind simFile)
//        match maybeError with
//        | Some newError -> simErrors @ [newError]
//        | None          -> simErrors
//      let simErrors = List.fold runSimulationTest [] simFiles
//      errorMsgs @ simErrors
//
//[<Fact>]
//(** run Oslo, Sundials and LNA (stiff and non-stiff) on CRNs generated from Visual DSD programs.
//    Simulation data is compared to data pre-computed by CliBME and stored in the ALL_SIMULATIONS folder.
//    The total running time is 98 minutes (CS: on my machine, as of 10/03/2017). **)
//[<Trait("Category", "Slow")>]
//let ``Oslo, Sundials and LNA regression tests - all`` () =
//  let projectDir = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
//  let testsDir   = projectDir +/ TEST_FOLDER 
//                              +/ SIMULATION_DIR 
//                              +/ DETERMINISTIC_SIM 
//                              +/ ALL_SIMULATIONS
//  let crns         = Directory.EnumerateFiles (testsDir, "*.crn") |> Seq.toList
//        
//  match List.fold (testSingleCrn testsDir) [] crns with
//  | []    -> ()
//  | errs  -> failwith (String.concat "\n" errs)
//
//
//
//[<Fact>]
//(** run the regression tests on a restricted subset of CRNs, so that it can be run within one minute **)
//let ``Oslo, Sundials and LNA regression tests - subset`` () =
//  let projectDir   = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
//  let testsPath    = projectDir +/ TEST_FOLDER 
//                                +/ SIMULATION_DIR 
//                                +/ DETERMINISTIC_SIM 
//                                +/ QUICK_SIMULATIONS
//  let crns         = Directory.EnumerateFiles (testsPath, "*.crn") |> Seq.toList
//        
//  match List.fold (testSingleCrn testsPath) [] crns with
//  | []    -> ()
//  | errs  -> failwith (String.concat "\n" errs)
//
//(** copied and pasted from Expression.fs, because CliBME prints floats with a trailing .0 whereas CRNEngine does not **)
//let legacyPrint f = let s = f.ToString()
//                    if (s.Contains ".") || (s.Contains "e") || (s.Contains "E") then s else s + ".0"
//
//let rec ep kp e : string = // expression printer
//  match e with
//  | Expression.t.Key x                         -> kp x
//  | Expression.t.Float f                       -> legacyPrint f
//  | Expression.t.Divide {div1=x; div2=y}       -> ep kp x + " / " + ep kp y
//  | Expression.t.Power {base_=x;exponent=n}    -> ep kp x + " ^ " + ep kp n
//  | Expression.t.Times l                       -> if l.Length  = 1
//                                                    then ep kp l.Head
//                                                    else l |> List.map (ep kp) 
//                                                           |> List.reduce (fun x y -> x + " * " + y)
//  | Expression.t.Plus l                        -> if l.Length = 1 
//                                                    then ep kp l.Head
//                                                    else l |> List.map (ep kp) 
//                                                           |> List.reduce (fun x y -> x + " + " + y)
//  | Expression.t.Minus {sub1=x;sub2=y}         -> ep kp x + " - " + ep kp y
//  | Expression.t.Absolute x                    -> "|" + ep kp x + "|"
//  | Expression.t.Log x                         -> "log(" + ep kp x + ")"
//  | Expression.t.Modulo {div = x; modulo = y}  -> ep kp x + " % " + ep kp y
//(** /copied and pasted **)
//
//(** copied and pasted from assignment.fs, because CliBME prints floats with a trailing .0 whereas CRNEngine does not **)
//let tuple_to_string (l:string list) = 
//  match l with
//  | [] -> ""
//  | [single] -> single
//  | tuple -> "(" + (String.concat "," tuple) + ")"
//
//let to_string (a:Assignment.t) =
//  let varsTuple    = tuple_to_string a.variables
//  let valuesTuples = a.values 
//                     |> List.map (List.map (ep id) >> tuple_to_string) 
//                     |> String.concat "; "
//  sprintf "%s = [%s]" varsTuple valuesTuples
//
//let to_string_bindings (a:Assignment.t) = 
//  let varsTuple = tuple_to_string a.variables
//  let valTuples = a.values 
//                |> List.map (List.map (ep id) >> tuple_to_string) 
//  List.map (fun tup -> varsTuple + " = " + tup) valTuples
//(** /copied and pasted**)
//
//
//
//[<Fact>]
//let ``CME regression tests`` () =
//  // get test file names
//  let projectDir   = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
//  let testsPath    = projectDir +/ TEST_FOLDER 
//                                +/ SIMULATION_DIR 
//                                +/ CME
//  let crns         = Directory.EnumerateFiles (testsPath, "*.crn") |> Seq.toList
//  
//  // CME specific constants
//  let CME_PREFIX              = "cme_"
//  let STIFF_PREFIX            = "cmestiff_"
//  let MEAN_STDEV_TABLE_PREFIX = "meanstd_" 
//  
//  // test a cnrFile
//  let testSingleCrn crnFile : string list option = 
//    let simDirName = Path.GetFileNameWithoutExtension crnFile + "_simulation"
//    let simDir     = testsPath +/ simDirName
//    if not (Directory.Exists simDir)
//      then  
//        // debug (sprintf "Skipping test \"%s\", no \"_simulation\" data folder found." crnFile)
//        None
//      else  
//        let crn         = parseLegacyCrn (File.ReadAllText crnFile)
//        let stiffCrn    = { crn with settings = 
//                                     {crn.settings with deterministic = 
//                                                        {crn.settings.deterministic with stiff = true}}}
//        let sweeps      = crn.settings.sweeps
//        if sweeps.Length > 1 
//          then // debug ("Skipping test \"" + crnFile + "\", multiple sweeps not supported"); 
//            None
//          else 
//        let sweep       = sweeps.Head // assum: only one sweep is run
//        let sweepName   = sweep.name
//        
//        // plot printer 
//        let pp          = ep Species.to_string 
//
//        
//        // constructs the test file name from the arguments (instance contains the assignments that have been used to run the simulation)
//        let mkTestFileName stiffPrefix kind (actualInstance:Instance.t) = 
//          let assignments       = Map.toList actualInstance.environment
//          let printAsg (x, y)   = x + " = " + legacyPrint y
//          let asgs              = String.concat ", " (List.map printAsg assignments)
//          simDir +/ sweepName +/ stiffPrefix + kind + asgs + ".tsv"
//
//        // loads the expected data
//        let loadCmeTable testFile (actualInstance:Instance.t) = 
//          let contents  = File.ReadAllText(testFile)
//          let table     = Parser.from_string Result.tsv_parser contents
//          { table with instance = {table.instance with environment = actualInstance.environment
//                                                       sweep       = sweep.name }}
//
//        let plotNames   = List.map pp crn.settings.simulation.plots
//        
//        // run a simulation sweep, and load its corresponding 
//        let simulateSweep isStiff (i:Instance.t) =
//          // run CME on a sweep
//          let plot = plotNames.Head
//          let testCrn = if isStiff
//                          then stiffCrn
//                          else crn
//          let aMarginalsRaw, aData = testCrn  |> Crn.substitute i.environment
//                                              |> Crn.simulate_cme_single 
//                                              |> fun (x,y) -> let p = Cme.probability_map x.probabilities plot
//                                                              (p, Result.create i (Table.point_to_float y))
//          // store the marginal distributions in a Result.t
//          let cmePrefix = if isStiff
//                            then STIFF_PREFIX
//                            else CME_PREFIX
//          let aColNames = if aMarginalsRaw.Length = 0
//                            then // error
//                              let msg       = "Simulator returned empty marginal distribution for \"" 
//                                            + plot + "\" in test \"" + crnFile + "\""
//                              failwith msg
//                            else
//                              List.map (fun n -> plot + " = " + n.ToString()) [0..(aMarginalsRaw.Head.Length-1)]
//          let aMarginalsRows  = List.map2 Row.from_array aData.table.times aMarginalsRaw
//          let aMarginalsTable = Table.from_rows aColNames aMarginalsRows
//          let aMarginals      = Result.create i aMarginalsTable
//          
//          // load expected data
//          // marginal probabilities
//          let eMarginalsFile  = mkTestFileName cmePrefix (plot + "_") i
//          let eMarginals      = loadCmeTable eMarginalsFile i        
//          
//          // mean and std. dev.
//          let eDataFile   = mkTestFileName cmePrefix MEAN_STDEV_TABLE_PREFIX i
//          let eData       = loadCmeTable eDataFile i   
//          
//          [(eDataFile, eData, aData); (eMarginalsFile, eMarginals, aMarginals)]
//        
//        // run CME and CME Stiff for all sweeps in the crn
//        let runSim isStiff = 
//          let testCrn   = if isStiff
//                            then stiffCrn
//                            else crn
//          testCrn |> Crn.get_instances 
//                  |> List.collect (simulateSweep isStiff)
//        
//        // compare expected and simulation results
//        let runTest isStiff = 
//          runSim isStiff
//          |> List.choose
//              (fun (testFile, expected, actual) ->  
//                let cmeHack (s:string) = s.Replace("(Std)", "(StDev)")
//                let hackedColNames = expected.table.columns 
//                                    |> List.map (fun c -> cmeHack c.name)
//                let lessThanFinal f = f <= crn.settings.simulation.final
//                let eTableInRange = expected.table |> Table.to_rows_reverse
//                                                |> List.filter (fun (r) -> lessThanFinal r.time  )
//                                                |> Table.from_rows_reverse hackedColNames
//                let eDataInRange = { expected with table = eTableInRange }
//                match compare eDataInRange actual [] with
//                | None -> None
//                | Some msg -> Some ("CME simulation test \""+ testFile + "\":\n" + msg) )
//        
//        // report error if any
//        let stiff = true
//        let cmeResults      = runTest (not stiff)
//        let cmeStiffResults = runTest stiff
//        let results         = cmeResults @ cmeStiffResults
//        if List.isEmpty results
//          then None
//          else Some results
//        
//  let testResults   = List.choose testSingleCrn crns
//  let errorMessages = String.concat "\n" (List.concat testResults)
//  Assert.True(testResults.IsEmpty, errorMessages)