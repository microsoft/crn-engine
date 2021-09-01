// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.CRNs.Analyser

open System.IO
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.CRNEngine

let And = Array.reduce (fun a b -> BAnd (a,b))
//let Or  = Array.reduce (fun a b -> BOr (a,b))

let EncodeNonlinCRN numSpecies numProducts = 
    let ode = 
        sprintf "super_S%d_P%d_ode.txt" numSpecies numProducts
        |> System.IO.File.ReadAllLines
        |> Array.map(fun l -> 
                let eq = l.Split('=')
                let v = eq.[0].Replace("d(","").Replace(")","").Trim()
                let e = eq.[1] |> ExpressionFunctions.Parse
                v,e)
    
    Dynamical.Create(ode, Map.ofArray [|"A",Float 1.0; "B", Key "DB"|])
    |> Dynamical.setAllLB 0.0 //all species and rates are non-negative
    |> Dynamical.setUB "DB" 1.0 
    |> Dynamical.setLB "DB" 0.0 
    
let ReadSubslist fname numSubsets index = 
    printfn "Reading list of reaction subsets"
    let sr = File.ReadLines fname
    let listSize = Seq.length sr
    let subsetSize = (float listSize) / (float numSubsets) |> System.Math.Ceiling |> int
    let startIndex = index*subsetSize
    let size = min (listSize-startIndex) subsetSize
    let sr' = Seq.skip startIndex sr
    Seq.take size sr' 
    |> Array.ofSeq
    |> Array.map (fun line -> line.Split(',') |> Array.map int)

let GenNonlinCRNiter printer settings encodeZ3 solver numSpecies numProducts (newsubslist:int[][]) = 
    let sys = EncodeNonlinCRN numSpecies numProducts
    let subMap, choices = 
        sys.GetVars
        |> Set.toArray
        |> Array.filter(fun v -> v.[0] = 'k')
        |> Array.map(fun v -> 
            let c = sprintf "c%s" v.[1..] |> Key
            (v, (Key v)*c), c)
        |> Array.unzip 

    let parameterised = subMap |> Map.ofSeq |> sys.SubstParam
    //let turingAnalysis s sys = try sys |> Solver.CheckTuring encodeZ3 solver s with | _ -> printf "[FAILED] "; sys, -1.0, true, ""
    let turingAnalysis s sys = sys |> Solver.CheckTuring encodeZ3 solver s
    let checkTuringNoReturn = turingAnalysis settings
    //let newnumsubs = newsubslist.Length|>float
    Array.mapi (fun i elem -> 
        let csubs = elem |> Array.map (fun i -> choices.[i])
        let onesubchoices, zerosubchoices = choices |> Array.partition (fun c -> Array.exists (fun x -> x=c) csubs)
        let zerochoiceValues = zerosubchoices |> Array.map(fun c -> BEq(c, Float 0.0)) |> And
        let onechoiceValues = onesubchoices |> Array.map(fun c -> BEq(c, Float 1.0)) |> And
        let output = parameterised |> Dynamical.addCst zerochoiceValues|> Dynamical.addCst onechoiceValues |> checkTuringNoReturn
        let status = 
            match output.solution with
            | SAT _        -> "SAT"
            | UNKNOWN _    -> "UNKNOWN"
            | UNSAT  _     -> "UNSAT"
            | FAILED (s,t) -> "FAILED"
            | _            -> "OTHER"            
        printer i elem status;
        status
    ) newsubslist

let Run solver settings numSpecies numProducts (subs:int[][]) = 
    let numsubs = subs.Length
    let fileContents = ref []
    let printer id (elem:int[]) status = 
        let result = elem |> Array.map string |> String.concat "," 
        printfn "ID %d of %d | Elements %s | %s" id numsubs result status
        fileContents := (result + "," + status)::!fileContents
    let status = GenNonlinCRNiter printer settings true solver numSpecies numProducts subs
    List.rev !fileContents, status

let RunFile solver settings numSpecies numProducts file =
    let subslist = ReadSubslist file 1 0
    let results, _ = Run solver settings numSpecies numProducts subslist
    let outputFile = sprintf "result_%s" file
    File.WriteAllLines (outputFile, results)

let DynamicalFromCrn dvary print code name =
    let directives = 
        match dvary with
        | Enumerator.A -> "directive parameters [D_A=1.0]\r\ndirective spatial {diffusibles = [A=D_A; B=1.0]}"
        | Enumerator.B -> "directive parameters [D_B=1.0]\r\ndirective spatial {diffusibles = [A=1.0; B=D_B]}"
    if print then printfn "-------\nCRN:\n%s" code;
    let system = 
        directives + code
        |> Microsoft.Research.CRNEngine.Crn.from_string
        |> Dynamical.fromCRN 
        |> Dynamical.setName name
        |> Dynamical.setAllLB 0.0 //all species and rates are non-negative
        |> fun s -> 
            match dvary with
            | Enumerator.A -> Dynamical.setLB "D_A" 1.0 s
            | Enumerator.B -> Dynamical.setUB "D_B" 1.0 s

    system//, directives, code

let DynamicalFromByteCodeCrn dvary print (crn : Microsoft.Research.CRNEngine.Crn) name = 
  let code = 
    crn.reactions 
    |> List.map (Reaction.to_string (Species.to_string) (Expression.to_string id) (Functional2.to_string)) 
    |> String.concat "|\r\n"
  DynamicalFromCrn dvary print code name

let CrnOfSubset dvary print rs elems =
    let code = elems |> Array.map (Array.get rs) |> String.concat " |\r\n"
    let name = elems |> Array.map string |> String.concat "," 
    DynamicalFromCrn dvary print code name

let RunCrn solver settings crn_settings elem = 
    let rs = Enumerator.reactions crn_settings
    let system = CrnOfSubset crn_settings.Dvary true rs elem
    let result = Solver.CheckTuring true solver settings system
    printfn "%s" (result.solution.ToString())
    // Print the results
    match result.solution with 
    | SAT (sol,t) -> sol |> Map.toList |> List.iter (fun (k,v) -> printfn "%s = %1.6g" k v)
    | _ -> ()
        
(* Approach 2: The following methods are for using the Solver.RunSet method *)
/// Run a set of dynamical systems with the Turing solver
let RunDynamicalSet solver settings verbose systems = 
    printfn "Evaluating %d systems" (Seq.length systems)
    let fileContents = System.Text.StringBuilder()
    let result_printer i system status time = 
        let str = status.ToString()
        if verbose 
        then printfn "| %s | %1.6g" str time 
        else 
            printf "."
            if (i+1) % 10 = 0 then printf "|"
        fileContents.AppendLine (sprintf "%s,%s,%1.6g" system str time) |> ignore
    
    let status = 
        systems
        |> Seq.mapi (fun i (S:Dynamical) ->
            if verbose then printf "%s " S.name
            let r = Solver.CheckTuring true solver settings S
            result_printer i r.name r.solution.Status r.solution.Time
            if (i+1) % 100 = 0 then printfn " %d" (i+1)
            r)        

    let counts = 
        status
        |> Seq.countBy (fun s -> s.solution.Status)        
        |> Map.ofSeq
    let sat = match Map.tryFind "SAT" counts with Some c -> c | None -> 0
    let unsat = match Map.tryFind "UNSAT" counts with Some c -> c | None -> 0
    let unknown = match Map.tryFind "UNKNOWN" counts with Some c -> c | None -> 0

    fileContents.ToString(), sat, unsat, unknown

let ElemsToDynamical crn_settings skip elems = 
    let rs = Enumerator.reactions crn_settings
    elems
    |> Array.indexed
    |> Array.filter (fun (i,_) -> not (List.contains i skip))
    |> Array.map (fun (_,sys) -> CrnOfSubset crn_settings.Dvary false rs sys)

let RunSubset solver settings crn_settings index skip =    
    let subset = 
        ReadSubslist ("subslist_" + Enumerator.filetag crn_settings + ".csv") crn_settings.subsets index 
        |> ElemsToDynamical crn_settings skip
    let result, sat, unsat, unknown = subset |> RunDynamicalSet solver settings crn_settings.verbose
    printfn "\n------------------\nSAT - %d\nUNSAT - %d\nUNKNOWN - %d\n------------------" sat unsat unknown
    let outputFile = sprintf "result_%s_%d.csv" (Enumerator.filetag crn_settings) index
    File.WriteAllText (outputFile, result)

let sampleWithoutReplacement (rnd:System.Random) count max = 
    rnd
    |> Seq.unfold (fun r -> Some(r.Next(max), r))
    |> Seq.distinct
    |> Seq.take count
    |> List.ofSeq

let Sample solver settings crn_settings filters n seed = 
    let rs = Enumerator.reactions crn_settings
    let max = Array.length rs - 1
    let rnd = System.Random seed
    let stoich  = Enumerator.Stoichiometry crn_settings.numSpecies crn_settings.numProducts
    let stoichR = Enumerator.StoichiometryReactant crn_settings.numSpecies crn_settings.numProducts
    printfn "- Generating %d random subsets over %d reactions" n max
    let filter = Enumerator.getStoichSubset stoich stoichR >> Enumerator.Filter filters false

    let rec generatePermissible count stable = 
        if count = 0 
        then stable
        else
            let cand = sampleWithoutReplacement rnd crn_settings.numReactions max |> List.sort
            if filter cand 
            then 
                if (count-1) % 100 = 0 then printfn "- %d to go" (count-1)
                generatePermissible (count-1) (cand::stable)
            else generatePermissible count stable

    let systems = generatePermissible n [] |> List.map Array.ofList |> Array.ofList |> Array.map (CrnOfSubset crn_settings.Dvary false rs)
    
    let result, sat, unsat, unknown = RunDynamicalSet solver settings crn_settings.verbose systems
    printfn "------------------\nSAT - %d\nUNSAT - %d\nUNKNOWN - %d\n------------------" sat unsat unknown
    let outputFile = sprintf "result_%s_sample%d.csv" (Enumerator.filetag crn_settings) seed
    File.WriteAllText (outputFile, result)

let LoadResult (settings:Enumerator.Settings) fname = 
    let data = File.ReadAllLines fname
    data
    |> Array.map (fun line ->
        let els = line.Split(',')
        let reaction_ids = els.[0..settings.numReactions-1] |> Array.map int
        let status = AnalysisResult.from_string els.[settings.numReactions]
        let time = float els.[settings.numReactions+1]
        reaction_ids, status, time
    )

let CombineResults settings dir =
    let tag = Enumerator.filetag settings
    let fS = File.CreateText (sprintf "allSAT_%s.csv" tag)
    let fU = File.CreateText (sprintf "allUNKNOWN_%s.csv" tag)
    let filesToRead = System.IO.Directory.EnumerateFiles(dir, sprintf "result_%s_*.csv" tag)
    let times, _ = 
        filesToRead
        |> Seq.fold (fun (times, (sat,unknown,unsat)) fname ->
            let data = LoadResult settings fname
            if Array.isEmpty data
            then times, (sat,unknown,unsat)
            else
                let status_times = 
                    data
                    |> Array.map (fun (reaction_ids, status, time) ->
                        match status with
                        | SAT _     -> fS.WriteLine(reaction_ids |> Array.map string |> String.concat ",")
                        | UNKNOWN _ -> fU.WriteLine(reaction_ids |> Array.map string |> String.concat ",")
                        | _         -> ()
                        status, time
                    )
                    |> List.ofArray
                let counts = status_times |> List.map fst |> List.countBy (fun s -> s.Status) |> Map.ofList

                let sat' = match Map.tryFind "SAT" counts with Some v -> v | None -> 0
                let unknown' = match Map.tryFind "UNKNOWN" counts with Some v -> v | None -> 0
                let unsat' = match Map.tryFind "UNSAT" counts with Some v -> v | None -> 0
                let resolved = status_times |> List.filter (fun (st,t) -> st.Status = "SAT" || st.Status = "UNSAT") |> List.map snd
                printfn "%s: %d SAT (%d total) %d UNKNOWN (%d total)" fname sat' (sat+sat') unknown' (unknown+unknown')
                resolved :: times, (sat+sat', unknown+unknown', unsat+unsat')
         ) ([], (0,0,0))
    
    fS.Close()
    fU.Close()
    
    let filtered = times |> List.concat |> List.filter (fun v -> not (System.Double.IsInfinity v || System.Double.IsNaN v))
    let fT = File.Create (sprintf "times_%s.dat" tag)
    use br = new BinaryWriter(fT)
    filtered |> List.iter (fun t -> br.Write(t))
    br.Close()
    fT.Close()
    filtered

let HistogramLogTimes (settings:Enumerator.Settings) times = 
    let tag = sprintf "S%d_P%d_R%d" settings.numSpecies settings.numProducts settings.numReactions
    let logtimes = times |> List.map log10         
    
    // Print the table of times
    if not (List.isEmpty logtimes)
    then 
        let buckets = 60
        let hist = MathNet.Numerics.Statistics.Histogram(logtimes, buckets)
        let lines = 
            [0..buckets-1] 
            |> List.map (fun i -> let h = hist.Item(i) in sprintf "%1.6g,%d" ((h.UpperBound + h.LowerBound)/2.0) (int h.Count))
            |> List.append ["log10(Time),Frequency"]
        File.WriteAllLines(sprintf "histogramLogTimes_%s.csv" tag, lines)
    
