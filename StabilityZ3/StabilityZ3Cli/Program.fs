// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.GRNs.GRN
open System.IO
open Argu

type Model = CRN | GRN | LinearCRN
type Solver = SMT | NLsat | Portfolio | Custom
//type CrnMode = Generate | Enumerate | IDs | Sequence | Run | CombineResults | Sample
type Mode = Generate | Individual | Batch | Sample

type CrnArguments = 
  | Reactions of int
  | Products of int
  | Dvary of CRNs.Enumerator.Dvary
  | Index of int 
  | Skip of int list
  | Ids of int list
  | Conservation
  | Nontrivial
  | Samples of int
  | Seed of int
  | Verbose
  | ByteEncoded
  interface IArgParserTemplate with 
    member s.Usage =
      match s with
      //| Encoding _      -> "Specify whether CRNs are stored using a bytes-encoding or reaction lookup table"
      | Reactions _     -> "Specify number of reactions"
      | Products _      -> "Maximum number of products in each reaction"
      | Dvary _         -> "Specify which species has a variable diffusion rate in analysis (N.B. diffusion of A always greater than diffusion of B)"
      | Index _         -> "Specify which subset to analyse"
      | Skip _          -> "Skip analysis of specific IDs (return UNKNOWN)"
      | Ids _           -> "Specify which set of reaction IDs to analyse"
      | Conservation    -> "Filter out conservation laws when enumerating CRNs"
      | Nontrivial _    -> "Filter out networks with trivial dynamics"
      | Samples _       -> "Specify number of samples in the sample mode"
      | Seed _          -> "Specify a seed for the random number generator in the sample mode"
      | Verbose         -> "Write out more information to console"
      | ByteEncoded     -> "Specify whether CRNs are stored using a byte-encoding"

and GrnArguments = 
  | Hypothesis of GRNs.GRN.Regulation * float
  | Interactions of int list
  | Diffusibles of int
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Hypothesis _   -> "Specify the regulation function and cooperativity"
      | Diffusibles _  -> "Number of diffusible species"
      | Interactions _ -> "Specify interaction graph as a flattened list of integers. e.g. '-1 1 1 -1' specifies a positive feedback loop over 2 species."
      

and CliArguments = 
  | [<Inherit>] Mode of Mode
  | [<Inherit>] Solver of Solver
  | [<Inherit>] TimeOut of int
  | [<Inherit>] Species of int
  | [<Inherit>] Noise of TuringSymbolic.NoiseAmplificationFilter
  | [<Inherit>] ZeroCross of bool
  | [<Inherit>] Oscillations of bool
  | [<Inherit>] ChunkSize of int
  | [<Inherit>] Directory of string
  | [<Inherit>] File of string  
  | [<CliPrefix(CliPrefix.None)>] CRN of ParseResults<CrnArguments>
  | [<CliPrefix(CliPrefix.None)>] GRN of ParseResults<GrnArguments>
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | CRN _          -> "Run CRN analysis"
      | GRN _          -> "Run GRN analysis"
      | Mode _         -> "Specify which function you wish to execute"
      | Species _      -> "Specify number of species"
      | Noise _        -> "Filter out noise-amplifying networks"
      | ZeroCross _    -> "Include [there exists w s.t. the characteristic polynomial constant term is zero] as an additional constraint"
      | Oscillations _ -> "Filter out oscillatory networks"
      | ChunkSize _    -> "Number of rows per file in generator"
      | Directory _    -> "Specify where to read or write results"
      | File _         -> "Specify the file to load and analyse"
      | Solver _       -> "Specify solver"
      | TimeOut _      -> "Specify (seconds) solver timeout"

let speciesNames = [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]

let stringReg i = match i with 1 -> "p" | 0 -> "o" | -1 -> "m" | _ -> failwithf "Unknown interaction %d" i
let stringOfInteractions is = is |> List.map stringReg |> String.concat ""

let printSettings solver (settings:TuringSymbolic.TuringAnalysisSettings) = 
    printfn "- Solver: %s,%s" (Solver.solverType.to_string solver) Microsoft.Z3.Version.FullVersion
    if not settings.prevent_oscillations then printfn "- Enabling oscillatory patterns"
    printfn "- Noise-amplification filter: %s" (settings.prevent_noise_amp.ToString())
    if settings.zero_cross_cst then printfn "- Enforcing zero-crossing constraint"


[<EntryPoint>]
let main argv = 

    let parser = ArgumentParser.Create<CliArguments>()
    let args = parser.Parse(inputs = argv, raiseOnUsage = true, ignoreUnrecognized = true)
    
    printfn "Initialising..."
    
    // Define the solver
    let tout = args.GetResult(TimeOut, defaultValue=10)
    let timeout = uint32 (tout * 1000)
    let solver =
      match args.GetResult(Solver, defaultValue=Portfolio) with 
      | SMT -> Solver.solverType.SmtTO timeout
      | NLsat -> Solver.solverType.NlsatTO timeout
      | Portfolio -> Solver.solverType.PortfolioNoCustomTO timeout
      | Custom -> Solver.solverType.PortfolioTO timeout
    
    // Analysis settings
    let prevent_oscillations = args.GetResult(Oscillations, defaultValue = true)
    let noise_filter = args.GetResult(Noise, defaultValue = TuringSymbolic.Neutral)
    let zero_cross = args.GetResult(ZeroCross, defaultValue = false)
    let settings : TuringSymbolic.TuringAnalysisSettings = 
        { prevent_oscillations = prevent_oscillations
        ; prevent_noise_amp    = noise_filter
        ; use_Lienarad_Chipart = false
        ; group_terms          = None
        ; model_reduction      = TuringSymbolic.Off
        ; timeout              = Some tout
        ; seed                 = None
        ; print_status         = false
        ; zero_cross_cst       = zero_cross
        }
    
    // I/O
    let dir = args.GetResult(Directory, defaultValue=".")
    System.IO.Directory.CreateDirectory dir |> ignore
    let chunkSize = args.TryGetResult ChunkSize
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()

    // Process the sub-command structure
    let model = args.GetSubCommand()
    let mode = args.GetResult(Mode, defaultValue = Batch)
    match model with 
    | GRN _ ->
        let grn_args = args.GetResult GRN
        let numSpecies = args.GetResult (Species, defaultValue = 2)
        let numDiffusibles = grn_args.GetResult (Diffusibles, defaultValue = 2)
        
        match mode with 
        | Generate ->
            let header = sprintf "%d %d" numSpecies numDiffusibles
            let grns = GRN.Generate (speciesNames.[..numSpecies-1]) numDiffusibles |> fst
            let lines = grns |> Array.map (fun grn -> grn.InteractionsString) 
            match chunkSize with
            | Some ch -> 
                lines 
                |> Array.chunkBySize ch
                |> Array.iteri (fun i chunk -> 
                    let fname = sprintf "%s/S%d_M%d_%d.csv" dir numSpecies numDiffusibles i
                    File.WriteAllLines (fname, Array.append [|header|] chunk)
                )
            | None -> 
                let fname = sprintf "%s/S%d_M%d.csv" dir numSpecies numDiffusibles
                File.WriteAllLines (fname, Array.append [|header|] lines)
        
        | Individual -> 
            printSettings solver settings
            let interactions = grn_args.GetResult Interactions |> Array.ofList
            let hypothesis = 
              match grn_args.TryGetResult Hypothesis with
              | Some (reg, coop) -> Some (GrnHypothesis.Create reg coop)
              | None -> None
            let grn = GRN.Create numSpecies numDiffusibles interactions
            GRNs.Program.Process solver settings hypothesis grn |> ignore

        | Batch ->
            printSettings solver settings
            let reg, coop = grn_args.GetResult Hypothesis
            let hypothesis = GrnHypothesis.Create reg coop
            let filetag = if reg = Linear then "Linear" else sprintf "%s%.1f" (reg.ToString()) coop
            printfn "Running GRNs"
            let input_file = args.GetResult File 
            let lines = File.ReadAllLines input_file 
            let header = lines.[0].Split(' ')
            let S = int header.[0]
            let M = int header.[1]
            let grns = 
                lines.[1..]
                |> Array.map (fun line -> GRN.Create S M (line.Split(' ') |> Array.map int))
            let results = 
                grns 
                |> Array.mapi (fun i grn ->
                    printf "%3d: " i
                    GRNs.Program.Process solver settings (Some hypothesis) grn
                )
                |> Array.concat
            let outfile = sprintf "%s/%s_%s" dir filetag (System.IO.Path.GetFileName input_file)
            File.WriteAllLines (outfile, results)

        | Sample -> failwith "Sampling not available for GRNs"

    | CRN _ ->  
        let crn_args = args.GetResult CRN
        let crn_settings = 
            { CRNs.Enumerator.numSpecies = args.GetResult Species
            ; CRNs.Enumerator.numReactions = crn_args.GetResult(Reactions, defaultValue=4)
            ; CRNs.Enumerator.numProducts = crn_args.GetResult(Products, defaultValue=2) 
            ; CRNs.Enumerator.Dvary = crn_args.GetResult(Dvary, defaultValue=CRNs.Enumerator.B)
            ; CRNs.Enumerator.subsets = 1
            ; CRNs.Enumerator.chunkSize = chunkSize
            ; CRNs.Enumerator.verbose = crn_args.Contains Verbose
            }
        let filters : CRNs.Enumerator.Filters = 
            { nontrivial = crn_args.Contains Nontrivial 
            ; conservation = crn_args.Contains Conservation
            ; stronglyConnected = true
            }
        let index = crn_args.GetResult(Index , defaultValue=0)
        let skip = crn_args.GetResult(Skip, defaultValue=[])
        
        match mode with
        | Generate -> 
            CRNs.SuperCRN.generate crn_settings.numSpecies crn_settings.numProducts
            printfn "- Generating subslist for %d species, %d reactions with %d maximum products" crn_settings.numSpecies crn_settings.numReactions crn_settings.numProducts 
            CRNs.Enumerator.Run filters crn_settings
        | Individual ->
            let elements = crn_args.GetResult Ids
            CRNs.Analyser.RunCrn solver settings crn_settings (Array.ofList elements) |> ignore
        (*| Sequence ->
            let elements = crn_args.GetResult Ids
            let z3 = new Microsoft.Z3.Context() 
            let tactics = Solver.PortfolioNoCustomTactic None z3
            tactics |> Array.iteri (fun i tactic -> 
                printfn "-----------------\nTrying tactic #%d" i
                let solver = Solver.CustomA (z3, z3.TryFor(tactic, timeout).Solver)
                CRNs.Analyser.RunCrn solver settings crn_settings (Array.ofList elements) |> ignore
            )*)
        | Sample ->
            let nsamples = crn_args.GetResult Samples
            let seed = crn_args.GetResult Seed
            CRNs.Analyser.Sample solver settings crn_settings filters nsamples seed
        | Batch ->
            let file = args.GetResult File
            let dynamicals = 
              if crn_args.Contains ByteEncoded
                then Microsoft.Research.Biology.StabilityZ3.CRNs.ByteEncoding.readByteEncoding crn_settings file
                else CRNs.Analyser.ReadSubslist (dir + "/" + file) 1 0
                     |> CRNs.Analyser.ElemsToDynamical crn_settings skip
            let results, sat, unsat, unknown = 
                dynamicals
                |> CRNs.Analyser.RunDynamicalSet solver settings crn_settings.verbose
            printfn "\n------------------\nSAT - %d\nUNSAT - %d\nUNKNOWN - %d\n------------------" sat unsat unknown
            let outfile = if crn_settings.subsets > 1 then sprintf "%s/result_%s_%d" dir file index else dir + "/result_" + file
            File.WriteAllText (outfile, results)
        (*| CombineResults ->
            let times = CRNs.Analyser.CombineResults crn_settings dir
            ()*)

    | _ -> failwithf "Not a valid subcommand: %s" (mode.ToString())

    sw.Stop()
    printfn "Completed: %1.1f seconds" ((float sw.ElapsedMilliseconds)/1000.0)

    0 // return an integer exit code
