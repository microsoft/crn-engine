// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.LinearCRN

open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringSymbolic

type SynthesisOutput = 
  //| Graphical  // the synthesized networks are visualized (as a table)
  //| Network    // a string representing the networks is displayed
  | Topology   // a string representing the topologies is displayed
  | RDNets 

type types = All | Type1 | Type2
type SynthesisSettings  =
  { isomorphisms  : bool //if true, isomorphisms are ignored during synthesis
  //; relaxedTuring : bool //if true, the relaxed Turing constraints are used
  ; oscil         : bool
  ; noise_amp     : NoiseAmplificationFilter
  ; types         : types
  ; output        : SynthesisOutput
  ; timeout       : int
  }

let And = Array.reduce (fun a b -> BAnd (a,b))
//let Or  = Array.reduce (fun a b -> BOr (a,b))

let initialiseODEs nSpecies (C:Matrix) (X:string[]) = 
    Array.init nSpecies (fun i -> 
            let eq = 
                List.init nSpecies (fun j -> 
                    let k = sprintf "k_{%i,%i}" i j |> Key                   
                    let r = k*C.[i].[j]                    
                    r*(Key X.[j])
                    )
                |> Plus
            X.[i], eq)   

let initialiseDiffusibles (X:string[]) n = 
    Array.init n (fun i ->
            let d = if i > 0 then sprintf "D_{%s}" X.[i] |> Key else Float 1.0
            X.[i], d)
        |> Map.ofSeq

(*
Approach: Construct a differential equation corresponding to the linear CRN system. Include choice variables for reaction rates.
Include constraints to make sure the network is connected by considering powers of the connectivity matrix C (matrix of choice variables). 
The first M species are forced to be diffusible (diffusion rates > 0.0) .
   * N is the number of nodes/species
   * K is the number of interactions/reactions
   * M is the number of diffusible species
*)
let Encode N K nDiffusibles =   
    let X = Array.init N (fun i -> sprintf "X_%i" i)
    
    //rate choice variable matrix
    let C = Array.init N (fun i -> Array.init N (sprintf "c_{%i,%i}" i >> Key)) |> Matrix.Create                
    
    let ode = initialiseODEs N C X
    let D = initialiseDiffusibles X nDiffusibles
    let S = Dynamical.Create(ode, D)    
    
    //choice constraints      
    let rate_choices = C.values |> Array.map(fun row -> row.values) |> Array.concat        
    let rate_choice_vals = rate_choices |> Array.map(fun c -> BOr (BEq(c,Float 0.0), BEq(c,Float 1.0))) |> And    
    let diff_cst = D |> Map.toArray |> Array.map (fun (_, d) -> BGT(d, Float 0.0)) |> And        
    let num_rates = BEq(rate_choices |> List.ofArray |> Plus, K |> float |> Float)        
    
    //non-negative states constraint
    //let state_cst = Xv |> Array.map(fun x -> Ge(x,Float 0.0)) |> And        
    //zero states constraint
    let state_cst = X |> Array.map(fun x -> BEq(Key x, Float 0.0)) |> And        
    
    //connectivity constraints     
    let connected = 
        let Ck = Array.init N (fun k -> C.Pow (k+1)) |> Array.reduce (+) //N-step connectivity        
        Ck.values 
        |> Array.collect(fun row -> row.values |> Array.map (fun x -> BGT(x,Float 0.0)))
        |> And    
        
    let S' = 
        S
        |> Dynamical.addCst rate_choice_vals
        |> Dynamical.addCst num_rates
        |> Dynamical.addCst diff_cst    
        |> Dynamical.addCst state_cst    
        |> Dynamical.addCst connected
        
    S', C


(*  C is a square connectivity (choice) matrix (0.0 or 1.0)
    M is the number of diffusible species
*)
let FromMatrix nDiffusibles (X:string[] option) (C:int[][]) =   
    let nSpecies = C.Length    //number of species    

    let X = 
        match X with 
        | Some x -> x
        | None -> Array.init nSpecies (fun i -> sprintf "X_%i" i)
    let Xv = X |> Array.map Key                
    
    let Cm = C |> Array.map (Array.map (float >> Float)) |> Matrix.Create
    let ode = initialiseODEs nSpecies Cm X
    let D = initialiseDiffusibles X nDiffusibles    
    let S = Dynamical.Create(ode, D)    
    
    let diff_cst = D |> Map.toArray |> Array.map (fun (_, d) -> BGT(d, Float 0.0)) |> And        
       
    //non-negative states constraint
    let state_cst = Xv |> Array.map(fun x -> BGeq(x,Float 0.0)) |> And

    //connectivity constraints     
    let connected = 
        let Ck = Array.init nSpecies (fun k -> Cm.Pow (k+1)) |> Array.reduce (+) //N-step connectivity        
        Ck.values 
        |> Array.collect(fun row -> row.values |> Array.map (fun x -> BGT(x,Float 0.0)))
        |> And    
        
    let S' = 
        S        
        |> Dynamical.addCst diff_cst    
        |> Dynamical.addCst state_cst    
        |> Dynamical.addCst connected
        
    S'

let rec distribute e = function
| [] -> [[e]]
| x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

let rec permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs) 

//TODO: uniqueness constraints for networks vs topologies?
let Generate (opt:SynthesisSettings) solver nSpecies nReactions =   
    let nDiffusibles = 2
    let Sdef, C = Encode nSpecies nReactions nDiffusibles
    
    let S = 
        match opt.types with
        | Type1 -> { Sdef with csts = Array.append Sdef.csts [|BLT (Key "D_{X_1}", Float 1.0)|] }
        | Type2 -> { Sdef with diffusion = Sdef.diffusion |> Map.map (fun _ _ -> Float 1.0)} 
        | All   -> Sdef
   
    //precompute permutations
    let permutations = 
        let d = permute [0..nDiffusibles-1]        //diffusible species ids
        let nd = permute [nDiffusibles..nSpecies-1]       //non-diffusible species ids

        [| for i in d do
            for j in nd do
                yield (List.append i j |> Array.ofList)
        |]    
    
    //uniqueness function
    let MkUnique model =   
        let C'  = C |> Matrix.map (substitute model)
        let expr = 
            if opt.isomorphisms then // Experimental permutations encoding         
                permutations            
                |> Array.map(fun p ->                                 
                    let C'' = C'.Slice p p              
                    BNot(Matrix.Eq C C''))
                |> And
            else
                BNot(Matrix.Eq C C')
        expr
    //settings
    let s = 
        { TuringSymbolic.TuringAnalysisSettings.Default with 
              prevent_oscillations = opt.oscil
            ; prevent_noise_amp    = opt.noise_amp
        }
        
    
    let t = System.DateTime.Now
    //let temp = Solver.CheckTuring ENCODE_TO_Z3 solver s S
    //let tempres = temp.solution
    //let solver = Solver.PortfolioTO (uint32 (10000*opt.timeout))
    //let solver = Solver.Portfolio
    //let solver = Solver.PortfolioNoCustom
    let results = Solver.EnumerateTuring true solver s MkUnique S
    
    //printfn "Done with status %A" (status)
    printfn "Total time: %.2f sec" (System.DateTime.Now - t).TotalSeconds
    
    results


let CrnsToString top (C:(int * Dynamical) list) = 
  C
  |> List.mapi(fun i (_,c) -> 
      let c0 = if top then c.Topology () else c.Network ()
      c0
      |> Array.concat
      |> Array.map (sprintf "%.0f")
      |> String.concat ","        
      |> sprintf "%i,%s" i
      )
  |> String.concat "\n"
  |> sprintf "%s"
