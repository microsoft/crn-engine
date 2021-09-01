// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.NotebookAPI.LinearCrnAPI

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.LinearCRN

type Solver = Portfolio | NLsat | Custom

let ENCODE_TO_Z3 = true //for larger systems, it is more efficient to avoid the intermediate constraints

let RDNetsCompare htmlView NS NR (C:(int * Dynamical * float)[]) = 
    C
    |> Array.map(fun (_,c,_) -> c)
    |> RDnets.testDynamical NS NR
    |> htmlView
     
let Compute (htmlView:string->unit) (disp:string -> unit) compute opt solver NS NR fname = 
    let str = 
        if compute 
        then 
            let res : string = Generate opt solver NS NR |> CrnsToString true
            System.IO.File.WriteAllText (fname, res);
            res
        else
            System.IO.File.ReadAllText fname 
    
    match opt.output with
    | Topology  -> str |> disp
    | RDNets    -> 
        str.Split('\n')
        |> Array.map (fun line -> 
            let elems = line.Split(',') |> Array.map float
            int elems.[0], elems.[1..] |> Array.chunkBySize NS
        )
        |> RDnets.testTopologies NS NR |> htmlView

let Solve hv opt C = 
    let S = FromMatrix 2 None C
    match opt.noise_amp with
    | TuringSymbolic.Strict -> DynamicalAPI.CheckTuringStrict hv S
    | TuringSymbolic.Neutral -> DynamicalAPI.CheckTuring hv S
    | TuringSymbolic.No -> DynamicalAPI.CheckTuringRelaxed hv S
    | TuringSymbolic.LessThan -> failwith "" //DynamicalAPI.CheckTuringRelaxed S


let CrnsToSvgTable htmlView (C:(int * Dynamical)[]) = 
    C
    |> Array.map(fun (i, TS) -> 
        let svg = Visualization.ToSVG 200.0 TS
        sprintf "<tr><td>%i</td><td>%s</td><td>%.2f</td></tr>" i svg TS.solution.Time)
    |> String.concat "\n"
    |> sprintf "<center><table><tr><th>ID</th><th>Network</th><th>Time (sec)</th></tr>%s</table></center>"
    |> htmlView
