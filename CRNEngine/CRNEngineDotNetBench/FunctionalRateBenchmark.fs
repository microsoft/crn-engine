// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace CRNEngineDotNetBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open Microsoft.Research.CRNEngine

type FunctionalRateBenchmark() = 
    let mutable luxRRate = Unchecked.defaultof<_>
    let mutable luxRconc = Unchecked.defaultof<_>
    let data = [| 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8 |] //Need to connect this...
    let reaction_from_string = 
        Parser.from_string 
        <| Reaction.parse Species.parse (Expression.parse Parser.name) 
                                        (Expression.parse (Key.parse Species.parse))
                                        (Expression.Float 1.0)
    
    [<GlobalSetup>]
    member this.SetupData() = 
        // "Total" LuxR and LasR (constitutive)
        //->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aR] luxR | luxR ->{dR} |
        //->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aS] lasR | lasR ->{dS} |
        
        let luxRReaction = reaction_from_string "->[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aR] luxR"
        let x = Population.create(false,0.5,"luxR")
        
        let populations = 
            [ ("luxR", 0.5)
              ("rc", 0.5)
              ("Kc", 0.5)
              ("nc", 0.5)
              ("rs", 0.5)
              ("x", 0.5)
              ("K", 0.5)
              ("nc", 2.0)
              ("aR", 0.5) ]
            |> List.map (fun (speciesName, amount) -> Population.create(false,amount,(Species.create speciesName)))
            |> Populations.create
        
        let env = 
            [ ("rc", 0.5)
              ("Kc", 0.5)
              ("nc", 0.5)
              ("rs", 0.5)
              ("x", 0.5)
              ("K", 0.5)
              ("nc", 2.0)
              ("aR", 0.5) ]
            |> Environment.create
        
        let reaction = 
            Reaction.get_sim_reactions_products 1.0 populations env Map.empty [ luxRReaction ]
            |> List.map Lib.fst3
            |> List.exactlyOne
        


        luxRRate <- match reaction.rate with
                    | Rate.Function lambda -> lambda
                    | _ -> failwith "No"
        luxRconc <- fun index -> 
            match index with
            | Inlined.Species i -> data.[i]
            | Inlined.Time -> 0.5

        ()
    
    [<Benchmark>]
    member this.ByExpressionSystem() = 
        let result = luxRRate.key luxRconc
        result
    
    [<Benchmark>]
    member this.DirectFSharp() =
        
        (*
        ("rc", 0.5) //0
        ("Kc", 0.5) //1
        ("nc", 0.5) //2
        ("rs", 0.5) //3
        ("x", 0.5)  //4
        ("K", 0.5)  //5
        ("nc", 2.0) //6
        ("aR", 0.5) //7
        *)

        //[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aR]
        //TODO: determine if access pattern is consistent
        let result = (data.[0]*data.[1]**data.[2] + data.[3]* (data.[4]/data.[5])**data.[2]) /(data.[1]**data.[2]+(data.[4]/data.[5])**data.[6])*data.[7]
        result

    [<Benchmark>]
    member this.DirectFSharpSimplified() =
        
        (*
        ("rc", 0.5) //0
        ("Kc", 0.5) //1
        ("nc", 0.5) //2
        ("rs", 0.5) //3
        ("x", 0.5)  //4
        ("K", 0.5)  //5
        ("nc", 2.0) //6
        ("aR", 0.5) //7
        *)

        //[(rc*Kc^nc+rs*([x]/K)^nc)/(Kc^nc+([x]/K)^nc)*aR]
        //TODO: determine if access pattern is consistent
        let result = (0.5*0.5**2.0 + 0.5* (data.[4]/0.5)**2.0) /(0.5**2.0+(data.[4]/0.5)**2.0)*0.5
        result
