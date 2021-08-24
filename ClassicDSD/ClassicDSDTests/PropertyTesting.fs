// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.DNA.PropertyTesting

open FsUnit
open FsUnit.Xunit
open Xunit

open FsCheck
open DSDGenerators

open Microsoft.Research.DNA
module Prim         = Microsoft.Research.CRNEngine.Expression
module Mset         = Microsoft.Research.CRNEngine.Mset
module Lib          = Microsoft.Research.CRNEngine.Lib

[<Trait("Category", "DSD")>]
[<Fact>]
let ``Melting before and after normalization`` () =
    Arb.register<Generators>() |> ignore

    let display = Lib.string_of_list Species.display ", "

    let meltIsNormMelt (s:Species.t) =
        TestUtils.debug ("Melting\n" + Species.display s)
        let m = s |> Species.melt
        let n = s |> Species.standard_form Options.default_options |> Species.melt
        TestUtils.debug ("Comparing\n" + display m + "\nand\n" + display n)
        Lib.is_permutation (Species.equal  Options.default_options) m n

    Check.One ({ Config.QuickThrowOnFailure with MaxTest = 20
                                                 StartSize = 2
                                                 EndSize = 15 }, meltIsNormMelt)

//[<Fact>]
//let ``Melting before and after expansion`` () =
//    Arb.register<Generators>() |> ignore
//
//    let build (s, n:int) = { Populations.empty () with initial = true; population = Prim.Float (float n) }
//    let expand ss =
//        let pops = ss |> Mset.from_list |> Mset.collectm build |> Populations.from_pop_info_list
//        let options = Options.default_options |> Options.setPolymers true
//        let expanded_pops = Dsd.compile options "" |> Term.set_populations pops |> Dsd.expandspecies (ref false) |> Term.get_populations
//        expanded_pops
//        |> Populations.get_pop_info_list
//        |> List.map (fun pi -> (pi.calculus_species, Prim.eval (fun _ -> 0.0) pi.population |> int))
//        |> Mset.from_mlist
//        |> Mset.to_list
//
//    let display = Lib.string_of_list Species.display ", "
//
//    let meltIsExpMelt (ss:list<Species.t>) =
//        TestUtils.debug ("Melting\n" + display ss)
//        let m = ss |> List.collect Species.melt
//        let e = ss |> expand |> List.collect Species.melt
//        TestUtils.debug ("Comparing\n" + display m + "\nand\n" + display e)
//        Lib.is_permutation (Species.equal Dsd.empty_meta.options) m e
//
//    Check.One ( { Config.QuickThrowOnFailure with MaxTest = 30
//                                                  StartSize = 0
//                                                  EndSize = 15
//                                                  (*Replay = (1992430798,295889214) |> Random.StdGen |> Some*) }
//                , meltIsExpMelt )
//
//[<Fact>]
//let ``Melting before and after reaction`` () =
//    Arb.register<Generators>() |> ignore
//
//    let build (s, n:int) = { Populations.empty_pop_info s with Populations.initial = true; Populations.population = Prim.Float (float n) }
//    let expand ss =
//        let pops = ss |> Mset.from_list |> Mset.collectm build |> Populations.from_pop_info_list
//        let options = Options.default_options |> Options.setPolymers true
//        let term, pis = Dsd.compile options "" |> Term.set_populations pops |> Dsd.prepare_expansion
//        Dsd.step_expansion (ref false) term pis |> fst
//
//    let display = Lib.string_of_list Species.display ", "
//
//    let test_reaction (r: Reaction.t<Species.t, Prim.t>) =
//        TestUtils.debug ("Testing reaction\n" + Reaction.toString r Species.display Prim.to_string)
//        let mr = r.reactants |> Mset.to_list |> List.collect Species.melt
//        let mp = r.products |> Mset.to_list |> List.collect Species.melt
//        TestUtils.debug ("Comparing reactant strands\n" + display mr + "\nand product strands\n" + display mp)
//        Lib.is_permutation (Species.equal Dsd.empty_meta.options) mp mr |> should be (equal true)
//
//    let reactionsPreserveMelting (ss:list<Species.t>) = (* Consider just using one or two species, to get smaller counter examples *)
//        TestUtils.debug ("Building term from\n" + display ss)
//        ss |> expand |> Term.get_reactions |> List.iter test_reaction
//
//    Check.One ( { Config.QuickThrowOnFailure with MaxTest = 300
//                                                  StartSize = 3
//                                                  EndSize = 20
//                                                  (*Replay = (1992430798,295889214) |> Random.StdGen |> Some*) }
//                , reactionsPreserveMelting )        