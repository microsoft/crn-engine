// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Tests.ParserTests
open Xunit
open FsUnit
open Operators
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Api
type species = Species
type reaction = Reaction<species,Expression.t<string>,Expression.t<Key<species>>>

let (<->) rev fwd = Some (Rate.MassAction rev), Rate.MassAction fwd
let (!->) fwd = None, Rate.MassAction fwd
let (<~>) rev fwd = Some (Rate.Function rev), Rate.Function fwd
let (!~>) fwd = None, Rate.Function fwd

let float = Expression.Float
let v (x:string) = Expression.Key (Key.Parameter x)
let s (x:string) = Expression.Key (Key.Species (Species.create x)) 
let from_string = Parser.from_string
let ( |>> ) a b c = Parser.( |>> ) a b c
//let sp = ["X"; "Y"; "Z"] |> List.map Parser.pstring |> Parser.choice
let exp_from_string = Parser.from_string <| Expression.parse (Key.parse Species.parse)
let rate_from_string = Parser.from_string <| Rate.parse (Expression.parse Parser.name) (Expression.parse (Key.parse Species.parse))
let reaction_from_string = Parser.from_string <| Reaction.parse Species.parse (Expression.parse Parser.name) (Expression.parse (Key.parse Species.parse)) (Expression.Float 1.0)

let sweep = Sweep.parse |> from_string
let crn_from_string = Crn.parse |> from_string

[<Fact(DisplayName="Parser - boolean")>]
let boolTest() =
  let expected = true in
  let got = "true" |> from_string Parser.pbool in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Parser - expression over species")>]
let ``Expression, species``() =
  let expected = Expression.Plus [float 2.0; Expression.Times [s "X"; float 4.0]] in
  let got = exp_from_string "2+[X]*4" in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Parser - expression over parameters")>]
let ``Expression, parameters``() =
  let expected = Expression.Plus [float 2.0; Expression.Times [s "X"; v "Y"]] in
  let got = "2+[X]*Y" |> exp_from_string in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Parser - functional rate")>]
let rate_functional() =
  let expected = Expression.Plus [float 2.0; s "X"] |> Rate.Function in
  let got = "[2+[X]]" |> rate_from_string in
  Assert.Equal(expected, got)

[<Fact(DisplayName="Parser - reaction functional rate")>]
let reaction_functional() =
  let expected:reaction = Reaction.create([],[Species.create "Y"], !~> (Expression.Plus [float 2.0; s "X"]), [Species.create "Z"]) in
  let got:reaction = reaction_from_string "Y ->[2+[X]] Z" in
  Assert.Equal(expected, got)
  //TODO: Fix structural equality of Reaction type
  let printReaction (x:reaction) = x.string Species.to_string (Expression.to_string id) Functional2.to_string
  Assert.Equal(printReaction expected, printReaction got)

[<Fact(DisplayName="Parser - single sweep")>]
let ``Sweep, single``() =
  let expected = Sweep.create("s", [["N"] => [[Expression.Float 0.0]] ]) in
  let got = "s = [N = [0.0]]" |> sweep in
  Assert.Equal<Sweep>(expected, got)


let crn_errors = Crn.parse |> Parser.from_string_find_errors

[<Fact(DisplayName="Parser - multiset syntax shorthands")>]
let multiset_syntax_shorthands() =
  let code = """
2X |
3Y |
2X + Y ->{2.0} X + B |
2X + Y -> X + B"""

  crn_from_string code 
    |> ignore

[<Fact(DisplayName="Parser - find parse errors")>]
let ``Find parse errors``() =
  
  let am_text = """directive simulation { final = 1.0; points = 20 }
directive sample 1.0 10000

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
Surprise Giraffe
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20"""

  Assert.Throws<Parser.Exception> (fun _ -> crn_from_string am_text |> ignore) |> ignore

  let program, errors = crn_errors am_text

  Assert.Equal(2, errors.[0].row)
  Assert.Equal(7, errors.[1].row) //The error goes into the reaction after, we may want to return error line number ranges CG

  Assert.Equal(2, errors.Length)

[<Fact(DisplayName="Parser - misspelt directive keyword")>]
let ``Misspelt directive keyword``() =
  
  let am_text = """directive simulation { final = 1.0; points = 20 }
direc7777tive sample 1.0 10000

X + Y ->{r} X + B |
Y + X ->{r} Y + B"""

  Assert.Throws<Parser.Exception> (fun _ -> crn_from_string am_text |> ignore) |> ignore

  let _, errors = crn_errors am_text

  Assert.Equal(2, errors.[0].row)
  Assert.Equal(1, errors.Length)

[<Fact(DisplayName="Parser - unknown directive")>]
let ``Unknown directive``() =
  
  let am_text = """directive simulation { final = 1.0; points = 20 }
directive solveit

X + Y ->{r} X + B |
Y + X ->{r} Y + B"""

  Assert.Throws<Parser.Exception> (fun _ -> crn_from_string am_text |> ignore) |> ignore

  let _, errors = crn_errors am_text

  Assert.Equal(2, errors.[0].row) //the parser itself is reading ahead through newlines, may want ranges CG)
  Assert.Equal(1, errors.Length)

[<Fact(DisplayName="Parser - unterminated reaction rate")>]
let ``Unterminated reaction rate``() =
  
  let am_text = """directive simulation { final = 1.0; points = 20 }

  X + Y ->{r X + B |
  Y + X ->{r} Y + B"""

  Assert.Throws<Parser.Exception> (fun _ -> crn_from_string am_text |> ignore) |> ignore

  let _, errors = crn_errors am_text

  Assert.Equal(3, errors.[0].row)
  Assert.Equal(1, errors.Length)

[<Fact(DisplayName="Parser - no error for styled")>]
let ``No errors for styled``() =
  
  let am_text = """
 directive simulation { final = 20.0
                      ; points = 10 }
  X + Y ->{r} X + B"""

  crn_from_string am_text |> ignore //No error

  let program, errors = crn_errors am_text

  Assert.Empty errors
  Assert.True program.IsSome

[<Fact(DisplayName="Parser - comments - single line ")>]
let ``Single line comment`` () = 
  let expected = true in
  let got = "true // comment" |> from_string Parser.pbool in
  Assert.Equal(expected, got)
  

[<Fact(DisplayName="Parser - comments - multi-line ")>]
let ``Multi-line comment`` () = 
  let expected = Expression.Plus [float 2.0; Expression.Times [s "X"; float 4.0]] in
  let got = exp_from_string "2+[X]*(* multi
    line 
    comment *)4" in
  Assert.Equal(expected, got)

    
[<Fact(DisplayName="Parser - comments - nested ")>]
let ``Nested comments`` () = 
  let expected = Expression.Plus [float 2.0; Expression.Times [s "X"; float 4.0]] in
  let got = exp_from_string "2+(* 
      outer
         (* inner comment *)
      comment 
    *)[X]*4" in
  Assert.Equal(expected, got)
  

[<Fact(DisplayName="Parser - comments - mixed")>]
let ``Parsing, comments``() =
  
  let am_text = """
 // comment 1
 directive simulation { final = 20.0 //comment 2
                      ; (* comment 3*)points = (* comment 4*)10 }
  (* 
     comment 5
     (* comment 6 *) 
  *)
  X + Y ->{r} X + B //comment 7"""

  crn_from_string am_text |> ignore //No error

  let program, errors = crn_errors am_text

  Assert.Empty errors
  Assert.True program.IsSome

[<Fact(DisplayName="Parser - log expression")>]
let ``Parse log expression``() = 
  let expected = Expression.Times [Expression.Log (Expression.div (v "K") (s "x")); s "x"]
  let got = exp_from_string """log(K/[x])*[x]"""
  Assert.Equal (expected, got)

[<Fact(DisplayName="Parser - multiple times")>]
let ``Parse multiple times``() = 
  let text = """
directive simulation {final = 1000}
directive simulator deterministic
| init X 1000
| init X 1000 @ 250
| init X 1000 @ 500
| init X 1000 @ 750
| X ->{0.1}"""
  let inferencegraph = match Parser.from_string_first_error_with_msg InferenceSiteGraph.parse text with 
                       | Choice1Of2 m -> m
                       | Choice2Of2 err -> failwith ""
  let model = inferencegraph.nodes |> Map.toSeq |> Seq.head |> snd
  let crn = model.top
  Assert.Equal (1, crn.settings.simulation.plots.Length)

(*
let contentEqualWithin (tolerance:float) (x:float[]) =
    NHamcrest.CustomMatcher<obj>(
        sprintf "Not equal within tolerance %A" x,
        fun c -> match c with
                 //| :? list<_> as l -> l |> List.exists(fun i -> i = x)
                 | :? array<float> as a -> not (Array.zip x a |> Array.exists (fun (x,y) -> abs (x-y) > tolerance))
                 //| :? seq<_> as s -> s |> Seq.exists(fun i -> i = x)
                 //| :? System.Collections.IEnumerable as e -> e |> Seq.cast |> Seq.exists(fun i -> i = x)
                 | _ -> false)
*)