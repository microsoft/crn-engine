module Microsoft.Research.DNA.CRNRegressions

open System.IO
open FsUnit
open Xunit

open TestUtils

open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Parser

let test_program crnCode dsdCode =
  
  // simplifies expressions so that they match Visual DSD
  let simplifyRate = Reaction.map id Expression.simplify Expression.simplify
  
  // load Classic DSD program
  let bundle        = dsdCode |> SLConversion.convertSL |> Dsd.parse 
  let convertedCode = bundle |> Dsd.convert_expand
  // renames a dsd species name to its origin species
  let renamer (sp:Species) = Species.create (convertedCode.attributes.[sp.name]).structure
  let rawDsd        = bundle 
                      |> Dsd.convert_expand 
                      |> fun c -> c.map renamer
  let dsd:Crn = Crn.create
                    "Classic DSD's CRN"
                    rawDsd.settings
                    (rawDsd.reactions |> List.map simplifyRate )
                    (rawDsd.initials  |> List.map (Initial.mapValues Expression.simplify))
                    rawDsd.attributes
                    true

  // species parser, to load CRN species of the form "<a b c>[d e]" (quotes included)
  let pQuotedSpecies = kw "\"" >>. manySatisfy ((<>) '\"') .>> kw "\""
                       |>> Species.create

  // load Visual DSD's CRN
  let crnParser = LegacyParser.parse_legacy_SL pQuotedSpecies
  let crn       = crnCode |> Parser.from_string crnParser
  let spec      = { crn with name = "Visual DSD's CRN" }
  check_crn_equal_relaxed dsd spec

let test_example (crnFile, dsdFile) : string option =
  try
    let crn = File.ReadAllText crnFile
    let dsd = File.ReadAllText dsdFile
    test_program crn dsd
    None
  with e -> let testName = Path.GetFileNameWithoutExtension dsdFile
            Some (sprintf "Test \"%s\":\n%s" testName e.Message)

let try_many testees =
  testees
  |> List.choose test_example
  |> function | [] -> ()
              | es -> es  |> String.concat "\n\n"
                          |> sprintf "The following tests failed:\n%s"
                          |> failwith

let trimEmpty = String.filter (fun c -> List.exists ((=) c) [' ';'\r';'\n';'\t'])


let TEST_FOLDER         = "testData"
let CRN_COMPARISON_DIR  = "crnComparison"

[<Fact>]
[<Trait("Category", "Slow")>]
let ``CRN equality regression tests`` () =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let (+/) a b = Path.Combine(a, b)
  
  // let projectDir = read_resource "projectDir.value" |> trimEmpty
  let projectDir  = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let testsPath   = projectDir +/ TEST_FOLDER +/ CRN_COMPARISON_DIR
  let crns        = Directory.EnumerateFiles (testsPath, "*.crn") |> Seq.toList
  let dsds        = Directory.EnumerateFiles (testsPath, "*.dna") |> Seq.toList
  debug (sprintf "Testing %d DSD programs" (Seq.length dsds))
  List.zip crns dsds |> try_many