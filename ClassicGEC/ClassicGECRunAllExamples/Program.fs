module Microsoft.Research.CRNEngine.RunAllExamples.Program
open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary.Testing

// Here we assume that we are only interested in the first solution, equivalent to how the GUI handles inference from a GEC program
let parser code = 
  let solveResult = Microsoft.Research.GEC.GECEngine.solveGEC (ref false) code Microsoft.Research.GEC.Databases.defaultParts Microsoft.Research.GEC.Databases.defaultReactions
  let firstSolution = Microsoft.Research.GEC.GECEngine.getCrnAssignment solveResult.graph solveResult.solution 0 
  firstSolution.model

[<EntryPoint>]
let main(args) =
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    System.Threading.Thread.CurrentThread.CurrentUICulture <- System.Globalization.CultureInfo.InvariantCulture
    testValidate (fun _ -> None) parser args