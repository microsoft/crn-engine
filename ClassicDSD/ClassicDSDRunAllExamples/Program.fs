module Microsoft.Research.CRNEngine.RunAllExamples.Program
open Microsoft.Research.CRNEngine
open Microsoft.Research.CliLibrary.Testing
open Microsoft.Research.DNA

let parser (code:string) : Microsoft.Research.CRNEngine.InferenceSiteGraph.IGraph = 
    let code = if code.Contains "old_syntax" then SLConversion.convertSL code else code
    let (bundle,task) = Dsd.parse_extended Microsoft.Research.DNA.DefaultDatabase.toeholds Microsoft.Research.DNA.DefaultDatabase.specificities code
    let converted = Dsd.convert_expand bundle
    { task = task
    ; nodes = Map.ofList ["DSDmodel", {top=converted;systems=[]}]
    ; edges = Map.empty
    ; expanded = false }

let getExternalSimulation (code:string) : (unit->Table<float> list) option =
    let code = if code.Contains "old_syntax" then SLConversion.convertSL code else code
    let (bundle,task) = Dsd.parse_extended Microsoft.Research.DNA.DefaultDatabase.toeholds Microsoft.Research.DNA.DefaultDatabase.specificities code
    if Dsd.is_jit bundle then
        let f () =
            match bundle with
            | Dsd.bundle.ClassicDSD bundle ->
                // Make sure the seed is set.
                let bundle = if bundle.settings.simulation.seed.IsSome then bundle else { bundle with settings = { bundle.settings with simulation = { bundle.settings.simulation with seed = Some 0 }}}
                let bundle = Dsd.bundle.ClassicDSD bundle
                let jit = JSAPI.get_jit_classic bundle
                let (_,table) = Jit.simulate jit.jit jit.calculus
                [table]
            | Dsd.bundle.Rules bundle ->
                // Make sure the seed is set.
                let bundle = if bundle.settings.simulation.seed.IsSome then bundle else { bundle with settings = { bundle.settings with simulation = { bundle.settings.simulation with seed = Some 0 }}}
                let bundle = Dsd.bundle.Rules bundle
                let jit = JSAPI.get_jit_rules bundle
                let (_,table) = Jit.simulate jit.jit jit.calculus
                [table]
        Some f
    else
        None

[<EntryPoint>]
let main(args) =
    System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
    System.Threading.Thread.CurrentThread.CurrentUICulture <- System.Globalization.CultureInfo.InvariantCulture
    testValidate getExternalSimulation parser args