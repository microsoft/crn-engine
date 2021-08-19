module Microsoft.Research.CRNIntegrationTestLib.Program

open System
open BrowserSetup
open canopy
open Expecto
open canopy.classic

// Remove test groups and timeout from the arguments. Test groups are enclosed in square brackets and comma-separated. The timeout is an argument that's made of only digits. This ought to be refactored using a proper argument parser, although I'm not sure how to integrate that with Expecto's own arguments.
let separateArgs args =
    let (groups,args) = Array.partition (fun (arg:string) -> arg.StartsWith("[") && arg.EndsWith("]")) args
    let groups = Array.map (fun (g:string) -> g.Trim('[',']').Split(',')) groups |> Array.concat
    let (args,timeout) = Array.partition (fun (arg:string) -> Seq.exists (fun c -> not (Char.IsDigit c)) arg) args
    let timeout = if timeout.Length = 0 then 60.0 else float timeout.[0]
    groups,timeout,args

let separateArgsDist args =
    let (groups,timeout,args) = separateArgs args
    // The first argument is the distribution folder. Any remaining arguments will get passed to Expecto.
    let (dir,args) = match List.ofArray args with a::rest -> (a,List.toArray rest) | _ -> (".",args)
    dir,groups,timeout,args

let run args tests =
    //There's state under the UI, so just run the tests serially
    let expectoConfig =
        { Expecto.Impl.ExpectoConfig.defaultConfig with ``parallel`` = false }
    sleep 1 //Let the browser and web server settle - ideally we'd have something explicit to wait on
    runTestsWithArgs expectoConfig args tests