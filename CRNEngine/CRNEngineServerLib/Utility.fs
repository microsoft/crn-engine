module Microsoft.Research.CRNEngineServerLib.Utility

// Lock on this object to make printfn thread-safe.
let private lockobj = new obj()

let printlog format =
    let out l = lock lockobj (fun _ -> printfn "%O %s" System.DateTime.Now l)
    Printf.kprintf out format
