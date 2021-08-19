[<JavaScript>]
module Microsoft.Research.GEC.Options

type t = { gecProgram:string
         ; simulationOnlyReactions:bool }

let default_options = { gecProgram=""
                      ; simulationOnlyReactions=false }

let keep_ui_options (opts:t) = opts

let getGECProgramText (opts:t) = opts.gecProgram
let setGECProgramText (s:string) (opts:t) = {opts with gecProgram=s}
let getSimulationOnlyReactions (opts:t) = opts.simulationOnlyReactions
let setSimulationOnlyReactions (b:bool) (opts:t) = {opts with simulationOnlyReactions=b}
