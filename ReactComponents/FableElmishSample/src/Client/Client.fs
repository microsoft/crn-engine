// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Client

open Elmish
open Elmish.React
open App
 
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
