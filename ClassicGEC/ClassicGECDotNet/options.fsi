// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.Options

type t
val default_options : t

val keep_ui_options : t -> t

val getGECProgramText : t -> string
val setGECProgramText : string -> t -> t
val getSimulationOnlyReactions : t -> bool
val setSimulationOnlyReactions : bool -> t -> t
