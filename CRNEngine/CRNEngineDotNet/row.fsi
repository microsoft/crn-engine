// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Row

type t<'v> = { time: float; values: 'v [] }

val create : time:float -> values:'v list -> t<'v>
val from_array : time:float -> values:'v[] -> t<'v>
val interpolate_float : t:float -> t0:float -> t1:float -> x0:float -> x1:float -> float
val from_list : row:float list -> t<float>
val interpolate_reverse : interpolate_point:(float -> float -> float -> 'v -> 'v -> 'v) -> rows:t<'v> list -> times:float list -> t<'v> list
val to_string : separator:string -> value_to_string:('v -> string) -> r:t<'v> -> string
