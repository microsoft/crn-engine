[<JavaScript>]
module Microsoft.Research.CRNEngine.Spatial_initial

type core = { inner : float
            ; outer : float
            ; width : float }

type point =  { x     : float
              ; y     : float
              ; width : float
              ; value : float }

type t = { random : float; core : core option; points : point list}

val to_string : t -> string
val defaults : t
val default_core  : core
val default_point : point

val parser : Parser.t<t>