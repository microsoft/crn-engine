[<JavaScript>]
module Microsoft.Research.CRNEngine.Spatial_settings 
open WebSharper

type boundary = | [<Constant "Periodic">] Periodic | [<Constant "ZeroFlux">] ZeroFlux   // ZeroFlux currently not implemented

type t<'s> when 's:equality =
  { parameters  : Parameter.t list
  ; diffusibles : ('s*Value) list
  ; dimensions  : int
  ; boundary    : boundary
  ; xmax        : float
  ; nx          : int
  ; dt          : float
  ; default_diffusion : float
  ; random      : float }

val defaults : t<'s>
val map: ('a -> 'b) -> t<'a> -> t<'b>
val mapExpressions: (Expression.t<'a> -> Expression.t<'b>) -> t<'a> -> t<'b>
val to_string: ('s -> string) -> t<'s> -> string
val parse: Parser.t<'s> -> Parser.t<t<Key.t<'s>>>