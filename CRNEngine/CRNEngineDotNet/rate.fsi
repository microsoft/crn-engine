[<JavaScript>] 
module Microsoft.Research.CRNEngine.Rate
open WebSharper
type env = Map<string,float>
[<NamedUnionCases>]
type  t<'v,'e>  = 
  | MassAction of MassAction:'v
  | Function of Function:'e
type gui = t<string,string>

val map : ('v1 -> 'v2) -> ('e1 -> 'e2) -> t<'v1,'e1> -> t<'v2,'e2>
val inlineRatesEnvironment : Environment.t 
                              -> Map<string, Expression.t<Key.t<'s>>>
                              -> Map<string, Expression.t<Key.inlined_t<'s>>>
val eval : Environment.t -> Map<string,Expression.t<Key.inlined_t<int>>> -> t<Expression.t<string>,Expression.t<Key.t<int>>> -> t<float,Expression.lambda<Key.inlined_t<int>>>
val to_string : ('v -> string) -> ('e -> string) -> t<'v,'e> -> string
val to_string_bare : ('v -> string) -> ('e -> string) -> t<'v,'e> -> string
val to_gui : ('v -> string) -> ('e -> string) -> t<'v,'e> -> gui
val from_gui : (string -> 'v) -> (string -> 'e) -> gui -> t<'v,'e>
val parse : Parser.t<'v> -> Parser.t<'e> -> Parser.t<t<'v,'e>>