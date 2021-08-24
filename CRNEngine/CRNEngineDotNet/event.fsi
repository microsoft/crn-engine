// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Event
open WebSharper

//'s for species, 'v for value
type population<'s,'v> = Population.t<'s,'v> 
type populations<'s,'v> when 's:equality = Populations.t<'s,'v> 
type target<'s,'v> when 's:equality =
  | Species of Species: populations<'s,'v>
  | [<Constant "OutputPoint">] OutputPoint

type t<'s,'time, 'v> when 's:equality = { 
  time: 'time;
  target: target<'s,'v>;
}
type env = Map<string,float>

val to_string : ('s -> string) -> ('time -> string) -> ('v -> string) -> t<'s, 'time, 'v> -> string
val map_species : ('a -> 'b) -> t<'a,'t,'v> -> t<'b,'t,'v>
val map_value   : ('a -> 'b) -> t<'s,'t,'a> -> t<'s,'t,'b>
val map_time    : ('a -> 'b) -> t<'s,'a,'v> -> t<'s,'b,'v>
//val inline_env : ('t -> float) -> ('v -> 'a) -> t<'s,'t,'v> -> t<'s,float,'a>
//val eval : ('v -> float) -> t<'s,'t,'v> -> t<'s,'t,float>
val create : populations<'s,'v> -> 't -> t<'s,'t,'v> 

