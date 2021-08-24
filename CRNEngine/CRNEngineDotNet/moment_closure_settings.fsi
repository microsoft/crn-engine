// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Moment_closure_settings
open WebSharper

type monomial = Expression.t<(Species.t * int) list>
type t<'a> when 'a:equality = 
  { order            : int 
  ; initial_minimum  : float
  ; log_evaluation   : bool 
  ; plots            : 'a list
  }

val create             : order : int -> initial_minimum : float -> log_evaluation : bool -> t<'a>
val initAllOrders      : maxSpecies:int -> degree:int -> int [] list
val defaults           : t<'a>
val saturate_plots     : species:Species.t list -> settings:t<monomial> -> t<monomial>
val to_string          : ('a -> string) -> t<'a> -> string
val to_string_monomial : monomial -> string
val from_string        : string -> t<monomial>
val parse              : Parser.t<t<monomial>>