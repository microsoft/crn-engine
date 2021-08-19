module Microsoft.Research.CRNEngine.Prior
open WebSharper

type posterior = Posterior.t

type range = {
  LowerBound:float;
  UpperBound:float;
}

type normal = { Mean:float; Stdev:float }
type truncated_normal = { Mean:float; Stdev:float; Range:range }

[<NamedUnionCases>]
type distribution = 
  | Uniform of Uniform:range
  | Normal of Normal:normal
  | TruncatedNormal of truncated_normal
  | Posterior of Posterior:posterior

type interval = 
  | [<Constant "Real">] Real
  | [<Constant "Log">] Log

type variation =
  | [<Constant "Random">] Random
  | [<Constant "Fixed">] Fixed
  | [<Constant "Initial">] Initial
  | [<Constant "Multiple">] Multiple

type t = {
  interval: interval;
  distribution: distribution;
  variation: variation;
}

val empty : t
val to_string : t -> string
val is_multiple : t -> bool
val is_fixed : t -> bool
val fix : t -> t
val vary : t -> t
val parse : Parser.t<t>