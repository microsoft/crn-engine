module Microsoft.Research.GEC.NumUtil

#if JavaScript

open WebSharper
[<Inline "isFinite($0)">]
#endif
let isNum s = let d = ref 0.0 in (System.Double.TryParse(s, d))

#if JavaScript
[<Inline "parseFloat($0)">]
#endif
let parse_double s =
  let d = ref 0.0 in
  if System.Double.TryParse(s, d)
  then !d
  else System.Double.NaN

[<JavaScript>]
let case_double f v s =
  let d = parse_double s in
  if System.Double.IsNaN d
  then v
  else f d