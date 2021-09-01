#load "load-project.fsx"

open SiteGraphReactor
open Parser

let site = Syntax.parse_site Syntax.parse_name
let strand, gate = ExtendedSyntax.strand_gate site

let strands = (attempt gate <|> strand) |> Syntax.sep_by_bars

let complex = strands |> Syntax.bracket "[" "]" |> Syntax.counted
let complexes = complex |> Syntax.sep_by_bars

let species = attempt (strands |>> fun s -> [(1,s)]) <|> complexes

let parse_syntax = spaces >>. sepEndBy Syntax.parse_toehold spaces1 .>>. species .>> spaces .>> eof

let toeholds = sepEndBy Syntax.parse_toehold spaces1

let prog_3i4half = "  1000 * [ <tether T2^!a X*!b T1^>
         | <A X!b T2^*!a> ]
| 1200 * [ <T1^* X!c RA>
         | <X*!c A*!d>
         | <A!d> ]"


let sites = sepBy1 site spaces1

let upper = sites |> Syntax.bracket "<" ">"
let lower = sites |> Syntax.bracket "{" "}"
let double = sites |> Syntax.bracket "[" "]"
let left_hp = sites |> Syntax.bracket "<" "}"
let right_hp = sites |> Syntax.bracket "{" ">"

run Syntax.parse_name ""

run ExtendedSyntax.parser_enzymes "<x t^>"
run ExtendedSyntax.parser_enzymes ""

run toeholds ""

run species "<x t^>"
run gate "[2]"
run strands "{1*}<x t^>}"
run strands "{1*}|{3}"
run species "{1*}<x t^>"
run parse_syntax "{1*}<x t^>"
run parse_syntax "{1*}"
run parse_syntax "{1*} "
run parse_syntax "<a t^>"
run parse_syntax "<a t^>[x]"

run lower "{1*}"
run site "1*"
run sites "1*"
run sites "1*}"
run (Syntax.skw "}") "}"
run (Syntax.kw "}") "}"

run parse_syntax prog_3i4half

run pint32 "32"