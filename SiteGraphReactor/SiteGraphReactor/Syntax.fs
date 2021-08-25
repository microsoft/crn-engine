// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module SiteGraphReactor.Syntax
open Parser

(* Syntax *)
type site =
  { domain : Strands.domain
  ; binding : string option }
type strand = site list

type complex = strand list
type complexes = (int * complex) list

type toehold =
  { name : string
  ; bind_rate : float
  ; unbind_rate : float }

type t = toehold list * complexes

(* Parsing *)
let mk_domain ((n, t), c) =
  { SiteGraphReactor.Strands.name = n
  ; SiteGraphReactor.Strands.complemented = c
  ; SiteGraphReactor.Strands.toehold = t }

let mk_site (d, b) =
  { domain = d
  ; binding = b }

let parse_name = many1Satisfy (fun c -> isLetter c || isDigit c || c = '_')
  
let after p s = p .>>. opt (pstring s) |>> function n, None -> (n, false) | n,_ -> (n,true)

let parse_domain name =
  let domainname = after name "^"
  after domainname "*" |>> mk_domain

let parse_site name =
  name |> parse_domain .>>. (pstring "!" >>. name |> opt) |>> mk_site

let kw s = pstring s .>> spaces
let skw s = spaces >>. kw s

let bracket l r = between (kw l) (skw r)

let parse_toehold =
  kw "new" >>. parse_name .>> kw "@" .>>. (pfloat .>> skw "," .>>. pfloat)
  |>> fun (n, (b, u)) -> { name = n; bind_rate = b; unbind_rate = u }

let counted p =
  p |>> (fun s -> (1,s))
  <|>
  ((pint32 .>> skw "*") .>>. p)

let sep_by_bars p = sepBy1 p (skw "|")

let errMsg msg (r, c) = sprintf "Expecting %s at row %d column %d" msg (r+1) (c+1)
let run_parser p s =
  match run_result p s with
  | OkEmpty (result, _)
  | OkConsumed (result, _) -> result
  | FailEmpty (error, (r,c))
  | FailConsumed (error, (r,c)) -> raise (Exception(errMsg error (r,c), [|{row=r+1; column=c; text=error}|] ))

let parser_syntax site =
  let sites = sepBy1 site spaces1
  let strands = sites |> bracket "<" ">"  |> sep_by_bars

  let complex = strands |> bracket "[" "]"
  let complexes = complex |> counted |> sep_by_bars

  let species = complexes <|> (strands |>> fun s -> [(1,s)] )

  spaces >>. sepEndBy parse_toehold spaces1 .>>. species .>> spaces .>> eof

//let parse_syntax site = parser_syntax site |> run_parser

let parser = parse_name |> parse_site |> parser_syntax
let parse = parser |> run_parser