[<JavaScript>]
module Microsoft.Research.CRNEngine.Moment_closure_settings
open System.Diagnostics

type monomial = Expression.t<(Species * int) list>

[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type t<'a> when 'a:equality = 
  { order            : int 
    initial_minimum  : float
    log_evaluation   : bool 
    plots            : 'a list
  }

let create k i_min log_e = 
  { order = k
    initial_minimum = i_min
    log_evaluation = log_e 
    plots = []
  }

let defaults = { order = 3
                 initial_minimum = 1e-16
                 log_evaluation = false
                 plots = []
               }

let rec initAllOrders (maxSpecies: int) (degree:int) =
  let indices = [0..maxSpecies-1]
  let rec init indices deg=
    match indices with
    | []      -> [Array.create maxSpecies 0]
    | i::rest -> [0..deg] 
                  |> List.fold (fun state d -> 
                      let otherOrders = init rest (deg - d)
                      let orders      = otherOrders
                                          |> List.map (fun o -> let o' = Array.copy o 
                                                                o'.[i] <- d
                                                                o' )
                      orders @ state) []
  init indices degree
   // filter out E[1] moments
   |> List.filter (fun i -> not (Array.forall (fun x -> x = 0) i))

let saturate_plots (species:Species list) settings : t<monomial> = 
  if List.isEmpty settings.plots
  then
    let monomials = initAllOrders species.Length settings.order
    let plots = monomials |> List.map (List.ofArray >> List.zip species >> Expression.Key)
    { settings with plots = plots }
  else
    settings

let to_string (plotPrinter: 'a -> string) (mc : t<'a>) =
  let plots = mc.plots 
              |> List.map plotPrinter
              |> String.concat "; "
  sprintf "{order = %i; initial_minimum=%f; log_evaluation=%b; plots=[%s]}" mc.order mc.initial_minimum mc.log_evaluation plots

let to_string_monomial (m:monomial) : string =
  let printFactor (sp:Species, n:int) = 
    match n with 
    | 1 -> sp.name
    | _ -> sprintf "%s^%i" sp.name n
  let printMonomial mon = mon |> List.filter (snd >> (<>) 0)
                              |> List.map printFactor
                              |> String.concat "*"
                              |> sprintf "<%s>"
  m |> Expression.to_string printMonomial

let (.>>) a b = Parser.(.>>) a b
let (>>.) a b = Parser.(>>.) a b
let (.>>.) a b = Parser.(.>>.) a b
let (|>>) a b = Parser.(|>>) a b
let canonicalMonomial ss = List.fold (fun acc (sp, i) -> match Map.tryFind sp acc with 
                                                         | None   -> acc.Add(sp, i)
                                                         | Some n -> acc.Add(sp, n+i)
                                                         ) Map.empty ss
                            |> Map.toList
let pmonomial = Parser.spaces >>. Parser.angleBrackets
                 (Parser.sepBy1 
                   ((Parser.name .>> Parser.spaces |>> Species.create)
                      .>>. Parser.choice [ Parser.kw "^" >>. Parser.pint32 .>> Parser.spaces |>> int
                                           Parser.preturn 1 ])
                   (Parser.kw "*"))
                 |>> canonicalMonomial
let pplot = Expression.parse pmonomial
let parse_defaults (defaults) = 
  Parser.record defaults [
    "order", Parser.pint32            |>> fun k  (s:t<monomial>) -> { s with order = k }
    "initial_minimum", Parser.pfloat  |>> fun im (s:t<monomial>) -> { s with initial_minimum = im }
    "log_evaluation", Parser.pbool    |>> fun le (s:t<monomial>) -> { s with log_evaluation = le }
    "plots", Parser.list_of pplot     |>> fun ps (s:t<monomial>) -> { s with plots = ps }
  ]
let parse = parse_defaults defaults
let from_string (s:string) = Parser.from_string parse s