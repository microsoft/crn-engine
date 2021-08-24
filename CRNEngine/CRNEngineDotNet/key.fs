// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>] 
type Key<'s> when 's:equality = 
  | Parameter of string
  | Rate      of string
  | Species   of 's
  | Time
  static member scale (scale:float) (k:Key<'s>) = 
    match k with 
    | Species s -> Expression.Divide{div1 = Expression.t.Key(Species s); div2 = Expression.Float(scale)}
    | _ -> Expression.Key k 
  static member to_string (f:'s -> string) (k:Key<'s>) =
    match k with
    | Parameter n  -> n
    | Rate m       -> "[" + m + "]"
    | Species s    -> "[" + f s + "]"  
    | Time         -> "[time]"
  ///A plot expression can be a single species with no brackets, or an expression of keys of species. 
  static member to_string_plot (f:'s -> string) (k:Key<'s>) = 
    match k with
    | Key.Species s -> f s
    | _ -> Key.to_string f k
  static member to_matlab (f:'s -> string) (time:string) (k:Key<'s>) =
    match k with
    | Parameter n  -> n
    | Rate m       -> m
    | Species s    -> f s
    | Time         -> time
  static member map (f:'s -> 's2) (k:Key<'s>) =
    match k with
    | Species s   -> Species (f s)
    | Rate m      -> Rate m
    | Parameter n -> Parameter n
    | Time -> Time
  static member collect (f:'s -> 's2 list) (k:Key<'s>) =
    match k with
    | Species s   -> List.map Species (f s)
    | Rate m      -> [Rate m]
    | Parameter n -> [Parameter n]
    | Time -> [Time]
  static member parse (ps:Parser.t<'s>) = 
    Parser.choice [
      Parser.kw "[time]" >>. Parser.preturn Time;
      Parser.sqBrackets ps .>> Parser.spaces |>> Species;
      Parser.name .>> Parser.spaces |>> Parameter;
    ]
  static member from_string (ps:Parser.t<'s>) (str:string) = Parser.from_string (Key.parse ps) str
  static member parse_plot (ps:Parser.t<'s>) =   
      (ps |>> fun s -> Expression.Key (Key.Species s)) <|> Expression.parse (Key.parse ps)
  static member inline_keys parameterEnv (rateEnv:Map<string, Expression.t<Inlined<'s>>>) (k:Key<'s>) = 
    match k with 
    | Key.Parameter s -> Expression.Float (Environment.find parameterEnv s)
    | Key.Species   s -> Expression.Key   (Inlined.Species s)
    | Key.Time        -> Expression.Key   (Inlined.Time)
    | Key.Rate      r -> 
        match Map.tryFind r rateEnv with 
        | None      -> failwith <| "Rate " + r + " is undefined."
        | Some rate -> rate
  // TODO: Factor out duplication with above method. 
  static member inline_rates_env (e:Environment.t) (rates: Map<string,Expression.t<Key<'s>>>) = 
    let rec inlineKey (k:Key<'s>) = 
      match k with
      | Key.Parameter p -> Expression.Float (Environment.find e p)
      | Key.Species s   -> Expression.Key (Inlined.Species s)
      | Key.Time        -> Expression.Key (Inlined.Time)
      | Key.Rate      r -> 
          match Map.tryFind r rates with
          | None      -> failwith <| "Rate " + r + " is undefined."
          | Some rate -> Expression.expand inlineKey rate
    Map.map (fun _ -> Expression.expand inlineKey >> Expression.simplify) rates
  static member eval_massaction (env:Environment.t) v = 
    let f0 x = Environment.find env x
    Expression.eval f0 v 
  static member eval_function (env:Environment.t) (ratesEnv:Map<string,Expression.t<Inlined<int>>>) e = 
    let f2 (x:Key<int>)  = Key.inline_keys env ratesEnv x
    let f3 (x:Expression.t<Key<int>>) = Expression.expand f2 x |> Expression.simplify
    Expression.to_lambda (f3 e)
