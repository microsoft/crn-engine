// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Fix

  let eval (env:Environment.t) (ratesEnv:Map<string,Expression.t<Key2<int>>>) (r:Rate<Value,Expression.t<Key<int>>>) : Rate<float,Lambda.t<Key2<int>>> = 
    (* WS eta-expansion fix *)
    let f0 x                            = Environment.find env x
    let f1 x                            = Expression.eval f0 x 
    let f2 (x:Key<int>)               = Expression.inline_keys env ratesEnv x
    let f3 (x:Expression.t<Key<int>>) = Expression.expand f2 x |> Expression.simplify
    (*\ WS eta-expansion fix *)
    match r.map f1 f3 with //NB check this
    (*(Expression.convert (Key.inline_variable env))*) 
    | MassAction m -> MassAction m
    | Function exp -> Function (Expression.to_lambda exp)
  /// inline all parameters and rate symbols in a rates environment. NB: does not terminate if rate symbols are mutually recursive
  let inlineRatesEnvironment (parametersEnv:Environment.t) (ratesEnv:Map<string, Expression.t<Key<'s>>>) : Map<string,Expression.t<Key2<'s>>> =
    let rec inlineKey (k:Key<'s>) = 
      match k with
        | Key.Species s   -> Expression.Key (Key2.Species s)
        | Key.Time        -> Expression.Key (Key2.Time)
        | Key.Parameter p -> Expression.Float (Environment.find parametersEnv p)
        | Key.Rate      r -> Expression.expand inlineKey ratesEnv.[r]
    Map.map (fun _ -> Expression.expand inlineKey >> Expression.simplify) ratesEnv 