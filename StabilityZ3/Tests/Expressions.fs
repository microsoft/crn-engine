// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests.Expressions

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringNumerical


module Expr = Microsoft.Research.CRNEngine.Expression

module ExprFn = Microsoft.Research.Biology.StabilityZ3.ExpressionFunctions
type NumExpr = Expr.t<string>


//to compare sets of expressions (WARNING: order matters)
let toStr es = es |> Set.toArray  |> Array.map(fun e -> ExprFn.ToText false e) |> String.concat "; "  


[<Fact(DisplayName="Expressions denominator (1)")>]
let Denom1 () =     
    let e =  ExprFn.Parse "(a + b)/(c+d)"
    let d = ExprFn.GetDenoms e 
    let d' = [ExprFn.Parse "(c+d)"] |> Set.ofSeq
    
    Assert.Equal (toStr d, toStr d')


[<Fact(DisplayName="Expressions denominator (2)")>]
let Denom2 () =     
    let e =  ExprFn.Parse "(a + b)/(c+d/e)"
    let d = ExprFn.GetDenoms e 
    let d' = [ExprFn.Parse "(c+d/e)"; ExprFn.Parse "e"] |> Set.ofSeq
    
    Assert.Equal (toStr d, toStr d')

[<Fact(DisplayName="Expressions denominator (3)")>]
let Denom3 () =     
    let e =  ExprFn.Parse "(a + b)/(c+d/(e+f))"
    let d = ExprFn.GetDenoms e 
    let d' = [ExprFn.Parse "(c+d/(e+f))"; ExprFn.Parse "(e+f)"] |> Set.ofSeq
    
    Assert.Equal (toStr d, toStr d')

[<Fact(DisplayName="Expressions denominator (4)")>]
let Denom4 () =     
    let e =  ExprFn.Parse "(a + b)/(c+d) + (e+f)/(g+h)"
    let d = ExprFn.GetDenoms e 
    let d' = [ExprFn.Parse "(c+d)"; ExprFn.Parse "(g+h)"] |> Set.ofSeq
    
    Assert.Equal (toStr d, toStr d')
