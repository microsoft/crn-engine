module Microsoft.Research.CRNEngine.Tests.ExpressionTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine.Expression
open Microsoft.Research.CRNEngine

let read (s:string) = from_string Parser.name s

[<Fact(DisplayName="Expressions - Parser - exponent precedence")>]
let parse_exponent_precedence() = 
  //let expected:t<string> = ((Float 2.0) * Power{base_=Float 3.0;exponent=Float 4.0}) in
  let expected:t<string> = ((Float 2.0) * ((Float 3.0) ** (Float 4.0))) in
    Assert.Equal(expected, read "2*3^4")

[<Fact(DisplayName="Expressions - Parser - parentheses")>]
let parse_parentheses () = 
  let expected:t<string> = (Float 2.0 + Float 3.0) * Float 4.0 in
    Assert.Equal(expected, read "(2+3)*4")

[<Fact(DisplayName="Expressions - Parser - minus precedence")>]
let parse_minus_precedence () = 
  let expected:t<string> = (((Float 2.0 + float 3.0) - Float 4.0) - Float 5.0) + Float 7.0 in
    Assert.Equal(expected, read "2+3-4-5+7")

[<Fact(DisplayName="Expressions - Parser - plus minus precedence")>]
let parse_plus_minus_precedence () = 
  let expected:t<string> = Float 2.0 + (Float 3.0 * Float 4.0) in
    Assert.Equal(expected, read "2+3*4")

[<Fact(DisplayName="Expressions - Parser - minus power precedence")>]
let eval_minusPowerPrecedence () = 
  let expected:t<string> = Float 0.0 - ((Key "c") ** (Float 2.0)) * Key "k"
  let actual = read "-c^2*k"
  Assert.Equal(expected, actual)


[<Fact(DisplayName="Expressions - Parser - variables")>]
let parse_variables () = 
  let expected:t<string> = (Float 2.0 + (Key "x" * Float 4.0)) in
    Assert.Equal(expected, read "2+x*4")

[<Fact(DisplayName="Expressions - Parser - round")>]
let parse_round () = 
  let expected:t<string> = Round (Float 2.4)
  Assert.Equal(expected, read "round(2.4 )")

[<Fact(DisplayName="Expressions - Parser - ceiling")>]
let parse_ceiling () = 
  let expected:t<string> = Ceiling (Float 2.4)
  Assert.Equal(expected, read "ceiling ( 2.4) ")

[<Fact(DisplayName="Expressions - Parser - floor")>]
let parse_floor () = 
  let expected:t<string> = Floor (Float 2.4)
  Assert.Equal(expected, read "floor(2.4)")

[<Fact(DisplayName="Expressions - eval round")>]
let eval_round () = 
  let expected = 2.0
  Assert.Equal(expected, Expression.eval (fun _->0.0) (read "round(2.4)"))

[<Fact(DisplayName="Expressions - eval ceiling")>]
let eval_ceiling () = 
  let expected = 3.0
  Assert.Equal(expected, Expression.eval (fun _->0.0) (read "ceiling(2.4)"))

[<Fact(DisplayName="Expressions - eval floor")>]
let eval_floor () = 
  let expected = 2.0
  Assert.Equal(expected, Expression.eval (fun _->0.0) (read "floor(1+1.4)"))

[<Fact(DisplayName="Expressions - eval conditional")>]
let eval_conditional () = 
  let expected = 1.0 in
    Assert.Equal(expected, Expression.eval (fun _->0.0) (read "if 2+2=5 then 3.0 else 1.0"))
