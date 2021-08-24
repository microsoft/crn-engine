// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.GecreactionTest

open Microsoft.Research.GEC.Gecreaction

open Xunit
open FsUnit.Xunit
open System.Diagnostics


[<Fact(DisplayName="GEC - Reaction Parser - Transport")>]
let ``TransportParserTest``() = 
    let transportInReaction = "m3OC6HSL->{1.0}c[m3OC6HSL]"
    let transportOutReaction = "c[m3OC12HSL] -> {1.01}m3OC12HSL"
    let from_string (s:string) = Parser.from_string parseReaction s
    
    let tir = from_string transportInReaction
    let tirProps = Gecreaction.isTransport tir
    if tirProps.IsSome then
        let (reactant,product,rate,compartment,direction) = tirProps.Value
        Assert.Equal(reactant.Head,"m3OC6HSL")
        Assert.Equal(reactant.Length,1)
        Assert.Equal(compartment,"c")
        Assert.Equal(rate,1.0)
        Assert.Equal(direction,Ast.direction.In)
        Assert.Equal(product.Length,1)
        Assert.Equal(product.Head,"m3OC6HSL")
    else 
        failwith("None encountered. Error.")
    let tor = from_string transportOutReaction
    let torProps = Gecreaction.isTransport tor
    if torProps.IsSome then
        let (reactant,product,rate,compartment,direction) = torProps.Value
        Assert.Equal(reactant.Head,"m3OC12HSL")
        Assert.Equal(reactant.Length,1)
        Assert.Equal(compartment,"c")
        Assert.Equal(rate,1.01)
        Assert.Equal(direction,Ast.direction.Out)
        Assert.Equal(product.Length,1)
        Assert.Equal(product.Head,"m3OC12HSL")
    else 
        failwith("None encountered. Error.")
        Assert.True(false)

    Debug.WriteLine("END of Test")


[<Fact(DisplayName="GEC - Reaction Parser - Normal")>]
let ``NormalParserTest``() = 
    let norNoReactants = "luxI ~ -> {1.0}m3OC6HSL" 
    let from_string (s:string) = Parser.from_string parseReaction s
    let nnr = from_string norNoReactants
    let nnrProps = Gecreaction.isNormal nnr
    if nnrProps.IsSome then
        let (catalysts,reactants,products,rate) = nnrProps.Value
        Assert.Equal(catalysts.Length,1)
        Assert.Equal(catalysts.Head.Head,"luxI")
        Assert.True(reactants.IsEmpty)
        Assert.Equal(rate,1.0)
        Assert.Equal(products.Head.Head,"m3OC6HSL")
        Assert.Equal(products.Length,1)
    else 
        failwith("None encountered. Error.")
        Assert.True(true)

    let nor1Nocat = "lasR + m3OC12HSL->{1.0}lasR::m3OC12HSL"
    let nor1nc = from_string nor1Nocat
    let nor1ncProps = Gecreaction.isNormal nor1nc
    if nor1ncProps.IsSome then
        let (catalysts,reactants,products,rate) = nor1ncProps.Value
        Assert.True(catalysts.IsEmpty)
        Assert.Equal(reactants.Length,2)
        Assert.Equal(reactants.Head.Head,"lasR")
        Assert.Equal(rate,1.0)
        Assert.Equal(products.Head.Item(0),"lasR")
        Assert.Equal(products.Head.Item(1),"m3OC12HSL")
        Assert.Equal(products.Length,1)
    else 
        failwith("None encountered. Error.")
        Assert.True(true)
    
    let nor2Nocat = "lasR::m3OC12HSL->{1.0}lasR+m3OC12HSL"
    let nor2nc = from_string nor2Nocat
    let nor2ncProps = Gecreaction.isNormal nor2nc
    if nor2ncProps.IsSome then
        let (catalysts,reactants,products,rate) = nor2ncProps.Value
        Assert.True(catalysts.IsEmpty)
        Assert.Equal(reactants.Length,1)
        Assert.Equal(reactants.Head.Length,2)
        Assert.Equal(reactants.Head.Item(0),"lasR")
        Assert.Equal(reactants.Head.Item(1),"m3OC12HSL")
        Assert.Equal(rate,1.0)
        Assert.Equal(products.Item(0).Item(0),"lasR")
        Assert.Equal(products.Item(1).Item(0),"m3OC12HSL")
        Assert.Equal(products.Length,2)
    else 
        failwith("None encountered. Error.")
        Assert.True(true)

    let nor1 = "a::b+b::c+dc+e::f ~ i::j->{0.9}t::v+y::u"
    let n1 = from_string nor1
    let n1Props = Gecreaction.isNormal n1
    if n1Props.IsSome then
        let (catalysts,reactants,products,rate) = n1Props.Value
        Assert.Equal(catalysts.Length,4)
        Assert.Equal(catalysts.Item(0).Item(0),"a")
        Assert.Equal(catalysts.Item(0).Item(1),"b")
        Assert.Equal(catalysts.Item(1).Item(0),"b")
        Assert.Equal(catalysts.Item(1).Item(1),"c")
        Assert.Equal(catalysts.Item(2).Item(0),"dc")
        Assert.Equal(catalysts.Item(3).Item(0),"e")
        Assert.Equal(catalysts.Item(3).Item(1),"f")

        Assert.Equal(reactants.Length,1)
        Assert.Equal(reactants.Head.Length,2)
        Assert.Equal(reactants.Head.Item(0),"i")
        Assert.Equal(reactants.Head.Item(1),"j")
        Assert.Equal(rate,0.9)
        Assert.Equal(products.Length,2)
        Assert.Equal(products.Item(0).Item(0),"t")
        Assert.Equal(products.Item(0).Item(1),"v")
        Assert.Equal(products.Item(1).Item(0),"y")
        Assert.Equal(products.Item(1).Item(1),"u")
    else 
        failwith("None encountered. Error.")
        Assert.True(true)

    Debug.WriteLine("END of Test")



