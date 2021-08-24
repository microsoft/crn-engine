// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Api

let (<->) rev fwd = Some (Rate.MassAction rev), Rate.MassAction fwd
let (!->) fwd = None, Rate.MassAction fwd
let (<~>) rev fwd = Some (Rate.Function rev), Rate.Function fwd
let (!~>) fwd = None, Rate.Function fwd


let (=>) a b = a,b
let species = Species.create 
let float = Expression.Float 
let rate = Expression.Key
let plot s = Expression.Key (Key.Species s)
let plots l = List.map plot l