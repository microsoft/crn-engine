// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.Filzbach

open Parameters
open Lib
open Filzbach

//The module contains members to be used from JavaScript only (after translating the module with WebSharper). Use the methods from Filzbach module if you use Filzbach from F#
//The methods in this module do not use complex parameters types
[<JavaScript>]
module FilzbachJS =
    let parameterEx name parameterRange lowerBound upperBound initialValue =
        let t =
            match parameterRange with
            |   0   ->  Real
            |   1   ->  Log
            |   _   ->  failwith "invalid parameter range identifier. use 0 for Real, 1 for Log"
        Microsoft.Research.Filzbach.Filzbach.parameter name t lowerBound upperBound initialValue
    
    let parameter name parameterRange lowerBound upperBound =
        parameterEx name parameterRange lowerBound upperBound None None

    let burninInit logLikelihood parameters =
        let parameters, space = parameters |> Filzbach.paramsCheck |> Filzbach.paramsSetup
        let p0, rng = Filzbach.paramsInit parameters (LCGRng 1u)
        Filzbach.burninInit (fun ps -> logLikelihood ps Parameters.All) parameters p0 space rng

    let run logLikelihood parameters dependencies (options:RunOptions) = // burninLen samplingLen=
        FilzbachMultiple.run logLikelihood parameters dependencies options