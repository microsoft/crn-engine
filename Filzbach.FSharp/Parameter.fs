// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.Filzbach

open Lib
open DataStructures

[<JavaScript>]
module Parameters = 

    type ParameterType =
        |   Real    //type 0
        |   Log   //type 1
        |   Fixed
    
    type ParameterRange = {
        pType: ParameterType
        lb: float
        ub:float
        }

    type Normal = { mean:float; stdev:float }
    type LogNormal = { mu:float; sigma:float }
    type TruncatedNormal = { mean:float; stdev:float; lb:float; ub:float; denominator:float }
    type PriorValue = 
        | NormalPrior of Normal 
        | LogNormalPrior of LogNormal 
        | TruncatedNormalPrior of TruncatedNormal
    let createNormal m s = if s > 0.0 then NormalPrior { mean=m; stdev=s } else failwith ("Cannot create Normal distribution with non-positive sigma. Value supplied: " + s.ToString())
    let createLogNormal m s = if s > 0.0 then LogNormalPrior { mu=m; sigma=s } else failwith ("Cannot create LogNormal distribution with non-positive sigma. Value supplied: " + s.ToString())
    let createTruncatedNormal mean stdev lb ub = 
        let den = Lib.normCDF mean stdev ub - Lib.normCDF mean stdev lb
        TruncatedNormalPrior { mean=mean; stdev=stdev; lb=lb; ub=ub; denominator=den}
    
    
    type AcceptanceStatistics = {
        accepted:int
        altered:int
    }

    type Parameter = {
        name:string
        range:ParameterRange
        initValue: float option
        prior:PriorValue option
        summary:bool
    }
    with
      member p.isFixed () = p.range.pType = Fixed
      member p.isReal () = p.range.pType = Real

    type ParameterSpace = AssociativeArray<ParameterRange>
    type ParameterValues = AssociativeArray<float>
    
    type NamedParameterRange = NamedObject<ParameterRange>
    type NamedValue = NamedObject<float>

    type ParameterStatistics = AcceptanceStatistics array
    
    type EvaluatedValues = {
        values: ParameterValues
        logLikelihood: float
        logPrior: float
        iteration: int
    }

    type Chain = EvaluatedValues list
    
    type InnovationGenerators = Map<int,NormalGenerator>

    type Ids = All | Subset of int list
    