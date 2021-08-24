// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.InferenceGraph

type ParameterMapping = Fixed | Gaussian | TruncatedGaussian
type EdgeProperties = string * ParameterMapping

type Edge =  EdgeProperties list

type To = string

type From =  string * Edge 

type graph = (From list * To list) list

type t = { name : string; graph : graph }


val execute_dag: List<Crn.t>  -> t -> (Map<string,Crn.t> * Map<string,Inference.mcmc_result>)

val parse : Parser.t<t>
