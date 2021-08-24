// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineServerLib.Jobs

open Microsoft.Research.CRNEngineServerLib.Serialisation
open Microsoft.Research.CRNEngineCloudLib.AzureJobsManagement
open Messages

let processGetJobsRequest webSocket allFiles =
    let sendObject = sendObject webSocket
    let jobs = getJobs false true allFiles |> List.toArray
    sendObject { mtype = "jobs"; jobs = jobs }
