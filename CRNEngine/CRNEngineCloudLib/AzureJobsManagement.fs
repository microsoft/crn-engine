// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineCloudLib.AzureJobsManagement

open System
open System.IO
open System.IO.Compression
open System.Globalization
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Blob
open Microsoft.WindowsAzure.Storage
open Microsoft.Azure.Batch
open Microsoft.Azure.Batch.Common
open Microsoft.Azure.Batch.Auth
open Microsoft.Research.CRNEngineCloudLib.Azure
open Microsoft.Research.CRNEngineCloudLib.DataStructures

let getJobs quiet zipFile allFiles : List<JobDescriptor> =
    let storage = createCloudBlobClient()
    use client = createBatchClient()

    // Set the expiry time and permissions for the blob shared access signature. In this case, no start time is specified, so the shared access signature becomes valid immediately.
    let sasConstraints = new SharedAccessBlobPolicy()
    sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
    sasConstraints.Permissions <- SharedAccessBlobPermissions.Read
    
    let rec processSegment token =
        seq {
            let segment = (storage.ListContainersSegmentedAsync(jobPrefix, token)).Result
            let addItem (container:CloudBlobContainer) =
                let jobId = container.Name
                if not quiet then printfn "Retrieving data for job %s" jobId
                let state =
                    try
                        let job = client.JobOperations.GetJob(jobId)
                        match job.State.Value with
                        | Common.JobState.Completed
                        | Common.JobState.Deleting
                        | Common.JobState.Terminating -> Completed
                        | _ -> Active
                    with _ -> NoJob
                let (verb, start) = unpackJobId jobId

                let toJobFile name =
                    // Construct the SAS URL for blob. This will allow the user to download it without further authentication.
                    let blobData = container.GetBlockBlobReference(name)
                    if blobData.ExistsAsync().Result then
                        let sasBlobToken = blobData.GetSharedAccessSignature(sasConstraints)
                        let blobSasUri = String.Format("{0}{1}", blobData.Uri, sasBlobToken)
                        if not quiet then printfn "Retrieved uri for %s" name
                        Some { name = name ; uri = blobSasUri }
                    else None

                let files =
                    if allFiles then
                        let rec processSegment token : seq<string> =
                            seq {
                                let segment = (container.ListBlobsSegmentedAsync("", true, BlobListingDetails.None, System.Nullable(), token, null, null)).Result
                                let addItem (i:IListBlobItem) =
                                    let blob = i :?> CloudBlob
                                    setMimeType blob
                                    blob.Name
                                for res in Seq.map addItem segment.Results do yield res
                                match segment.ContinuationToken with
                                | null -> ()
                                | token -> for res in processSegment token do yield res
                            }
                        let fileNames = processSegment null
                        Seq.map toJobFile fileNames |> Seq.map (fun n -> n.Value) |> Seq.toArray
                    else [||]
                
                let zipFile = if zipFile then toJobFile "results.zip" else None

                if not quiet then printfn "Retrieved data for job %s (%O)" jobId state
                { id = jobId
                ; state = state
                ; verb = verb
                ; start = start
                ; zipFile = zipFile
                ; files = files
                }
            for res in Seq.map addItem segment.Results do yield res
            match segment.ContinuationToken with
            | null -> ()
            | token -> for res in processSegment token do yield res
        }
    processSegment null |> Seq.toList

let getFile quiet jobId name =
    let storage = createCloudBlobClient()
    let containerName = containerName jobId
    let container = storage.GetContainerReference(containerName)
    let blobData = container.GetBlockBlobReference(name)
    if blobData.ExistsAsync().Result then
       if not quiet then printfn "Retrieved: %s" blobData.Name
       let stream = blobData.OpenReadAsync().Result
       Some stream
    else None

let getFiles quiet jobId =
    let storage = createCloudBlobClient()
    let containerName = containerName jobId
    let container = storage.GetContainerReference(containerName)
    
    let rec processSegment token : seq<string*string> =
        seq {
            let segment = (container.ListBlobsSegmentedAsync("", true, BlobListingDetails.None, System.Nullable(), token, null, null)).Result
            let addItem (i:IListBlobItem) =
                let blob = i :?> CloudBlob
                let text =
                    use ms = new System.IO.MemoryStream()
                    (blob.DownloadToStreamAsync ms).Wait()
                    System.Text.Encoding.UTF8.GetString(ms.ToArray())
                let res = (blob.Name, text)
                if not quiet then printfn "Retrieved: %s" blob.Name
                res
            for res in Seq.map addItem segment.Results do yield res
            match segment.ContinuationToken with
            | null -> ()
            | token -> for res in processSegment token do yield res
        }
    processSegment null
    
let stopJob jobId =
    use client = createBatchClient()
    try
        printfn "Terminating job %s..." jobId
        client.JobOperations.TerminateJob(jobId)
        printfn "Job terminated"
    with exc ->
        printfn "Could not terminate job: %s" (exc.ToString())

let deleteJob jobId =
    use client = createBatchClient()
    try
        printfn "Deleting job %s..." jobId
        client.JobOperations.DeleteJob(jobId)
        printfn "Waiting for deletion..."
    with
        | :? BatchException as be -> if be.RequestInformation.BatchError.Code <> BatchErrorCodeStrings.JobBeingDeleted && be.RequestInformation.BatchError.Code <> BatchErrorCodeStrings.JobNotFound then printfn "Could not delete jon: %s" (be.ToString())
        | exc -> printf "Could not delete job: %s" (exc.ToString())
    let mutable deleted = false
    while not deleted do
        try
            client.JobOperations.GetJob(jobId) |> ignore
        with _ ->
            deleted <- true
    done
    let storage = createCloudBlobClient()
    let inputContainerName = containerName jobId;
    let inputContainer = storage.GetContainerReference(inputContainerName)
    printfn "Deleting input container..."
    inputContainer.DeleteIfExistsAsync().Wait()
    let outputContainerName = containerName jobId;
    let outputContainer = storage.GetContainerReference(outputContainerName)
    printfn "Deleting output container..."
    outputContainer.DeleteIfExistsAsync().Wait()
    printfn "Job deleted"
