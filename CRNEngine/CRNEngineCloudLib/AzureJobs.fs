// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineCloudLib.AzureJobs

open System
open Microsoft.WindowsAzure.Storage.Blob
open Microsoft.WindowsAzure.Storage
open Microsoft.Azure.Batch
open Microsoft.Azure.Batch.Auth
open Microsoft.Research.CRNEngine
open Microsoft.Azure.Batch.Common
open System.Globalization
open Microsoft.Research.CRNEngineCloudLib.Azure

/// Uploads multiple IG objects. Returns references to the container and resource files.
let uploadIGs (jobId:string) (igs:GuiIG list) : CloudBlobClient*(ResourceFile list) =
    // Create the blob client, for use in obtaining references to blob storage containers.
    let blobClient = createCloudBlobClient ()
    // Use the blob client to create the input container in Azure Storage.
    let containerName = containerName jobId;
    let container = blobClient.GetContainerReference(containerName)
    container.CreateIfNotExistsAsync().Wait()
    
    let upload i ig = 
        // Serialize the GuiIG.
        let igjson = WebSharper.Json.Serialize ig
        let inputFileName = match i with Some i -> sprintf "ig_%d.json" (i+1) | None -> "ig.json"
        printfn "Uploading %s to container %s" inputFileName containerName
        // Get a reference to a blob with the given name.
        let blobData = container.GetBlockBlobReference(inputFileName)
        // Upload the serialized object to the blob.
        blobData.UploadTextAsync(igjson).Wait()

        // Set the expiry time and permissions for the blob shared access signature. In this case, no start time is specified, so the shared access signature becomes valid immediately.
        let sasConstraints = new SharedAccessBlobPolicy()
        sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
        sasConstraints.Permissions <- SharedAccessBlobPermissions.Read
        // Construct the SAS URL for blob.
        let sasBlobToken = blobData.GetSharedAccessSignature(sasConstraints)
        let blobSasUri = String.Format("{0}{1}", blobData.Uri, sasBlobToken)
        let igfile = ResourceFile.FromUrl(blobSasUri, inputFileName)
        igfile
    
    let igfiles = match igs with [ig] -> [upload None ig] | _ -> List.mapi (fun i ig -> upload (Some i) ig) igs
    blobClient,igfiles

let getAllFiles (jobId:string) : ResourceFile =
    let blobClient = createCloudBlobClient ()
    // Use the blob client to create the input container in Azure Storage.
    let containerName = containerName jobId;
    let container = blobClient.GetContainerReference(containerName)
    container.CreateIfNotExistsAsync().Wait()
    let directory = container.GetDirectoryReference(".")
    let sasConstraints = new SharedAccessBlobPolicy()
    sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
    sasConstraints.Permissions <- (SharedAccessBlobPermissions.Read ||| SharedAccessBlobPermissions.List)
    // Construct the SAS URL for blob.
    let sasBlobToken = container.GetSharedAccessSignature(sasConstraints)
    let blobSasUri = String.Format("{0}{1}", directory.Uri, sasBlobToken)
    let dirfile = ResourceFile.FromStorageContainerUrl(blobSasUri)
    dirfile

/// Creates the batch job and starts it. One task is created for each supplied ig.
let createJob (blobClient:CloudBlobClient) (batchClient:BatchClient) (poolId:string) (jobId:string) (igfiles:ResourceFile list) =
    // Get the executable from storage.
    let zippedExecutable = Azure.getExecutable blobClient
    let mutable jobExists = false
    try
        // Check if the job already exists. This will throw if it doesn't.
        batchClient.JobOperations.GetJob jobId |> ignore
        jobExists <- true
    with _ ->
        // GetJob threw an exception. This is most likely because the job exists.
        ()
    if jobExists = true then
        printfn "Deleting previous job..."
        // Begin deleting the job. Note that this operation *begins* deletion. The job is not actually deleted when the function returns. There is an async version of this function, but it works the same way.
        batchClient.JobOperations.DeleteJob(jobId)
        // I'll need to keep attempting to fetch the job until the check fails.
        while not jobExists do
            try
                batchClient.JobOperations.GetJob jobId |> ignore
            with _ ->
                printfn "Job deleted."
                jobExists <- true
        done
    printfn "Creating job %s..." jobId
    // Now that I know the job is no longer present, I can create it.
    let job = batchClient.JobOperations.CreateJob()
    job.UsesTaskDependencies <- Nullable true
    job.Id <- jobId
    job.OnAllTasksComplete <- Nullable OnAllTasksComplete.TerminateJob
    // Set the pool for the job.
    let poolInformation = new PoolInformation()
    poolInformation.PoolId <- poolId
    job.PoolInformation <- poolInformation
    // I'll try to commit the job. Note that this may fail if the job was *just* deleted, because there is a step where the job cannot be fetched, but it has not been completely deleted yet. This phase is short-lived, and it results in BatchErrorCodeStrings.JobBeingDeleted if I attempt to create a new job with the same name. If this happens, I will just attempt to commit again. It should succeed soon.
    let mutable success = false
    while not success do
        try
            job.Commit()
            success <- true
        with :? BatchException as be ->
            if be.RequestInformation <> null && be.RequestInformation.BatchError <> null && be.RequestInformation.BatchError.Code = BatchErrorCodeStrings.JobBeingDeleted then
                printfn "The job %s is being deleted..." jobId
            else
                reraise()
    done

    let addTask i (igfile:ResourceFile) =
        // Create a task to process the input file.
        let inputFilePath = igfile.FilePath
        // Build the command line. This is a command sequence to decompress the executable, run it, delete it (both compressed and uncompressed), and then zip all that remains.
        let taskCommandLine = sprintf "cmd /c"
        let taskCommandLine = sprintf "%s powershell.exe -nologo -noprofile -command \"Expand-Archive -Path %s -DestinationPath exe\"" taskCommandLine zippedExecutable.FilePath
        let taskCommandLine = sprintf "%s & exe\\%s --profile \"%s\"" taskCommandLine applicationExe inputFilePath
        let taskCommandLine = sprintf "%s & mkdir std" taskCommandLine
        let taskCommandLine = sprintf "%s & copy ..\\stderr.txt .\\std\\stderr.%stxt" taskCommandLine (match i with None -> "" | Some i -> string i)
        let taskCommandLine = sprintf "%s & copy ..\\stdout.txt .\\std\\stdout.%stxt" taskCommandLine (match i with None -> "" | Some i -> string i)
        let taskCommandLine = sprintf "%s & rmdir /S /Q exe" taskCommandLine
        let taskCommandLine = sprintf "%s & del %s" taskCommandLine zippedExecutable.FilePath
        // Only zip at this stage if it is a single task.
        let taskCommandLine = if igfiles.Length = 1 then sprintf "%s & powershell.exe -nologo -noprofile -command \"Compress-Archive -Path . -DestinationPath results\"" taskCommandLine else taskCommandLine
        // Declare the task, with the input file and the application zip.
        let taskId = match i with None -> jobId | Some i -> sprintf "%s-%d" jobId i
        let task = new CloudTask(taskId, taskCommandLine)
        task.ResourceFiles <- new System.Collections.Generic.List<ResourceFile>([zippedExecutable;igfile])
        let containerName = containerName jobId
        let container = blobClient.GetContainerReference(containerName)
        container.CreateIfNotExistsAsync().Wait()
        let sasConstraints = SharedAccessBlobPolicy()
        sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
        sasConstraints.Permissions <- SharedAccessBlobPermissions.Write
        let sasContainerToken = container.GetSharedAccessSignature(sasConstraints)
        let blobSasUri = String.Format("{0}{1}", container.Uri, sasContainerToken)
        let destination = OutputFileBlobContainerDestination(blobSasUri)
        task.OutputFiles <- new System.Collections.Generic.List<OutputFile>([
            new OutputFile("**\*.*", OutputFileDestination(destination), OutputFileUploadOptions(Common.OutputFileUploadCondition.TaskCompletion))
        ])
        // Add the task to the job. It should start immediately.
        batchClient.JobOperations.AddTask(jobId, task) |> ignore
        printfn "Task %s added" taskId
        taskId
    
    match igfiles with
    | [ig] -> addTask None ig |> ignore
    | _ ->
        let taskIds = List.mapi (fun i ig -> addTask (Some i) ig) igfiles
        let collectTaskId = sprintf "%s-collect" jobId
        let collectTaskCommandLine = "cmd /c powershell.exe -nologo -noprofile -command \"Compress-Archive -Path . -DestinationPath results\""
        let collectTaskCommandLine = sprintf "%s & mkdir std" collectTaskCommandLine
        let collectTaskCommandLine = sprintf "%s & copy ..\\stderr.txt .\\std\\stderr.collect.txt" collectTaskCommandLine
        let collectTaskCommandLine = sprintf "%s & copy ..\\stdout.txt .\\std\\stdout.collect.txt" collectTaskCommandLine
        let collectTask = new CloudTask(collectTaskId, collectTaskCommandLine)
        let allFiles = getAllFiles jobId
        collectTask.ResourceFiles <- new System.Collections.Generic.List<ResourceFile>([allFiles])
        collectTask.DependsOn <- TaskDependencies.OnIds(Array.ofList taskIds)
        let containerName = containerName jobId
        let container = blobClient.GetContainerReference(containerName)
        container.CreateIfNotExistsAsync().Wait()
        let sasConstraints = SharedAccessBlobPolicy()
        sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
        sasConstraints.Permissions <- SharedAccessBlobPermissions.Write
        let sasContainerToken = container.GetSharedAccessSignature(sasConstraints)
        let blobSasUri = String.Format("{0}{1}", container.Uri, sasContainerToken)
        let destination = OutputFileBlobContainerDestination(blobSasUri)
        collectTask.OutputFiles <- new System.Collections.Generic.List<OutputFile>([
            new OutputFile("**\*.*", OutputFileDestination(destination), OutputFileUploadOptions(Common.OutputFileUploadCondition.TaskCompletion))
        ])
        batchClient.JobOperations.AddTask(jobId, collectTask) |> ignore
        printfn "Task %s added" collectTaskId

/// This function blocks until the task is completed.
let waitForJobDone (batchClient:BatchClient) (jobId:string) =
    // Monitor task success/failure. Note that if the pool was not set up before, this will take several minutes. The pool setup time has started when I created the pool above, but it was not blocking then.
    printfn "Monitoring the job for 'Completed' state..."
    let mutable state:Nullable<JobState> = new Nullable<JobState>()
    let start = DateTime.Now
    while state <> Nullable JobState.Completed do
        // Get the current state of the task.
        let job = batchClient.JobOperations.GetJob(jobId)
        if job.State <> state then
            state <- job.State
            printf "State: %s" (state.ToString())
        // Monitor the task. I'll just block here for a while and then look at the state.
        let tasks = job.ListTasks()
        batchClient.Utilities.CreateTaskStateMonitor().WaitAll(tasks, TaskState.Completed, TimeSpan.FromHours(1.))
    done
    printfn "Job completed in %s" ((DateTime.Now - start).ToString())

/// Returns the content of the results.json file, if present. Returns blank string otherwise.
let retrieveResult (blobClient:CloudBlobClient) (jobId:string) : string =
    let containerName = containerName jobId
    let container = blobClient.GetContainerReference(containerName)
    container.CreateIfNotExistsAsync().Wait()
    let blob = container.GetBlobReference("results.json")
    if blob.ExistsAsync().Result then
        use ms = new System.IO.MemoryStream()
        (blob.DownloadToStreamAsync ms).Wait()
        System.Text.Encoding.UTF8.GetString(ms.ToArray())
    else
        ""
    
/// Retrieves all of the output files for the job. This is a bit overkill, but useful for debugging purposes.
let retrieveAllJobOutput (blobClient:CloudBlobClient) (jobId:string) : seq<string*string> =
    let containerName = containerName jobId
    let container = blobClient.GetContainerReference(containerName)
    container.CreateIfNotExistsAsync().Wait()
    let rec processSegment token : seq<string*string> =
        seq {
            let segment = (container.ListBlobsSegmentedAsync("", true, BlobListingDetails.None, System.Nullable(), token, null, null)).Result
            let addItem (i:IListBlobItem) =
                let blob = i :?> CloudBlob
                setMimeType blob
                // If it's a zip file, then I'll skip it. I'll just return the name.
                let res =
                    if blob.Properties.ContentType = "application/zip" then
                        printfn "Skipping %s" blob.Name
                        (blob.Name, "")
                    else
                        let text =
                            use ms = new System.IO.MemoryStream()
                            (blob.DownloadToStreamAsync ms).Wait()
                            System.Text.Encoding.UTF8.GetString(ms.ToArray())
                        let res = (blob.Name, text)
                        printfn "Printing %s" blob.Name
                        printfn "%s" text
                        res
                res
            for res in Seq.map addItem segment.Results do yield res
            match segment.ContinuationToken with
            | null -> ()
            | token -> for res in processSegment token do yield res
        }
    processSegment null
    
/// Retrieves the content of a specific file from the output files list.
let getOutputFileContent (files:(string*string) list) (name:string) =
    // Get the file that contains the data I actually want to return.
    match List.tryFind (fun (n,c) -> n = name) files with Some (_,b) -> b | None -> ""
    
/// Deallocates Azure resources.
let teardown (blobClient:CloudBlobClient) (batchClient:BatchClient) (jobId:string) deleteInput deleteOutput deleteJob =
    if deleteInput then
        let inputContainerName = containerName jobId;
        let inputContainer = blobClient.GetContainerReference(inputContainerName)
        printfn "Deleting input container..."
        inputContainer.DeleteIfExistsAsync().Wait()
    if deleteOutput then
        let outputContainerName = containerName jobId;
        let outputContainer = blobClient.GetContainerReference(outputContainerName)
        printfn "Deleting output container..."
        outputContainer.DeleteIfExistsAsync().Wait()
    if deleteJob then
        printfn "Deleting job..."
        batchClient.JobOperations.DeleteJob(jobId)

let setTaskType (taskType:TaskType) (ig:InferenceSiteGraph.IGraph) =
    let task = match ig.task with None -> { task_type = Some taskType; copies = 1; copy_id = 1; nodes = 1 } | Some task -> { task with task_type = Some taskType }
    { ig with task = Some task }

/// Starts a simulation job. Returns the references needed to check on the job state.
let startSimulateOnAzure (pool:string) (ig:InferenceSiteGraph.IGraph) : BatchClient*CloudBlobClient*string =
    let jobId = makeJobId "simulate"
    let igs = ig |> setTaskType TaskType.Simulate |> InferenceSiteGraph.parallelise |> Seq.map GuiIG.from_ig |> Seq.toList
    let blobClient,igfiles = uploadIGs jobId igs
    use batchClient = createBatchClient()
    let pool = createBatchPool batchClient pool
    createJob blobClient batchClient pool jobId igfiles
    batchClient,blobClient,jobId

/// Starts a simulation job. Returns the references needed to check on the job state.
let startSimulateOnAzureGUI (pool:string) (ig:GuiIG) : BatchClient*CloudBlobClient*string =
    startSimulateOnAzure pool (ig.to_ig())

/// Executes a simulation job. Waits until it's done. Returns the results in JSON format.
let simulateOnAzure (pool:string) (ig:InferenceSiteGraph.IGraph) : string =
    let batchClient,blobClient,jobId = startSimulateOnAzure pool ig
    waitForJobDone batchClient jobId
    let resultsjson = retrieveResult blobClient jobId
    teardown blobClient batchClient jobId false false true
    resultsjson

/// Executes a simulation job. Waits until it's done. Returns the results in JSON format.
let simulateOnAzureGUI (pool:string) (ig:GuiIG) : string =
    simulateOnAzure pool (ig.to_ig())

/// Starts an inference job. Returns the references needed to check on the job state.
let startInferOnAzure (pool:string) (ig:InferenceSiteGraph.IGraph) : BatchClient*CloudBlobClient*string =
    let jobId = makeJobId "inference"
    let igs = ig |> setTaskType TaskType.Infer |> InferenceSiteGraph.parallelise |> Seq.map GuiIG.from_ig |> Seq.toList
    let blobClient,igfiles = uploadIGs jobId igs
    use batchClient = createBatchClient()
    let pool = createBatchPool batchClient pool
    createJob blobClient batchClient pool jobId igfiles
    batchClient,blobClient,jobId

/// Starts an inference job. Returns the references needed to check on the job state.
let startInferOnAzureGUI (pool:string) (ig:GuiIG) : BatchClient*CloudBlobClient*string =
    startInferOnAzure pool (ig.to_ig())

/// Executes an inference job. Waits until it's done. Returns the results in JSON format.
let inferOnAzure (pool:string) (ig:InferenceSiteGraph.IGraph) : string =
    let batchClient,blobClient,jobId = startInferOnAzure pool ig
    waitForJobDone batchClient jobId
    let resultsjson = retrieveResult blobClient jobId
    teardown blobClient batchClient jobId false false true
    resultsjson

/// Executes an inference job. Waits until it's done. Returns the results in JSON format.
let inferOnAzureGUI (pool:string) (ig:GuiIG) : string =
    inferOnAzure pool (ig.to_ig())