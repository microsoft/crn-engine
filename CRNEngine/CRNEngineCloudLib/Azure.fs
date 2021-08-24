// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngineCloudLib.Azure

open System
open System.Globalization
open System.Text.RegularExpressions
open System.Xml.Linq
open Microsoft.WindowsAzure.Storage
open Microsoft.WindowsAzure.Storage.Blob
open Microsoft.Azure.Batch
open Microsoft.Azure.Batch.Auth
open Microsoft.Azure.Batch.Common

// Azure credentials are read from a file named azure.xml in the current directory. If the file does not exist, then Azure execution is disabled.

let private azureConfigFile = "azure.xml"

let enabled =
    let path = System.IO.Path.GetFullPath(azureConfigFile)
    let ret = System.IO.File.Exists(path)
    if ret then
        printfn "%s found, run-on-Azure enabled" path
    else
        printfn "%s not found, run-on-Azure disabled" path
    ret

(* Sample azure.xml:

<azureconfig>
    <batchAccountName>batchaccountname</batchAccountName>
    <batchAccountKey>batchaccountkey</batchAccountKey>
    <batchAccountURL>https://batchaccounturl.batch.azure.com</batchAccountURL>
    <storageAccountName>storageaccountname</storageAccountName>
    <storageAccountKey>storageaccountkey</storageAccountKey>
    <batchPool name="ready" size="STANDARD_A1_v2" standbyNodes="1"/>
    <batchPool name="fast" size="Standard_A8m_v2" standbyNodes="0"/>
    <startTask>(some command line to be executed when a node is created)</startTask>
</azureconfig>

batchPool nodes are optional and can be used to create pools.s

*)

let private azureXml = if enabled then System.Xml.Linq.XDocument.Load(azureConfigFile) |> Some else None

let getXmlValue name defaultValue = if azureXml.IsNone then
                                        defaultValue
                                    else
                                        let n = azureXml.Value.Root.Element(XName.Get name)
                                        if n = null then
                                            defaultValue
                                        else
                                            n.Value

let batchAccountName = getXmlValue "batchAccountName" ""
let internal batchAccountKey = getXmlValue "batchAccountKey" ""
let internal batchAccountURL = getXmlValue "batchAccountURL" ""
let internal storageAccountName = getXmlValue "storageAccountName" ""
let internal storageAccountKey = getXmlValue "storageAccountKey" ""
let internal startTask = getXmlValue "startTask" ""

type PoolDef = {
    name: string;
    size: string;
    standbyNodes: int;
}

let private pools =
    seq {
        if enabled then
            let declaredPools = azureXml.Value.Root.Elements(XName.Get "batchPool") |> Seq.toList
            match declaredPools with 
            | [] -> { name = "crnbatchpool"; size = "STANDARD_A1_v2"; standbyNodes = 1 }
            | _ ->
                let mapper (e:XElement) : PoolDef = {
                    name = e.Attribute(XName.Get "name").Value
                    size = e.Attribute(XName.Get "size").Value
                    standbyNodes =
                        let n = e.Attribute(XName.Get "standbyNodes")
                        if n = null then 1 else Int32.Parse n.Value
                }
                yield! List.map mapper declaredPools
    }

let private getPoolDef (preferred:string) =
    match preferred with
    | "" -> Seq.head pools
    | id ->
        match Seq.tryFind (fun p -> p.name = id) pools with
        | None ->
            let defaultPool = Seq.head pools
            printfn "WARNING: no pool %s declared, using pool %s instead" id defaultPool.name
            defaultPool
        | Some pool -> pool

let internal programsContainer = "programs"
let internal applicationId = "clicrn"
let internal applicationExe = "CliCRN.exe"
let internal jobPrefix = "task"
let internal makeJobId verb = String.Format(CultureInfo.InvariantCulture, "{0}-{1}-{2:yyMMddHHmmss}", jobPrefix, verb, DateTime.UtcNow)
let internal unpackJobId (jobId:string) =
    let tokens = jobId.Split('-')
    if tokens.[0] <> jobPrefix then failwith ("invalid job id "+jobId)
    let verb = tokens.[1]
    let timestamp = DateTime.ParseExact(tokens.[2], "yyMMddHHmmss", CultureInfo.InvariantCulture)
    (verb,timestamp)
let internal containerName jobId = jobId

let internal createCloudBlobClient () =
    // Construct the Storage account connection string.
    let storageConnectionString = sprintf "DefaultEndpointsProtocol=https;AccountName=%s;AccountKey=%s" storageAccountName storageAccountKey
    // Retrieve the storage account.
    let storageAccount = CloudStorageAccount.Parse(storageConnectionString)
    // Create the blob client.
    let blobClient = storageAccount.CreateCloudBlobClient()
    blobClient

let internal createBatchClient () =
    let cred = new BatchSharedKeyCredentials(batchAccountURL, batchAccountName, batchAccountKey)
    BatchClient.Open(cred)

/// Creates the batch pool, if it doesn't exist already.
let internal createBatchPool (batchClient:BatchClient) (preferredPool:string) =
    let remotepools = batchClient.PoolOperations.ListPools()
    let existing = Seq.map (fun (pool:CloudPool) -> pool.DisplayName) remotepools |> Set.ofSeq
    if Set.contains preferredPool existing then
        printfn "Pool [%s] exists." preferredPool
        preferredPool
    else
        let poolDef = getPoolDef preferredPool
        printfn "Creating pool [%s]..." poolDef.name
        // Configure a Windows Server image, VM configuration, Batch pool.
        let imageReference = new ImageReference("WindowsServer", "MicrosoftWindowsServer", "2019-datacenter-smalldisk", "latest")
        let vmConfiguration = new VirtualMachineConfiguration(imageReference, "batch.node.windows amd64")
        // Create the pool with the given configuration.
        let pool = batchClient.PoolOperations.CreatePool(poolDef.name, poolDef.size, vmConfiguration)
        
        if startTask.Trim() <> "" then
            printfn "Adding start task: %s" startTask
            let startTask = new StartTask(startTask)
            startTask.WaitForSuccess <- Nullable false
            startTask.UserIdentity <- new UserIdentity(new AutoUserSpecification(Nullable AutoUserScope.Pool, Nullable ElevationLevel.Admin))
            pool.StartTask <- startTask
        
        try
            // Attempt to commit the creation of the pool. This will throw if the pool already exists.
             pool.Commit()
        with :? BatchException as be ->
            // Accept the specific error code PoolExists as that is expected if the pool already exists.
            if be.RequestInformation.BatchError.Code = BatchErrorCodeStrings.PoolExists then
                printfn "The pool %s already existed when we tried to create it." poolDef.name
            else
                reraise ()

        // Formula for auto scaling: I'm targeting the max between the average task count over the last minute, and the average task count over the last hour (minimum 1, if there was at least some activity during that hour). The objective here is to have the number of VMs ramp up quickly, but ramp down slowly. This should help avoid having to spin up VMs over and over again when running short tasks in a sequence. Note that I'm allocating a minimum of 1 node, to avoid setup time. Also, if the sample percent is zero (i.e. no data at all), then I start a VM. This is to circumvent an apparent catch-22 problem on a freshly-created pool, where stats don't seem to get gathered until at least one node has been created.
        let pool = batchClient.PoolOperations.GetPool(poolDef.name)
        let autoScaleFormula = sprintf """$perc = $PendingTasks.GetSamplePercent(TimeInterval_Hour * 1);
    $avgMinute = $perc < 50 ? 0 : avg($PendingTasks.GetSample(TimeInterval_Minute * 1, 50));
    $avgHour = $perc < 50 ? 0 : avg($PendingTasks.GetSample(TimeInterval_Hour * 1, 50));
    $avgHourCapped = $perc == 0 ? 1 : $avgHour > 0 ? max(1,$avgHour) : $avgHour;
    $targetVMs = max($avgMinute, $avgHourCapped);
    $TargetDedicatedNodes = max(%d, min($targetVMs, 3000));
    $TargetLowPriorityNodes = 0;
    $NodeDeallocationOption = taskcompletion;""" poolDef.standbyNodes
        pool.EnableAutoScale(autoScaleFormula, Nullable (TimeSpan.FromMinutes(5.0)));
        poolDef.name

/// Returns all valid pool names, including both existing pools and pools that are defined in the xml file.
let getPoolNames() =
    if enabled then
        let batchClient = createBatchClient()
        let remotepools = batchClient.PoolOperations.ListPools()
        let existing = Seq.map (fun (pool:CloudPool) -> pool.Id) remotepools |> Set.ofSeq
        let defs = Seq.map (fun (pool:PoolDef) -> pool.name) pools |> Set.ofSeq
        Set.union existing defs
    else
        Set.empty

let internal getExecutable (blobClient:CloudBlobClient) : ResourceFile =
    // Use the blob client to create the input container in Azure Storage.
    let container = blobClient.GetContainerReference(programsContainer)
    container.CreateIfNotExistsAsync().Wait()
    let expectedFileName = sprintf "clicrn.%s.zip" Microsoft.Research.CRNEngine.Lib.commit_number
    let blobData =
        // See if there's a version that matches.
        let expectedFile = container.GetBlockBlobReference(expectedFileName)
        if expectedFile.ExistsAsync().Result then
            printfn "Remote exe: %s" expectedFile.Name
            expectedFile
        else
            // Use the most recently modified version.
            let rec processSegment (current:CloudBlockBlob) token : CloudBlockBlob =
                let segment = (container.ListBlobsSegmentedAsync("", true, BlobListingDetails.None, System.Nullable(), token, null, null)).Result
                let folder (current:CloudBlockBlob) (next:CloudBlockBlob) =
                    if not (Regex.IsMatch(next.Name, "clicrn\\.([0-9]|[a-f]|[A-F])*\\.zip")) then current else
                    if not current.Properties.LastModified.HasValue then next else
                    if current.Properties.LastModified.Value > next.Properties.LastModified.Value then current else
                    next
                let current = segment.Results |> Seq.cast<CloudBlockBlob> |> Seq.fold folder current
                match segment.ContinuationToken with
                | null -> current
                | token -> processSegment current segment.ContinuationToken
            let ret = processSegment expectedFile null
            printfn "Remote exe: %s not available, using %s" expectedFile.Name ret.Name
            ret
    
    // Set the expiry time and permissions for the blob shared access signature. In this case, no start time is specified, so the shared access signature becomes valid immediately.
    let sasConstraints = new SharedAccessBlobPolicy()
    sasConstraints.SharedAccessExpiryTime <- Nullable (DateTimeOffset(DateTime.UtcNow.AddYears(1)))
    sasConstraints.Permissions <- SharedAccessBlobPermissions.Read
    // Construct the SAS URL for blob.
    let sasBlobToken = blobData.GetSharedAccessSignature(sasConstraints)
    let blobSasUri = String.Format("{0}{1}", blobData.Uri, sasBlobToken)
    let zippedExecutable = ResourceFile.FromUrl(blobSasUri, blobData.Name)
    zippedExecutable

/// Sets the mime type for the blob, unless it has been set already. The practical effect of this is that if you make a HTML link to the blob, and the user clicks on it, the browser will automatically select the right program to open the content.
let internal setMimeType (blob:CloudBlob) =
    if blob.Properties.ContentType = null || blob.Properties.ContentType = "" then
        match System.IO.Path.GetExtension(blob.Name) with
        | ".htm"
        | ".html" ->
            blob.Properties.ContentType <- "text/html"
        | ".txt" ->
            blob.Properties.ContentType <- "text/plain"
        | ".csv" ->
            blob.Properties.ContentType <- "text/csv"
        | ".tdv"
        | ".tsv" ->
            blob.Properties.ContentType <- "text/tab-separated-value"
        | ".json" ->
            blob.Properties.ContentType <- "application/json"
        | ".zip" ->
            blob.Properties.ContentType <- "application/zip"
        | _ -> ()
        printfn "Content type of %s set to %s" blob.Name blob.Properties.ContentType
        blob.SetPropertiesAsync() |> ignore