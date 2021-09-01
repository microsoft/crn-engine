// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

open Api
open Fable
open Giraffe
open Giraffe.Serialization
open Giraffe.Serialization.Json
open Microsoft.Extensions.DependencyInjection
open Saturn
open System.IO
open Newtonsoft.Json
open Fable.Remoting.Server
open Fable.Remoting.Giraffe

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let browserRouter = router {
    get "/" (htmlFile (Path.Combine(clientPath, "/index.html"))) }

let mainRouter = router {
    forward "/api" apiRouter
    forward "" browserRouter }

let config (services:IServiceCollection) =
    // Configure JsonSerializer to use Fable.JsonConverter
    let fableJsonSettings = JsonSerializerSettings()
    fableJsonSettings.Converters.Add(Fable.JsonConverter())
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(fableJsonSettings))

let app = application {
    use_router mainRouter
    url ("http://0.0.0.0:" + port.ToString() + "/")
    memory_cache 
    use_static clientPath
    service_config config
    use_gzip }

run app
