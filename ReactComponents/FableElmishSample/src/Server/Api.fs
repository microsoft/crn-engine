module Api

open Giraffe
open Microsoft.AspNetCore.Http
open Saturn
open ReactIDD
open FSharp.Control.Tasks
open System.Linq
open Fable.Remoting.Giraffe
open Fable.Remoting.Server

let markershape text =
  match text with
  | "circle" -> MarkerShape.Circle
  | "cross" -> MarkerShape.Cross
  | "diamond" -> MarkerShape.Diamond
  | "triangle" -> MarkerShape.Triangle
  | _ -> MarkerShape.Box

/// This function turns a chunk of text into IPlot objects. The format assumes that each row is either a property, in which case it is in the format 'key:value', or a row of data.
let parsePlot (text:string) =
  let rows = text.Split('\r','\n') |> Array.toSeq
            |> Seq.map (fun row -> row.Trim())
            |> Seq.where (fun row -> row <> "")
  let rows = rows |> Seq.groupBy (fun row -> row.Contains(":")) |> Map.ofSeq
  let props = rows.TryFind true |> Option.defaultValue Seq.empty
           |> Seq.map (fun row -> match row.Split(':') with [|k;v|] -> (k.Trim(),v.Trim()) | _ -> failwith "invalid prop")
           |> Map.ofSeq
  let data = rows.TryFind false |> Option.defaultValue Seq.empty
          |> Seq.map (fun row -> row.Split(' ') |> Array.map float)
          |> Array.ofSeq
  let id = props.TryFind("id") |> Option.defaultValue "myPlot"
  if data.Length = 0 then Delete id else
  let plot = match props.TryFind("kind") |> Option.defaultValue "polyline" with
             | "polyline" ->
               let (x,y) = data |> Array.map (fun a -> (a.[0],a.[1])) |> Array.unzip
               { id = id
                 x = x
                 y = y
                 stroke = props.TryFind("stroke") |> Option.defaultValue "black"
                 thickness = props.TryFind("thickness") |> Option.defaultValue "2" |> int }
               |> Polyline
             | "markers" ->
               let (x,y) = data |> Array.map (fun a -> (a.[0],a.[1])) |> Array.unzip
               { id = id
                 x = x
                 y = y
                 color = props.TryFind("color") |> Option.defaultValue "black"
                 size = props.TryFind("size") |> Option.defaultValue "2" |> float
                 shape = props.TryFind("shape") |> Option.defaultValue "box" |> markershape }
               |> Markers
             | "sizedmarkers" ->
               let (x,y,s) = data |> Array.map (fun a -> (a.[0],a.[1],a.[2])) |> Array.unzip3
               { id = id
                 x = x
                 y = y
                 sizes = s
                 color = props.TryFind("color") |> Option.defaultValue "black"
                 shape = props.TryFind("shape") |> Option.defaultValue "box" |> markershape }
               |> SizedMarkers
             | "bars" ->
               let (x,y) = data |> Array.map (fun a -> (a.[0],a.[1])) |> Array.unzip
               { id = id
                 x = x
                 y = y
                 color = props.TryFind("color") |> Option.defaultValue "black"
                 barWidth = props.TryFind("barwidth") |> Option.defaultValue "1" |> float }
               |> Bars
             | "boxandwhiskers" ->
               let (x,q) = data |> Array.map (fun a -> (a.[0],(a.[1],a.[2],a.[3],a.[4],a.[5]))) |> Array.unzip
               let (m,s,n) = q |> Array.map (fun (m, l68, u68, l95, u95) -> m,(l68,u68),(l95,u95)) |> Array.unzip3
               let (l68,u68) = s |> Array.unzip
               let (l95,u95) = n |> Array.unzip
               { id = id
                 x = x
                 q = {
                   median = m
                   lower68 = l68
                   upper68 = u68
                   lower95 = l95
                   upper95 = u95
                 }
                 color = props.TryFind("color") |> Option.defaultValue "black"
                 size = props.TryFind("size") |> Option.defaultValue "10" |> float }
               |> BoxAndWhiskers
             | "heatmap" ->
               let x = Array.init data.Length float
               let y = Array.init data.[0].Length float
               { id = id
                 x = x
                 y = y
                 values = data
                 colorPalette = props.TryFind("colorPalette") |> Option.defaultValue "red,green"
               }
               |> Heatmap
             | kind -> failwith (sprintf "unknown plot kind %s" kind)
  Plot plot

let parseApi : IPlotParseApi = {
  parse = (fun text -> async{ return parsePlot text })
}

let apiRouter : HttpHandler =
  Remoting.createApi()
  |> Remoting.withRouteBuilder (fun t m -> sprintf "/api/%s/%s" t m)
  |> Remoting.fromValue parseApi
  |> Remoting.buildHttpHandler