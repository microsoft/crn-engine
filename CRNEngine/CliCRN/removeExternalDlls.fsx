(* tools like BinSkim need all pdbs, get rid of e.g. xUnit from output *)

open System.IO

let rec files directories =
    if Seq.isEmpty directories then Seq.empty else
        seq { yield! directories |> Seq.collect Directory.EnumerateFiles
              yield! directories |> Seq.collect Directory.EnumerateDirectories |> files }

let xunitPaths = files [__SOURCE_DIRECTORY__] |> Seq.filter (fun path -> path.Contains("xunit") || path.Contains("NHamcrest"))

xunitPaths |> Seq.iter File.Delete