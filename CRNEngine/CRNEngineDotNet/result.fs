// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open Operators
[<JavaScript>]
type Result<'v> = 
  { instance:Instance<Functional>; table:Table<'v> }
  static member create (i:Instance<Functional>) (t:Table<'v>) = {instance = i; table = t}
  static member empty = {instance = Instance<Functional>.empty; table = Table<'v>.empty}
  static member to_table (r:Result<'v>) = r.table 
  static member same_sweep (r1:Result<'v>) (r2:Result<'v>) = Instance.same_sweep r1.instance r2.instance
  static member append_sweepstr_columns (res:Result<'v>) = 
    let sweepstr = Environment.to_string res.instance.environment
    let columns = res.table.columns |> List.map (fun col -> {col with name = sprintf "%s (%s)" col.name sweepstr}) 
    { res with table = {res.table with columns = columns} } 
  static member group_sweeps (rs:Result<'v> list) =   
    let rec f (acc:Result<'v> list list) (rs:Result<'v> list) = 
      match rs with
      | [] -> List.rev acc
      | r::rs -> 
         let same,rest = List.partition (Result<_>.same_sweep r) rs 
         f ((r::same)::acc) rest
    f [] rs
    |> List.map (fun r -> 
      if List.length r > 1 
      then List.map Result<_>.append_sweepstr_columns r
      else r)
  static member concat (rs:Result<'a> list) = 
    { instance = rs.Head.instance; table = rs |> List.map (fun r -> r.table) |> Table.concat }
  static member qsummary (rs:Result<float> list) = 
    //let unique_instances = rs |> List.map (fun r -> r.instance) |> List.distinct
    //if List.length unique_instances > 1 
    //then failwith "Trying to combine results for different instances" 
    //else { instance = unique_instances.Head; table = rs |> List.map (fun r -> r.table) |> Table<float>.qsummary }
    { instance = rs.Head.instance; table = rs |> List.map (fun r -> r.table) |> Table<float>.qsummary }
  ///hs is a list of N headers
  ///rs is a a matrix of floats as a list of rows. In turn, each row is a list of N floats.
  ///We assume that rs[0] is a timestamp, whereas rs[i] is a simulation value for i > 0.
  ///The Result is what Crn.simulate returns
  static member createResults (hs : string list) (rs : float list list) : Result<float> = 
    //separate timestamps from data rows
    let rowAppend (x, y) (xs, ys) = (x::xs, y::ys)
    let splitRow (xs:float list) = 
      if xs.Length < 2
      then failwith "Missing simulation data."
      else (xs.Head, xs.Tail)
    let times, rows = rs |> List.map splitRow |> fun xs -> List.foldBack rowAppend xs ([], [])      
    //create results from a table
    let table = Table.create times (hs.Tail) rows // hs.Tail removes the timestamps' header
    Result<_>.create (Instance<Functional>.empty)  table
  static member parse_row size   = 
    (Parser.sepBy Parser.pfloat (Parser.pstring "\t")) .>> (Parser.linebreak <|> Parser.eof)
    //check if the row has the right length
    |~> fun (_, r) ns -> 
          if ns.Length <> size
          then //case 1: row length doesn't match headers' length
            let errorMsg = 
              "Row " + (r+1).ToString() +
              " has " + ns.Length.ToString() +
              " columns instead of " + size.ToString()
            Choice1Of2 errorMsg
          else Choice2Of2 ns //case2: all good            
    //stop parsing if row's and headers' lengths mismatch
    >>= fun r -> 
          match r with
          | Choice1Of2 msg -> Parser.failParser msg
          | Choice2Of2 row -> Parser.preturn row
  ///Result parser from a Table-Separated Value file (.tsv)
  ///Each row must be as long as the headers
  static member tsv_parser =  
    let parse_rows size  = Parser.manyTill (Result<'v>.parse_row size) Parser.eof 
    let parse_header   = Parser.many1Satisfy (fun c -> c <> '\t' && c <> '\r' && c <> '\n')
    let parse_headers  = (Parser.sepBy parse_header (Parser.pstring "\t")) .>> Parser.linebreak
    parse_headers >>= fun hs -> parse_rows (hs.Length) |>> Result<'v>.createResults hs   
