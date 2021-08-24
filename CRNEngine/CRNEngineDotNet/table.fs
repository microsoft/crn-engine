// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
open System.Diagnostics
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Table<'v> = 
  { times: float list; columns: Column<'v> list }
  static member likelihood (penalty_functions:('v -> 'v -> float) list) (sim:Table<'v>) (data:Table<'v>) = 
    let column_likelihood penalty (c1:Column<'v>) (c2:Column<'v>) = List.sum (List.map2 penalty c1.values c2.values)
    List.sum (List.map3 column_likelihood penalty_functions sim.columns data.columns )
  static member empty : Table<'v> = { times = []; columns = [] }
  static member get_column_names (t:Table<'v>) = (List.map (fun c -> c.name) t.columns) 
  static member get_column_values (t:Table<'v>) = (List.map (fun c -> c.values) t.columns) 
  static member remove_column (name:string) (t:Table<'v>) = { t with columns = List.filter (fun c -> name <> c.name) t.columns }
  static member find_column (name:string) (t:Table<'v>) = List.find (fun c -> name = c.name) t.columns 
  static member find_column_last (name:string) (t:Table<'v>) = List.head <| List.rev (Table.find_column name t).values
  static member reverse (t:Table<'v>) = {
    times = List.rev t.times;
    columns = List.map Column.reverse t.columns
  }
  static member average_columns (tables: Table<float> list) : Table<float> = 
    let times = tables.Head.times
    List.init tables.Head.columns.Length (fun i -> 
      let column_values = tables |> List.map (fun table -> table.columns.[i].values)
      let column_name = tables.Head.columns.[i].name
      let ssummary = 
        List.init times.Length (fun j -> 
          let values = column_values |> List.map (List.item j)
          values |> Microsoft.Research.Filzbach.FilzbachAnalysis.summary
        )
      Column.create (ssummary |> List.map (fun s -> s.mean)) column_name
    )
    |> fun cols -> { times=times; columns=cols }
  static member qsummary (tables: Table<float> list) : Table<float> = 
    let times = tables.Head.times
    List.init tables.Head.columns.Length (fun i -> 
      let column_values = tables |> List.map (fun table -> table.columns.[i].values)
      let column_name = tables.Head.columns.[i].name
      let ssummary, qsummary = 
        List.init times.Length (fun j -> 
          let values = column_values |> List.map (List.item j)
          let s = values |> Microsoft.Research.Filzbach.FilzbachAnalysis.summary
          let qs = values |> Microsoft.Research.Filzbach.FilzbachAnalysis.qsummary 
          s, qs
        )
        |> List.unzip
      [ Column.create (ssummary |> List.map (fun s -> s.mean)) (column_name + " Mean")
      ; Column.create (ssummary |> List.map (fun s -> sqrt s.variance)) (column_name + " Std")
      ; Column.create (qsummary |> List.map (fun q -> q.lb95)) (column_name + " L95")
      ; Column.create (qsummary |> List.map (fun q -> q.ub95)) (column_name + " U95") ] 
    )
    |> List.concat
    |> fun cols -> { times=times; columns=cols }
  static member concat (tables: Table<'v> list) : Table<'v> =
    let times = tables.Head.times
    let columns : Column<'v> list = List.fold (fun cols tab -> 
      if (tab.times = times)
      then List.append cols tab.columns
      else failwith "Cannot merge tables with different time-points") [] tables
    { times = times; columns = columns }
  static member from_rows_reverse (column_names:string list) (rows:Row<'v> list) = 
    let add_value (v:'v) (c:Column<'v>) = {c with values = v::c.values}
    let f (t:Table<'v>) (r:Row<'v>) =
      { t with
          times = r.time::t.times
          columns = List.map2 add_value (List.ofArray r.values) t.columns
      }
    let columns:Column<'v> list = List.map (Column.create []) column_names
    List.fold f { times = []; columns = columns } rows
  static member from_rows (column_names:string list) (rows:Row<'v> list) = Table.reverse <| Table.from_rows_reverse column_names rows
  static member to_rows_reverse (t:Table<'v>) = 
    let add_row (rows:Row<'v> list, columns:'v list list) (time:float) = 
      let values:'v list = List.map List.head columns
      let row:Row<'v> = Row.create time values
      row::rows, (List.map List.tail columns)
   
    fst <| List.fold add_row ([],Table.get_column_values t) t.times
  static member to_rows (t:Table<'v>) = List.rev <| Table.to_rows_reverse t
  static member create (times:float list) (headers:string list) (rows:'v list list) = Table.from_rows headers (List.map2 Row.create times rows)
  static member filter_by_tmax tmax (t:Table<'v>) = 
    match t.times |> List.tryFindIndex (fun t -> t > tmax) with
    | Some loc -> { times = t.times.[0..loc-1]; columns = t.columns |> List.map (fun col -> { col with values = col.values.[0..loc-1] }) }
    | None     -> t
  ///Convert a table of point columns (mean and standard deviation) into a table of float columns (doubles the numbers of columns, putting a block of means then a block of standard deviations)
  static member point_to_float (tab:Table<Point>) : Table<float> =
    let mean_cols = tab.columns |> List.map (fun col -> { name = col.name + " (Mean)"; values = col.values |> List.map (fun v -> v.mean) })
    let stdev_cols = tab.columns |> List.map (fun col -> { name = col.name + " (StDev)"; values = col.values |> List.map (fun v -> v.stdev) })
    { times = tab.times; columns = List.append mean_cols stdev_cols }
  /// Convert a table of float[] into columns for each spatial point
  static member floatarray_to_float xs (tab:Table<float[]>) : Table<float> =
    let columns = tab.columns |> List.collect (fun col -> xs |> List.mapi (fun i x -> { name = sprintf "%s (x = %1.6f)" col.name x; values = col.values |> List.map (Array.item i) }))
    { times = tab.times; columns = columns }
  /// Convert a table of float[][] into columns for each spatial point
  static member floatarrayarray_to_float xs ys (tab:Table<float[][]>) : Table<float> =
    let columns = 
      tab.columns 
      |> List.collect (fun col -> 
        xs 
        |> List.mapi (fun i x -> 
          ys
          |> List.mapi (fun j y ->
            { name = sprintf "%s (x = %1.6f, y = %1.6f)" col.name x y; values = col.values |> List.map (fun c -> c.[i].[j]) }
          )
        )
        |> List.concat
      )
    { times = tab.times; columns = columns }
  ///Assume that the first column is time in both the rows and the names
  static member from_list_rows (rows:float list list) (names:string list) =
    match names with 
    | [] -> Table<float>.empty
    | n::ns -> Table.from_rows ns (List.map Row<float>.from_list rows)
  ///Assume that the plot names do not include the times
  static member from_array_rows (times:float list) (rows:'v[][]) (plots:string list) = 
    let f (time:float) (row:'v[]) = Row.from_array time row
    let rows:Row<'v> list = List.ofSeq <| Seq.map2 f times rows 
    Table.from_rows_reverse plots (List.rev rows)
  static member from_array_columns (times:float list) (columns:'v[][]) (plots:string list) = 
    let f (name:string) (column:'v[]) = Column.create (List.ofArray column) name
    {times = times; columns = List.ofSeq <| Seq.map2 f plots columns}
  static member to_string (separator:string) (value_to_string:'v -> string) (t:Table<'v>) = 
    let headers = "Time"::(Table.get_column_names t)
    let rows:Row<'v> list = Table.to_rows t 
    String.concat "\r\n" <| (String.concat separator headers) :: (List.map (Row.to_string separator value_to_string) rows)
  static member to_string_horizontal (separator:string) (value_to_string:'v -> string) (t:Table<'v>) = 
    let headers = "Time"::(t.times |> List.map string) |> String.concat separator
    let rows:string list = t.columns |> List.map (fun col -> col.name :: (col.values |> List.map value_to_string) |> String.concat separator)
    String.concat "\r\n" <| headers :: rows
  static member to_tab_separated_CSV (data:Table<float>) = Table.to_string "\t" (fun x -> x.ToString()) data
  static member map (f:'v -> 'b) (t:Table<'v>) : Table<'b> = 
    let mapColumn c = Column.create (c.values |> List.map f) c.name
    { times   = t.times 
      columns = t.columns |> List.map mapColumn }

  //TODO: optimise the following to reduce array to list conversions. 
  static member parse delim (s:string) = 
    let str = s.Split('\r','\n') |> Array.filter (fun s -> s <> "")
    let names = str.[0].Split(delim)
    let rows = Array.map (fun (row:string) -> row.Split(delim) |> Array.map float) str.[1..]
    Table<float>.from_list_rows ((Array.map List.ofArray rows) |> List.ofArray) (List.ofArray names)
  static member parse_tsv (s:string) = Table<float>.parse [|'\t'|] s
  static member parse_csv (s:string) = Table<float>.parse [|','|] s
  static member parse_multiple delim (number_of_plots:int) (s:string) = 
    let t:Table<float> = Table<float>.parse delim s
    let columns_list = Lib.split number_of_plots t.columns
    List.map (fun columns -> {times = t.times; columns = columns}) columns_list
  static member parse_multiple_tsv nplots s = Table<float>.parse_multiple [|'\t'|] nplots s
  static member parse_multiple_csv nplots s = Table<float>.parse_multiple [|','|] nplots s

