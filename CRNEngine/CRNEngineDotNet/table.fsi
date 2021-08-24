// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Table

type column<'v> = { name: string; values: 'v list }
type t<'v> = { times: float list; columns: column<'v> list }

val empty : t<'v> 
val create : times:float list -> headers:string list -> rows:'v list list -> t<'v>
val concat : t<'v> list -> t<'v>
val map    : ('a -> 'b) -> t<'a> -> t<'b>

val likelihood : f:('v -> 'v -> float) -> t1:t<'v> -> t2:t<'v> -> float
val from_rows_reverse : string list -> Row<'v> list -> t<'v>
val from_rows : string list -> Row<'v> list -> t<'v>
val to_rows_reverse : t<'v> -> Row<'v> list
val to_rows : t<'v> -> Row<'v> list
val parse : delim:char [] -> s:string -> t<float>
val parse_tsv : s:string -> t<float>
val parse_csv : s:string -> t<float>
val parse_multiple_tsv : number_of_plots:int -> s:string -> t<float> list
val parse_multiple_csv : number_of_plots:int -> s:string -> t<float> list
val point_to_float : t<Point> -> t<float>
val floatarray_to_float : float list -> t<float []> -> t<float>
val floatarrayarray_to_float : float list -> float list -> t<float [][]> -> t<float>
val from_list_rows : rows:float list list -> names:string list -> t<float>
val from_array_rows : times:float list -> rows:'v [] [] -> names:string list -> t<'v>
val from_array_columns : times:float list -> columns:'v [] [] -> plots:string list -> t<'v>
val find_column : name:string -> t:t<'v> -> column<'v>
val find_column_last : name:string -> t:t<'v> -> 'v
val to_string : separator:string -> value_to_string:('v -> string) -> t:t<'v> -> string
val to_tab_separated_CSV: t<float> -> string