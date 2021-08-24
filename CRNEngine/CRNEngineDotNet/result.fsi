// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Result

type t<'v> = {
  instance: Instance.t;
  table: Table.t<'v>;
}

val empty : t<'v>
val create : Instance.t -> Table.t<'v> -> t<'v>
val to_table : t<'v> -> Table.t<'v>
val group_sweeps : t<'v> list -> t<'v> list list
val tsv_parser : Parser.t<t<float>>