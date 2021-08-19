module Microsoft.Research.CRNEngine.Tests.TableTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Table - from_list_rows, two rows")>]
let from_list_rows_two_rows () =
  let t1:Table<float> = Table<float>.from_list_rows [[log(2.0); 0.5; 1.0]] ["Time";"X1";"X2"] in
  let t2:Table<float> = {
      times = [log(2.0)];
      columns = [{name = "X1"; values = [0.5]}; {name = "X2"; values = [1.0]}]
    }
  in Assert.Equal(t1,t2)

[<Fact(DisplayName="Table - from_array_rows, two rows")>]
let from_array_rows_two_rows () =
  let t1:Table<float> = Table.from_array_rows [log(2.0)] [|[| 0.5; 1.0 |]|] ["X1";"X2"] in
  let t2:Table<float> = {
      times = [log(2.0)];
      columns = [{name = "X1"; values = [0.5]}; {name = "X2"; values = [1.0]}]
    }
  in Assert.Equal(t1,t2)

[<Fact(DisplayName="Table - from_array_columns, two columns")>]
let from_array_columns_two_columns () =
  let t1:Table<float> = Table.from_array_columns [log(2.0)] [|[| 0.5|]; [|1.0 |]|] ["X1";"X2"] in
  let t2:Table<float> = {
      times = [log(2.0)];
      columns = [{name = "X1"; values = [0.5]}; {name = "X2"; values = [1.0]}]
    }
  in Assert.Equal(t1,t2)