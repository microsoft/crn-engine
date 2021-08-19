module Microsoft.Research.CRNEngine.Tests.StatisticsTest
open Xunit
open FsUnit.Xunit
open Microsoft.Research.CRNEngine

[<Fact(DisplayName="Statistics - Pearson correlation matrix")>]
let correlation () = 
  let data = 
    [| [| 2.0; 3.0; 5.0; 4.0|]
     ; [| 4.0; 6.0; 4.0; 4.0|]
     ; [| 5.0; 4.0; 2.0; 3.0|]
     ; [| 1.0; -1.0; 3.0; -0.2|]
    |]
  let expected = 
    [| [|  1.0000; -0.2582; -1.0000;  0.5046|]
     ; [| -0.2582;  1.0000;  0.2582; -0.6514|]
     ; [| -1.0000;  0.2582;  1.0000; -0.5046|]
     ; [|  0.5046; -0.6514; -0.5046;  1.0000|] 
     |]
  let got = Statistics.correlation_pearson data
  (expected, got)
  ||> Array.iter2 (
    Array.iter2 (fun eij gij -> 
      Assert.Equal (eij, gij, 3)
    )
  ) 
