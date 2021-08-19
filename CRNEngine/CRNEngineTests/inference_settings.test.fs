module Microsoft.Research.CRNEngine.Tests.Inference_settingsTest
open Xunit
open FsUnit.Xunit
 open Microsoft.Research.CRNEngine
 
[<Fact(DisplayName="Inference settings - record")>]
let record () =
  let expected = { Inference_settings.defaults with samples = 20000 } in
  let got = Inference_settings.from_string "{ samples = 20000 }"  in
  Assert.Equal(expected, got)
