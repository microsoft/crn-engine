module Microsoft.Research.GEC.GecSpeciesTest

open Parser

open Xunit
open System.Diagnostics


[<Fact(DisplayName="GEC - Species - To String")>]
let ``speciesToStringTest``() = 
   let species0 = {GecSpecies.t.empty_Species with species = ["a"]} 
   let species1 = {GecSpecies.t.empty_Species with species = ["a";"b"]} 
   let species2 = {GecSpecies.t.empty_Species with species = ["a";"b";"c";"d"]} 
   Assert.Equal(GecSpecies.t.empty_Species.to_string(),"")
   Assert.Equal(species0.to_string(),"a")
   Assert.Equal(species1.to_string(),"a::b")
   Assert.Equal(species2.to_string(),"a::b::c::d")

[<Fact(DisplayName="GEC - Species - To CRN String")>]
let ``speciesToCrnStringTest``() = 
   let species0 = {GecSpecies.t.empty_Species with species = ["a"]} 
   let species1 = {GecSpecies.t.empty_Species with species = ["a";"b"]} 
   let species2 = {GecSpecies.t.empty_Species with species = ["a";"b";"c";"d"]} 
   Assert.Equal(GecSpecies.t.empty_Species.to_string(),"")
   Assert.Equal(species0.to_crn_string(),"a")
   Assert.Equal(species1.to_crn_string(),"a_b")
   Assert.Equal(species2.to_crn_string(),"a_b_c_d")

[<Fact(DisplayName="GEC - Species - Parse Tests")>]
let speciesParseTest() = 
  let species0 = "Signal"
  let species1 = "Receiver::Signal"
  let species2 = "cell[gfp]"
  let species3 = "a::b::c"
  let get_species (s:string) = Parser.from_string GecSpecies.parse s 
  let s0 = get_species species0
  let s1 = get_species species1
  let s2 = get_species species2
  let s3 = get_species species3
  
  //Compartment tests
  Assert.Equal(s0.compartment,None)
  Assert.Equal(s1.compartment,None)
  Assert.Equal(s2.compartment,Some("cell"))
  Assert.Equal(s3.compartment,None)

  //Length tests
  Assert.Equal(s0.species.Length,1)
  Assert.Equal(s1.species.Length,2)
  Assert.Equal(s2.species.Length,1)
  Assert.Equal(s3.species.Length,3)



