module Microsoft.Research.DNA.OldSyntax

open FsUnit
open Xunit
open Microsoft.Research.DNA
open System.IO

(**********************************************************************
 * Tests for convertSL, which converts a Visual DSD program into 
 * a Classic DSD programs. The test data is the suite of online examples
 * in Visual DSD. Converted files are stored in the CLASSIC_DSD folder.
 * Conversion of JIT, directive fit, Z3 queries and spatial examples
 * is currently unspported.
 **********************************************************************)


(* Test functions *)

// append a name to a file path
let (+/) = TestUtils.(+/)

let load fileName = 
  let projectDir  = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let fileName    = projectDir +/ TestUtils.TEST_FOLDER +/ TestUtils.VISUAL_DSD +/ fileName
  File.ReadAllText fileName

let save (fileName : string) extension content =
  let projectDir  = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let fileDotDna  = fileName.Replace (".vdsd", extension)
  let path        = projectDir +/ TestUtils.TEST_FOLDER +/ TestUtils.CLASSIC_DSD +/ fileDotDna
  File.WriteAllText (path, content)

// convert a file, save it, and try to expand it to CRN
let test isJit fileName = 
  let code        = load fileName
  let translation = code |> SLConversion.convertSL

  if isJit
    then 
      // parse translation only. Skip CRN expansion, since it might not terminate in JIT
      Dsd.parse translation |> ignore
    else 
      // test expansion to crn 
      let bundle        = Dsd.parse translation
      let crn           = Dsd.convert_expand bundle
      crn.to_string
       |> ignore

  // save the expanded file in a .dna 
  save fileName ".dna" translation
  

(**********************************************************)
(**********************************************************)
(**************  Test cases  ******************************)
(**********************************************************)
(**********************************************************)

[<Fact>]
[<Trait("Category","Slow")>]
let ``Visual DSD legacy parser``()=
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  System.Threading.Thread.CurrentThread.CurrentUICulture <- System.Globalization.CultureInfo.InvariantCulture
  let tests = [ "Scienze 2007 - Catalytic.vdsd"
                "Scienze 2007 - Catalytic Directives.vdsd"
                "Tutorial, VEMDP 2014 - Join.vdsd"
                "Tutorial, VEMDP 2014 - Join Reporter.vdsd"
                (** [<Fact(Skip="\"predicate\" keyword not supported")>]
                    "Tutorial, VEMDP 2014 - Join Reporter Z3.vdsd" **)
                "Tutorial, VEMDP 2014 - Join Directives.vdsd"
                "Tutorial, VEMDP 2014 - Join Events.vdsd"
                (** [<Fact(Skip="directive \"fit\" not supported")>]
                    "Tutorial, VEMDP 2014 - Join Fit.vdsd" **)
                "Yordanov et al., ACS Synth Biol 2014 - PI Controller - 2Domain.vdsd"
                "Yordanov et al., ACS Synth Biol 2014 - PI Controller - 4Domain.vdsd"
                "Yordanov et al., ACS Synth Biol 2014 - PI Controller - DNA Enzymes.vdsd"
                "Yordanov et al., ACS Synth Biol 2014 - PI Controller - RNA Enzymes.vdsd"
                "Yordanov et al., ACS Synth Biol 2014 - PI Controller - CRN.vdsd"
                "Kim et al., Mol Syst Biol 2011 - Oscillator.vdsd"
                "Montagne et al., Mol Syst Biol 2011 - Oscillator.vdsd"
                "Lakin et al., DNA 2014 - Transmission lines.vdsd"
                "Lakin et al., DNA 2014 - Remote transmission lines.vdsd"
                "Lakin et al., DNA 2014 - Threshold spatial AND.vdsd"
                """Lakin et al., DNA 2014 - Threshold spatial (reporter) AND.vdsd"""
                 (** 
                    [<Fact(Skip="Species not supported in Spatial settings, Core")>]
                    "Dalchau et al., DNA 2014 - Autocatalytic Spatial.vdsd"
                    "Dalchau et al., DNA 2014 - Lotka Spatial.vdsd"
                    
                    [<Fact(Skip="directive \"default diffusion\" not supported")>]
                    "Dalchau et al., DNA 2014 - Consensus Spatial.vdsd"
                  **)
                "Lakin, Parker et al., Interface 2012 - Model-checking examples.vdsd"
                "Lakin et al., Interface 2012 - Join gates.vdsd"
                "Lakin et al., Interface 2012 - Buffered oscillators.vdsd"
                "Lakin et al., Interface 2012 - Unbuffered oscillators.vdsd"
                "Co-operative - Lotka.vdsd"
                "Co-operative - Mapk.vdsd"
                "Simple - AND Circuit.vdsd"
                "Simple - Migrations.vdsd"
                "3-domain, DNA 2016 - Transducer.vdsd"
                "3-domain, DNA 2016 - Transducer Composition.vdsd"
                "3-domain, DNA 2016 - Buffered Transducer.vdsd"
                "3-domain, DNA 2016 - Buffered Fork.vdsd"
                "3-domain, DNA 2016 - Buffered Join.vdsd"
                "3-domain, DNA 2016 - Oscillating.vdsd"
                "3-domain, DNA 2016 - Ultrasensitive.vdsd"
                "2-domain, DCM - Two-domain transducer.vdsd"
                """2-domain, DCM - Two domain fork-join.vdsd"""
                "Zhang, Science 2007 - Fluorescent Catalytic.vdsd"
                "Zhang, Science 2007 - Autocatalytic.vdsd"
                "Zhang, Science 2007 - Feed forward.vdsd"
                "Zhang, Science 2007 - Cross Catalytic.vdsd"
                "Zhang, Science 2007 - And Gate.vdsd"
                "Zhang, Science 2007 - Independent Input.vdsd"
                "Square root - Qian and Winfree 4 bit square root circuit.vdsd"
                "Localization - OR gate.vdsd"
                "Localization - OR gate with nicks.vdsd"
                "Localization - AND gate.vdsd"
                "Localization - AND gate with nicks.vdsd"
                "Localization - Wires.vdsd"
                "Localization - OR of ANDs.vdsd"
                "Localization - Square root.vdsd"
                "Nature Nanotech 2013 - Consensus Modules.vdsd"
                "Nature Nanotech 2013 - Consensus Parameterized.vdsd"
                 (** [<Fact(Skip="directive \"fit\" not supported")>]
                     "Parameter Inference - Reporter.vdsd"
                     "Parameter Inference - Join + Reporter.vdsd" 
                  **)
                 (** [<Fact(Skip="\"query\" keyword not supported")>]
                   "Formal Analysis - (buggy) 2 Transducers.vdsd"
                   "Formal Analysis - 2 Transducers.vdsd"
                   "Formal Analysis - (buggy) 3 Transducers.vdsd"
                   "Formal Analysis - 3 Transducers.vdsd"
                   "Formal Analysis - (buggy) 10 Transducers.vdsd"
                   "Formal Analysis - 10 Transducers.vdsd"
                 **)
                "Formal Analysis - (localized) Fanout.vdsd"
                "Formal Analysis - Fanout.vdsd"
                "Formal Analysis - And.vdsd"
                "Formal Analysis - Or.vdsd"
                "Formal Analysis - (localized) SQRT.vdsd"
                "Formal Analysis - (localized Fanout) SQRT.vdsd"
                "Formal Analysis - SQRT.vdsd"]
  List.map (test false) tests |> ignore
  
  let jitTests = [  "Simple - Monomers.vdsd"
                    "Simple - Hairpin-free HCR.vdsd"
                    "Stack Machine, DNA17 - Ripple carry adder.vdsd" ]
  List.map (test true) jitTests |> ignore


[<Fact>]
let ``Visual DSD legacy parser - functional rate``() =
  System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.InvariantCulture
  let code = """directive parameters
  [ k1 = 0.07
  ; k2 = 0.17
  ; k3 = 0.02 ]
directive simulation deterministic
directive duration 0,100
directive scale 500.0

( {e1} | <x>
| {e2} | <y>
| rxn {e1} + <x> <->{k1,k3} {es1}
| rxn {es1} ->{k2} {e1} + [x]
| rxn {e2} + <y> ->[(k2 * [<y>]) / ((k2+k3)/k1 + [<y>])] {e2} + [y]
)
"""
  code 
  |> SLConversion.convertSL 
  |> Dsd.compile 
  |> ignore