module Microsoft.Research.DNA.DSDUnitTests

open FsUnit
open Xunit

open System.IO
open Microsoft.Research.CRNEngine.Jit
open Microsoft.Research.DNA
open Microsoft.Research.DNA.Dsd
open Microsoft.Research.CRNEngine
open RulesDSD

let cle = Microsoft.Research.DNA.LogicDSD.engine

[<Fact(DisplayName="DSD - JIT CTMC")>]
let testJitCtmc () =
  // This model cannot be expanded without JIT.
  let code = """directive simulation {final=600}
  directive simulator stochastic
  directive jit
  directive polymers
  directive parameters [k=0.003;u=0.1]
  directive compilation infinite
  dom tb = {bind=k;unbind=u;colour="red"}
  dom tx = {bind=k;unbind=u;colour="green"}
  dom to = {bind=k;unbind=u;colour="blue"}
  def Input1() = <tb^ x>
  def Input2() = <tx^ x>
  def Join() = {tb^*}[x tx^]:[x]{to^>
  def Reporter() = <tb^}[x]{to^*}
  ( 3 Input1()
  | 3 Input2()
  | 3 Join()
  | 2 Reporter()
  )
  """
  let bundle = Dsd.parse code
  match Dsd.to_jit bundle with
  | Choice2Of2 _ -> failwith ""
  | Choice1Of2 jit ->
    let calculus = Dsd.makeDsdCalculus bundle
    let result,_ = Jit.to_ctmc jit calculus
    let states = result.ctmc.graph.Count
    let transitions = result.ctmc.graph |> Seq.map (fun k -> List.length k.Value) |> Seq.sum
    let expectedStates = 32
    let expectedTransitions = 68
    if states <> expectedStates then failwithf "expected %d states, found %d" expectedStates states
    if transitions <> expectedTransitions then failwithf "expected %d transitions, found %d" expectedTransitions transitions
  ()

[<Fact(DisplayName="Logic DSD - JIT - Polymerase/Exonuclease")>]
let testPolExo () =
  let code = """directive jit
directive simulation {final = 10; plots = []}
directive rules {
reaction([P],"pol",Q) :- 
  P = C [b>],  
  Q = C [b b>].

reaction([P],"exo",Q) :- 
  P = C [b b>],  
  Q = C [b>].

}
directive parameters [ pol = 0.3; exo = 1 ]
( 10 [<tb^ b>] )"""
  let bundle   = code   |> Dsd.parse
  match bundle |> Dsd.to_jit with
  | Choice1Of2 _   -> failwith ""
  | Choice2Of2 jit -> 
      let (guiBundle,_) = JSAPI.parser "" "" code
      let calculus = bundle |> Dsd.to_rules_calculus
      let crn = Dsd.convert_unexpanded guiBundle
      let plots = ref (List.map (fun p -> (Functional2.to_string p, new System.Collections.Generic.List<float>())) crn.settings.simulation.plots |> List.toArray)
      let addRow (row:row) =
        if row.values.Length <> (!plots).Length then failwith "row has wrong length" else
        Array.iteri (fun i f -> match (!plots).[i] with (_,l) -> l.Add f) row.values
      let addPlottable (p:newplottable) =
        let newList = Array.zeroCreate (match (!plots).[0] with (_,l) -> l.Count)
        plots := Array.append !plots [|(p.name, new System.Collections.Generic.List<float>(newList))|]
      Jit.simulate_callback 
        (ref false) 
        addRow
        addPlottable
        jit
        calculus
      ()

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Double stranded")>]
let convTest1 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * [a b c]
    | 1 * [<a!0 b!1 c!2> | <c*!2 b*!1 a*!0>])
    """
  let x = Dsd.compile code
  Assert.True(x.initials.Length = 1)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Gate")>]
let convTest2 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * {a b}<c d>[e f]<g h>{i j})
    """
  let x = Dsd.compile code
  Assert.Equal("<c d e!0 f!1 g h> | <j i f*!1 e*!0 b a>", x.attributes.["sp_0"].structure)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Hairpin right")>]
let convTest5 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * {a b}<c d>[e f]{g h>)
    """
  let x = Dsd.compile code
  Assert.Equal("<c d e!0 f!1 h g f*!1 e*!0 b a>", x.attributes.["sp_0"].structure)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Hairpin left")>]
let convTest6 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * <a b}[c d]<e f>{g h})
    """
  let x = Dsd.compile code
  Assert.Equal("<h g d*!0 c*!1 b a c!1 d!0 e f>", x.attributes.["sp_0"].structure)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Join Top")>]
let convTest3 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * {a1 b1}<c1 d1>[e1 f1]<g1 h1>{i1 j1}::{a2 b2}<c2 d2>[e2 f2]<g2 h2>{i2 j2})
    """
  let x = Dsd.compile code
  Assert.Equal("<c1 d1 e1!0 f1!1 g1 h1 c2 d2 e2!2 f2!3 g2 h2> | <j1 i1 f1*!1 e1*!0 b1 a1> | <j2 i2 f2*!3 e2*!2 b2 a2>", x.attributes.["sp_0"].structure)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Join Bottom")>]
let convTest4 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * {a1 b1}<c1 d1>[e1 f1]<g1 h1>{i1 j1}:{a2 b2}<c2 d2>[e2 f2]<g2 h2>{i2 j2})
    """
  let x = Dsd.compile code
  Assert.Equal("<c1 d1 e1!0 f1!1 g1 h1> | <c2 d2 e2!2 f2!3 g2 h2> | <j2 i2 f2*!3 e2*!2 b2 a2 j1 i1 f1*!1 e1*!0 b1 a1>", x.attributes.["sp_0"].structure)


[<Fact(DisplayName="Logic DSD - Retrocompatibility - Join Hairpin right")>]
let convTest7 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * {a1 b1}<c1 d1>[e1 f1]<g1 h1>{i1 j1}:{a2 b2}<c2 d2>[e2 f2]{g2 h2>)
    """
  let x = Dsd.compile code
  Assert.Equal("<c1 d1 e1!0 f1!1 g1 h1> | <c2 d2 e2!2 f2!3 h2 g2 f2*!3 e2*!2 b2 a2 j1 i1 f1*!1 e1*!0 b1 a1>", x.attributes.["sp_0"].structure)

[<Fact(DisplayName="Logic DSD - Retrocompatibility - Join Hairpin left")>]
let convTest8 () = 
  let code = """
    directive simulation {final = 1}
    directive rules {}
    (1 * <a1 b1}[c1 d1]<e1 f1>{g1 h1}:{a2 b2}<c2 d2>[e2 f2]<g2 h2>{i2 j2})
    """
  let x = Dsd.compile code
  Assert.Equal("<c2 d2 e2!0 f2!1 g2 h2> | <j2 i2 f2*!1 e2*!0 b2 a2 h1 g1 d1*!2 c1*!3 b1 a1 c1!3 d1!2 e1 f1>", x.attributes.["sp_0"].structure)


[<Fact(DisplayName="Logic DSD - Plotting - plot expressions")>]
let plots1 () = 
  let code = """
directive rules {}
directive parameters [
bind = 1;
]
directive simulation { final=150000; plots=[ [A()]-[A'()]; [B()]-[B'()]; [C()]-[C'()] ]}
def A()  = [<a1>]
def A'() = [<a2>]
def B()  = [<b1>]
def B'() = [<b2>]
def C()  = [<c1>]
def C'() = [<c2>]

( 1 A()
| 1 A'()
| 1 B()
| 1 B'()
| 1 C()
| 1 C'()
)
"""
  let x = Dsd.compile code
  Assert.Equal(3, x.settings.simulation.plots.Length)
  
let runJitTest callback fileName =
  let (+/) a b    = TestUtils.(+/) a b
  let projectDir  = Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.FullName.Trim [|' ';'\r';'\n';'\t'|]
  let filePath    = TestUtils.TEST_FOLDER +/ TestUtils.VISUAL_DSD +/ fileName
  let fileContent = File.ReadAllText (projectDir +/ filePath)
  let text        = SLConversion.convertSL fileContent 
  let bundle      = Dsd.parse text
  let options = Dsd.get_options bundle
  let options = { options with is_jit = true }
  let bundle = Dsd.set_options bundle options
  let jit         = match Dsd.to_jit bundle with Choice1Of2 jit -> jit | _ -> failwith "this test is not for Rules JIT"
  let calculus    = Dsd.makeDsdCalculus bundle
  
  if callback then
    let initialCRN = Dsd.convert_unexpanded bundle
    let mutable plotsCount = List.length initialCRN.settings.simulation.plots
    let outputplottable p = plotsCount <- plotsCount + 1
    // Ensure that the number of columns remains consistant with the number of plottables at all times.
    let output (r:Microsoft.Research.CRNEngine.Row<float>) = Assert.Equal (plotsCount, (Array.length r.values))
    Microsoft.Research.CRNEngine.Jit.simulate_callback (ref false) output outputplottable jit calculus
  else
    let _, results  = Microsoft.Research.CRNEngine.Jit.simulate jit calculus
    // print the results table, for debugging purposes
    let stringTab = Microsoft.Research.CRNEngine.Table.to_string "\t" (fun x -> x.ToString()) results
    ignore stringTab

[<Fact>]
let ``JIT, Hairpin-free HCR`` () = runJitTest false "Simple - Hairpin-free HCR.vdsd"

[<Fact>]
let ``JIT, Monomers`` () = runJitTest false "Simple - Monomers.vdsd"

[<Fact>]
let ``JIT, Stack Machine`` () = runJitTest false "Stack Machine, DNA17 - Ripple carry adder.vdsd"

[<Fact>]
let ``JIT, Catalytic, Callback`` () = runJitTest true "Scienze 2007 - Catalytic.vdsd"

[<Fact>]
let ``JIT, Catalytic Directives, Callback`` () = runJitTest true "Scienze 2007 - Catalytic Directives.vdsd"

[<Fact(DisplayName="ClassicDSD - JIT - GUI plot patterns")>]
let testJit () = 
  let code = """(* 
    Demonstration of cascading: 
        Out = 1000 if (((#A >= 1) && (#B >= 1)) || (#C >= 1)) && (#D ~<~ 100) and otherwise 0
    where ~<~ means roughly less than, i.e. we're OK if it doesn't work perfectly near 100.

    In practice, the system below behaves well unless D is roughly between 95 and 105
*)
directive simulation { final=30000; points=1000; plots = [<a^ A>; <b^ B>; <c^ C>; <d^ D>; < _ h^ X>; <k^ W>; <4 5^ 2>] }
directive simulator stochastic
directive jit
(* just-in-time network enumeration is primarily useful for polymers, 
but also incidentally turns on the Export/Final panel, which can be nice *)
directive compilation infinite

( 0 * <a^ A>  (* input A *)
| 0 * <b^ B>  (* input B *)
| 10 * <c^ C>   (* input C *)
| 95 * <d^ D>  (* input D *)
(* translate A into Y and B into Z while keeping low copy numbers *)
| 1 * {a^*}[A i^]<Y>
| 1 * {b^*}[B j^]<Z>
(* our previous AND gate "from the refrigerator": X = Y AND Z *)
| 1 * {j^*}[Z i^]:[Y h^]<X> 
(* a "wire-OR" via translating C to X *)
| 1 * {c^*}[C h^]<X>
(* a threshold to absorb the first 100 of D and simultaneously deplete W *)
| 200 * {d^*}[D k^]:[W]
| 100 * <k^ W>
| 200 * [D]{k^*}
(* remaining W, if any, can enable X to transform into pre-autocatalyst *)
| 1 * {h^*}[X k^]:[W]<1^>
| 10 * <X k^>
(* translate pre-autocatalyst to the input of an autocatalyst from our "refrigerator" *)
| 1 * <4>[5^ W]{1^*}
(* here is our autocatalyst <4 5^ 2> ; it can be triggered by any <? 4 5^ ?> *)
| 1300 * <5^ 2 3^ 4>
| 1000 * <4>[5^ 2]:<6>[3^ 4]{5^*}
)"""

  let (guiBundle,_) = JSAPI.parser "" "" code
  let jit       = match Dsd.to_jit guiBundle with Choice1Of2 c -> c | _ -> failwith "this test should not produce a Rules bundle"
  let calculus  = Dsd.makeDsdCalculus guiBundle
  let crn = Dsd.convert_unexpanded guiBundle
  let plots = ref (List.map (fun p -> (Functional2.to_string p, new System.Collections.Generic.List<float>())) crn.settings.simulation.plots |> List.toArray)
  let addRow (row:row) =
    if row.values.Length <> (!plots).Length then failwith "row has wrong length" else
    Array.iteri (fun i f -> match (!plots).[i] with (_,l) -> l.Add f) row.values
  let addPlottable (p:newplottable) =
    let newList = Array.zeroCreate (match (!plots).[0] with (_,l) -> l.Count)
    plots := Array.append !plots [|(p.name, new System.Collections.Generic.List<float>(newList))|]
  Jit.simulate_callback 
    (ref false) 
    addRow
    addPlottable
    jit
    calculus
  ()


[<Fact(DisplayName="ClassicDSD - JIT - empty plots, GUI pattern")>]
let testJitEmptyPlots () = 
  let code = """directive jit
directive simulation { final = 600; }
directive parameters [ k = 0.003; u = 0.1; ]
directive compilation infinite
dom tb = {bind = k; unbind = u; colour = "red"}
dom tx = {bind = k; unbind = u; colour = "green"}
dom to = {bind = k; unbind = u; colour = "blue"}
def Input1() = <tb^ b>
def Input2() = <tx^ x>
def Output() = <x to^>
def Join() = {tb^*}[b tx^]:[x to^]
def Reporter() = <fl^>[x]{to^*}
def Signal() = <fl^ x>
( 10 Input1()
| 10 Input2()
| 0 Output()
| 100 Join()
| 100 Reporter()
| 0 Signal()
)"""
  let (guiBundle,_) = JSAPI.parser "" "" code
  let jit       = match Dsd.to_jit guiBundle with Choice1Of2 c -> c | _ -> failwith "this test should not produce a Rules bundle"
  let calculus  = Dsd.makeDsdCalculus guiBundle
  let crn = Dsd.convert_unexpanded guiBundle
  let plots = ref (List.map (fun p -> (Functional2.to_string p, new System.Collections.Generic.List<float>())) crn.settings.simulation.plots |> List.toArray)
  let addRow (row:row) =
    if row.values.Length <> (!plots).Length then failwith "row has wrong length" else
    Array.iteri (fun i f -> match (!plots).[i] with (_,l) -> l.Add f) row.values
  let addPlottable (p:newplottable) =
    let newList = Array.zeroCreate (match (!plots).[0] with (_,l) -> l.Count)
    plots := Array.append !plots [|(p.name, new System.Collections.Generic.List<float>(newList))|]
  Jit.simulate_callback 
    (ref false) 
    addRow
    addPlottable
    jit
    calculus
  ()
  
[<Fact>]
let ``Melting with non-trivial double segment`` () =
//  let catalytic = 
//    "new 3@ 4.2E-4 , 4.0E-2
//     new 5@ 6.5E-4 , 4.0E-3
//     ( 13 * <2 3^ 4>
//     | 10 * <4 5^>
//     | 10 * <1>[2]:<6>[3^ 4]{5^*}
//     )"
//  let t1 = compile catalytic

//  let v1  = "[ (a, 100000); (b, 100000) ]"
//  let v1p = Parser.from_string Option.localConc v1

  let t2text = """  directive localconcentrations [ (a, 100000); (b, 100000) ]
        directive polymers

        dom a0 = { colour = "red" }
        dom x  = { colour = "green" }
        dom y  = { colour = "blue" }
        dom r  = { colour = "purple" }
        dom Q  = { colour = "black" }
        dom F  = { colour = "black" }

        def input()    = <a0^ s>
        def fuel()     = <y^*>[s*]{x^>
        def probe()    = <r^*>[s*]<Q^>{F^}
        def origami()  = [[ {tether(a) a0^*}[s]{y^>
                          | {tether(a,b) x^*}[s]{y^>
                          | {tether(b) x^*}[s]{r^> ]]
        def reporter() = {s F^}

        ( input()
        | 2 of fuel()
        | probe()
        | origami()
        | 0 of reporter()
        )
        """
  let t2' = compile t2text

//  let transmission_lines =
//    """directive polymers
//        directive localconcentrations [(a,100000);(b,100000)]dom a0 = { colour = "red" }
//        dom x  = { colour = "green" }
//        dom y  = { colour = "blue" }
//        dom r  = { colour = "purple" }
//        dom Q  = { colour = "black" }
//        dom F  = { colour = "black" }
//
//        def input()    = <a0^ s>
//        def fuel()     = <y^*>[s*]{x^>
//        def probe()    = <r^*>[s*]<Q^>{F^}
//        def origami()  = [[ {tether(a) a0^*}[s]{y^>
//                          | {tether(a,b) x^*}[s]{y^>
//                          | {tether(b) x^*}[s]{r^> ]]
//        def reporter() = {s F^}
//
//        ( input()
//        | 2 of fuel()
//        | probe()
//        | origami()
//        | 0 of reporter()
//        )"""
  
//  let t2 = compile transmission_lines
  
  let test =
    """(<x>{y}[z] | [z]{y}<x> | <x>[z]{y} | <a>{b}[z]{y}<b>:<x>[z]{y} | <x>[z]{y}::<a>[z]{y}<b>)"""

  let crn = Dsd.compile test
  ()
//  crn |> Crn.to_string |> printfn "Compiled CRN:\n%s\n---"

//  let style = ""
//  let svg = crn |> Crn.to_svg |> Svg.to_string style
//  System.IO.File.WriteAllText ("crn.svg", svg)
//
//  printfn "Wrote file crn.svg"
//
//  System.Console.ReadLine () |> ignore;
//  0


//    let opts = Options.default_options
//    let s =
//      "{x*}<x3^*z1>[z^*x1]<y*>{z3^x2}::{y3^z1}<x4^*y1>[y2]<z4*>{x*}"
//      |> D.parse
////      |> List.collect Species.melt
//    let strands =
//      [ "{x* z^ x1* z3^ x2}"
//      ; "<x3^* z1 z^* x1 y* x4^* y1 y2 z4*>"
//      ; "{y3^ z1 y2* x*}" ]
//      |> List.collect (parse_species opts)
//    Lib.is_permutation (Species.equal opts) s strands
//      |> should be (equal true)

[<Fact>]
let ``directive rendermode`` () =
  let text x = """directive rendering {renderer = classic ; mode = """ + x + """}

        dom a0 = { colour = "red" }
        dom x  = { colour = "green" }
        dom y  = { colour = "blue" }
        dom r  = { colour = "purple" }
        dom Q  = { colour = "black" }
        dom F  = { colour = "black" }

        def input()    = <a0^ s>
        def fuel()     = <y^*>[s*]{x^>
        def probe()    = <r^*>[s*]<Q^>{F^}
        def reporter() = {s F^}

        ( input()
        | 2 of fuel()
        | probe()
        | 0 of reporter()
        )
        """
  text "complement"   |> compile |> ignore
  text "condensed"    |> compile |> ignore
  text "nucleotides"  |> compile |> ignore


[<Fact>]
let ``position in type errors`` () =
  let code = """
  directive parameters [b=0]
  def m(a) = <a>
  (  m(b)  )
  """

  try code |> Dsd.parse |> ignore
  with
  | :? Microsoft.Research.DNA.Errors.DSDException as e -> Assert.Equal(e.BeginPos, Some(3,2))
  | e                                                  -> raise e
 

[<Fact>]
let ``origami, reactions count`` () =
  let code = """
directive localconcentrations 
  [ (l1,local); (l2,local); (l3,local); (l4,local); (l5,local); (l6,local); (l7,local7); (l,local); (ds,localds)
  ; (lv,local); (vds,localds); (vds3,localds3); (vds4,localds4)
  ; (t1,localt); (t2,localt)]
directive polymers
directive parameters [ 
  ka=1.0;
  kx=1.0;
  kfr=1.0;
  ky=1.0;
  kyl=1.0;
  ky2=1.0;
  kf=1.0;
  kl=1.0;
  
  local=1.0;
  local7=1.0;
  localds=1.0;
  localds3=1.0;
  localds4=1.0;
  localt=1.0;
  ro=1.0;
  rt=1.0;
  kt=1.0;
  
  NF=0.0; 
  NI=0.0; 
  NS=0.0; 
  NR=0.0; 
  ]


dom a = { colour = "blue"; bind = ka; unbind = 0.0 }
dom a0 = { colour = "blue"; bind = ka; unbind = 0.0 }
dom b0 = { colour = "blue"; bind = ka; unbind = 0.0 }
dom c0 = { colour = "blue"; bind = ka; unbind = 0.0 }
dom f  = { colour = "green"; bind = kf; unbind = 0.0 }
dom fc  = { colour = "purple"; bind = kf; unbind = 0.0 }
dom xs = { colour = "orange"; bind = kx*ro; unbind = 0.0 }
dom i  = { colour = "orange"}
dom x  = { colour = "orange"; bind = kx; unbind = 0.0; subdomains = [xs;i] }
dom xc  = { colour = "brown"; bind = kx; unbind = 0.0 }
dom y  = { colour = "red"; bind = ky; unbind = 0.0 }
dom y2  = { colour = "red"; bind = ky2; unbind = 0.0 }
dom th = { colour = "purple"; bind = kt; unbind = 0.0 }
dom Q  = { colour = "black" }
dom F  = { colour = "yellow" }
new s

def Fuel() = {f^*}[s]{i^* xs^*>
def Input() = <tether(l) a0^*>[s*]{f^>
def Output() = <tether(l) xs^>{t}[s*]{y^> 
def HP2() = [[ Input() | Output()]]
def Reporter() = {y^*}[s t]{F^}<Q^>
def Strand() = {a0^ s}
( NF * Fuel()
| NI * HP2()
| NR * Reporter()
| NS * Strand()
) """
//  let code = """
//directive localconcentrations 
//	[ (l1,local); (l2,local); (l3,local); (l4,local); (l5,local); (l6,local); (l7,local7); (l,local); (ds,localds)
//	; (lv,local); (vds,localds); (vds3,localds3); (vds4,localds4)
//	; (t1,localt); (t2,localt)]
//directive polymers
//dom a0 = { colour = "blue";   bind = 1.0; unbind = 0.0 }
//dom f  = { colour = "green";  bind = 1.0; unbind = 0.0 }
//dom xs = { colour = "orange"; bind = 1.0; unbind = 0.0 }
//dom y  = { colour = "red";    bind = 1.0; unbind = 0.0 }
//new s 
//([[ <tether(l)>[a0^* s*]::[f^ s]{xs^* s} | {t}<tether(l) xs^>[s*]{y^> ]])
//"""
  let bundle = code |> Dsd.parse
  let crn    = bundle |> Dsd.convert_expand
  Assert.Equal(4, crn.reactions.Length)

/// This example is from Erik Winfree's class at Caltech (Feb 2018)
[<Fact(Skip="this test intermittently fails with a result of 0", DisplayName="Classic DSD - JIT - Cascading")>]
let testJitCascading() =
  let text = """
(* 
    Demonstration of cascading: 
        Out = 1000 if (((#A >= 1) && (#B >= 1)) || (#C >= 1)) && (#D ~<~ 100) and otherwise 0
    where ~<~ means roughly less than, i.e. we're OK if it doesn't work perfectly near 100.
 
    In practice, the system below behaves well unless D is roughly between 95 and 105
*)
directive simulation { final=30000; points=1000; plots = [<a^ A>; <b^ B>; <c^ C>; <d^ D>; (*< _ h^ X>;*) <k^ W>; <4 5^ 2>] }
directive simulator stochastic
directive jit
(* just-in-time network enumeration is primarily useful for polymers, 
but also incidentally turns on the Export/Final panel, which can be nice *)
directive compilation infinite
 
( 0 * <a^ A>  (* input A *)
| 0 * <b^ B>  (* input B *)
| 10 * <c^ C>   (* input C *)
| 95 * <d^ D>  (* input D *)
(* translate A into Y and B into Z while keeping low copy numbers *)
| 1 * {a^*}[A i^]<Y>
| 1 * {b^*}[B j^]<Z>
(* our previous AND gate "from the refrigerator": X = Y AND Z *)
| 1 * {j^*}[Z i^]:[Y h^]<X> 
(* a "wire-OR" via translating C to X *)
| 1 * {c^*}[C h^]<X>
(* a threshold to absorb the first 100 of D and simultaneously deplete W *)
| 200 * {d^*}[D k^]:[W]
| 100 * <k^ W>
| 200 * [D]{k^*}
(* remaining W, if any, can enable X to transform into pre-autocatalyst *)
| 1 * {h^*}[X k^]:[W]<1^>
| 10 * <X k^>
(* translate pre-autocatalyst to the input of an autocatalyst from our "refrigerator" *)
| 1 * <4>[5^ W]{1^*}
(* here is our autocatalyst <4 5^ 2> ; it can be triggered by any <? 4 5^ ?> *)
| 1300 * <5^ 2 3^ 4>
| 1000 * <4>[5^ 2]:<6>[3^ 4]{5^*}
)

"""
  let bundle = Dsd.parse text
  let crn = Dsd.convert_unexpanded bundle
  let plot452idx = crn.settings.simulation.plots.Length - 1
  let calc   = makeDsdCalculus bundle
  
  let jit = match Dsd.to_jit bundle with Choice1Of2 c -> c | _ -> failwith "this test should not produce a Rules bundle"
  let mutable plot452val = 0.0
  let output (r:row) = plot452val <- r.values.[plot452idx]
  let outputplottable (p:newplottable) = ignore p
  Microsoft.Research.CRNEngine.Jit.simulate_callback (ref false) output outputplottable jit calc
  
  (* CS: <4 5^ 2> is expected to reach 1000.0, but I'm checking 
         within the range 100-1000 just to avoid false negatives.
         At the beginning <4 5^ 2> is absent, and it can only be 
         produced in a catalytic loop, so there shouldn't be 
         false positives either. *) 
  Assert.InRange(plot452val, 100.0, 1000.0)

/// Another example from Erik Winfree's class at Caltech (Feb 2018)
[<Fact(DisplayName="Classic DSD - JIT - Stack Machine (LNCS Qian et al. 2011)")>]
let testJitStack() =
  let text = """
directive simulation {
  initial=0; final=10000000; points=1000000; 
  plots=[ 
    sum([<_ T^ pS1>]);  sum([<_ T^ pS2>]);  sum([<_ T^ pS3>]); 
    sum([<_ T^ pS4>]);  sum([<_ T^ pS5>]);  sum([<_ T^ pS6>]); 
    sum([<_ T^ pS7>]);  sum([<_ T^ pS8>]);  sum([<_ T^ pS9>]); 
    sum([<_ T^ pS10>]); sum([<_ T^ pS11>]); sum([<_ T^ pS12>]); 
    sum([<_ T^ pS13>]); sum([<_ T^ pAcc>]); sum([<_ T^ pRej>]);
  ];
}
directive compilation infinite
directive polymers
directive jit
def lots = 20.0
new mBot 
new pBot 
new T 
def EditStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ) = 
  (( lots * <pPushZ T^>
  | lots * {GlueZ T^}
  | lots * <T^>[GlueZ* T^*]<mPopZ>
  | lots * {T^}[mPopZ T^]<pPopZ>:[pPushZ T^]{GlueZ}))
def EmptyStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def OneStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def TwoStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX, mY, pY) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]:<mY mPushZ>[T^ pPushZ]<pY>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def ThreeStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX, mY, pY, mW, pW) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]:<mY mPushZ>[T^ pPushZ]<pY>:[T^ GlueZ*]::[T^* mPopZ]:<mW mPushZ>[T^ pPushZ]<pW>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def FourStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX, mY, pY, mW, pW, mU, pU) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]:<mY mPushZ>[T^ pPushZ]<pY>:[T^ GlueZ*]::[T^* mPopZ]:<mW mPushZ>[T^ pPushZ]<pW>:[T^ GlueZ*]::[T^* mPopZ]:<mU mPushZ>[T^ pPushZ]<pU>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def FiveStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX, mY, pY, mW, pW, mU, pU, mR, pR) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]:<mY mPushZ>[T^ pPushZ]<pY>:[T^ GlueZ*]::[T^* mPopZ]:<mW mPushZ>[T^ pPushZ]<pW>:[T^ GlueZ*]::[T^* mPopZ]:<mU mPushZ>[T^ pPushZ]<pU>:[T^ GlueZ*]::[T^* mPopZ]:<mR mPushZ>[T^ pPushZ]<pR>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def SixStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mX, pX, mY, pY, mW, pW, mU, pU, mR, pR, mP, pP) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mX mPushZ>[T^ pPushZ]<pX>:[T^ GlueZ*]::[T^* mPopZ]:<mY mPushZ>[T^ pPushZ]<pY>:[T^ GlueZ*]::[T^* mPopZ]:<mW mPushZ>[T^ pPushZ]<pW>:[T^ GlueZ*]::[T^* mPopZ]:<mU mPushZ>[T^ pPushZ]<pU>:[T^ GlueZ*]::[T^* mPopZ]:<mR mPushZ>[T^ pPushZ]<pR>:[T^ GlueZ*]::[T^* mPopZ]:<mP mPushZ>[T^ pPushZ]<pP>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def SevenStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mA, pA, mB, pB, mC, pC, mD, pD, mE, pE, mF, pF, mG, pG) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mA mPushZ>[T^ pPushZ]<pA>:[T^ GlueZ*]::[T^* mPopZ]:<mB mPushZ>[T^ pPushZ]<pB>:[T^ GlueZ*]::[T^* mPopZ]:<mC mPushZ>[T^ pPushZ]<pC>:[T^ GlueZ*]::[T^* mPopZ]:<mD mPushZ>[T^ pPushZ]<pD>:[T^ GlueZ*]::[T^* mPopZ]:<mE mPushZ>[T^ pPushZ]<pE>:[T^ GlueZ*]::[T^* mPopZ]:<mF mPushZ>[T^ pPushZ]<pF>:[T^ GlueZ*]::[T^* mPopZ]:<mG mPushZ>[T^ pPushZ]<pG>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def EightStack(mPushZ, pPushZ, mPopZ, pPopZ, GlueZ, mA, pA, mB, pB, mC, pC, mD, pD, mE, pE, mF, pF, mG, pG, mH, pH) = 
  (( <mBot mPushZ>[T^ pPushZ]<pBot>:[T^ GlueZ*]::[T^* mPopZ]:<mA mPushZ>[T^ pPushZ]<pA>:[T^ GlueZ*]::[T^* mPopZ]:<mB mPushZ>[T^ pPushZ]<pB>:[T^ GlueZ*]::[T^* mPopZ]:<mC mPushZ>[T^ pPushZ]<pC>:[T^ GlueZ*]::[T^* mPopZ]:<mD mPushZ>[T^ pPushZ]<pD>:[T^ GlueZ*]::[T^* mPopZ]:<mE mPushZ>[T^ pPushZ]<pE>:[T^ GlueZ*]::[T^* mPopZ]:<mF mPushZ>[T^ pPushZ]<pF>:[T^ GlueZ*]::[T^* mPopZ]:<mG mPushZ>[T^ pPushZ]<pG>:[T^ GlueZ*]::[T^* mPopZ]:<mH mPushZ>[T^ pPushZ]<pH>:[T^ GlueZ*]::[T^* mPopZ]{T^*}:[pPushZ T^]{GlueZ}
  | EditStack(mPushZ,pPushZ,mPopZ,pPopZ,GlueZ)))
def Element(mX, mPushZ, pPushZ, pX) = <mX mPushZ T^ pPushZ pX>
def Signal(mX, pX) = <mX T^ pX>
def Irrev33x33(mX, pX, mY, pY, mA, pA, mB, pB) = 
  (new mA
  ( lots * {T^*}[pX T^]:[pY T^]:[mA T^]<pA>:[mB T^]<pB>
  | lots * <T^ mA T^ mB>))
def Irrev53x35(mX, mPushZ1, pPushZ1, pX, mY, pY, mB, pB, mA, mPushZ2, pPushZ2, pA) = 
  (new mB
  ( lots * {T^*}[pPushZ1 pX T^]:[pY T^]:[mB T^]<pB>:[mA mPushZ2 T^]<pPushZ2 pA>
  | lots * <T^ mB T^ mA mPushZ2>))
def Irrev53x33(mX, mPushZ1, pPushZ1, pX, mY, pY, mA, pA, mB, pB) = 
  (new mA
  ( lots * {T^*}[pPushZ1 pX T^]:[pY T^]:[mA T^]<pA>:[mB T^]<pB>
  | lots * <T^ mA T^ mB>))
def Irrev53x3(mX, mPushZ1, pPushZ1, pX, mY, pY, mA, pA) = 
  (new mA
  ( lots * {T^*}[pPushZ1 pX T^]:[pY T^]:[mA T^]<pA>
  | lots * <T^ mA T^>))
def Irrev33x35(mX, pX, mY, pY, mB, pB, mA, mPushZ2, pPushZ2, pA) = 
  (new mB
  ( lots * {T^*}[pX T^]:[pY T^]:[mB T^]<pB>:[mA mPushZ2 T^]<pPushZ2 pA>
  | lots * <T^ mB T^ mA mPushZ2>))
new mPushA 
new pPushA 
new mPopA 
new pPopA 
new GlueA 
new mPushB 
new pPushB 
new mPopB 
new pPopB 
new GlueB 
new mPushC 
new pPushC 
new mPopC 
new pPopC 
new GlueC 
new mPushX 
new pPushX 
new mPopX 
new pPopX 
new GlueX 
new mZero 
new pZero 
new mOne 
new pOne 
new mS1 
new pS1 
new mS2 
new pS2 
new mS3 
new pS3 
new mS4 
new pS4 
new mS5 
new pS5 
new mS6 
new pS6 
new mS7 
new pS7 
new mS8 
new pS8 
new mS9 
new pS9 
new mS10 
new pS10 
new mS11 
new pS11 
new mS12 
new pS12 
new mS13 
new pS13 
new mAcc 
new pAcc 
new mRej 
new pRej 
( TwoStack(mPushA,pPushA,mPopA,pPopA,GlueA,mZero,pZero,mZero,pZero)
| TwoStack(mPushB,pPushB,mPopB,pPopB,GlueB,mZero,pZero,mZero,pZero)
| OneStack(mPushC,pPushC,mPopC,pPopC,GlueC,mZero,pZero)
| EmptyStack(mPushX,pPushX,mPopX,pPopX,GlueX)
| Signal(mS1,pS1)
| Signal(mPopA,pPopA)
| Irrev53x33(mBot,mPushA,pPushA,pBot,mS1,pS1,mS2,pS2,mPopB,pPopB)
| Irrev53x33(mOne,mPushA,pPushA,pOne,mS1,pS1,mS3,pS3,mPopB,pPopB)
| Irrev53x33(mZero,mPushA,pPushA,pZero,mS1,pS1,mS4,pS4,mPopB,pPopB)
| Irrev53x3(mBot,mPushB,pPushB,pBot,mS2,pS2,mAcc,pAcc)
| Irrev53x3(mOne,mPushB,pPushB,pOne,mS2,pS2,mRej,pRej)
| Irrev53x3(mZero,mPushB,pPushB,pZero,mS2,pS2,mRej,pRej)
| Irrev53x3(mBot,mPushB,pPushB,pBot,mS3,pS3,mRej,pRej)
| Irrev53x33(mOne,mPushB,pPushB,pOne,mS3,pS3,mS5,pS5,mPopC,pPopC)
| Irrev53x33(mZero,mPushB,pPushB,pZero,mS3,pS3,mS6,pS6,mPopC,pPopC)
| Irrev53x3(mBot,mPushB,pPushB,pBot,mS4,pS4,mRej,pRej)
| Irrev53x33(mOne,mPushB,pPushB,pOne,mS4,pS4,mS6,pS6,mPopC,pPopC)
| Irrev53x33(mZero,mPushB,pPushB,pZero,mS4,pS4,mS7,pS7,mPopC,pPopC)
| Irrev53x3(mBot,mPushC,pPushC,pBot,mS5,pS5,mRej,pRej)
| Irrev53x35(mOne,mPushC,pPushC,pOne,mS5,pS5,mS8,pS8,mOne,mPushC,pPushC,pOne)
| Irrev53x35(mZero,mPushC,pPushC,pZero,mS5,pS5,mS9,pS9,mOne,mPushC,pPushC,pOne)
| Irrev53x3(mBot,mPushC,pPushC,pBot,mS6,pS6,mRej,pRej)
| Irrev53x35(mOne,mPushC,pPushC,pOne,mS6,pS6,mS9,pS9,mOne,mPushC,pPushC,pOne)
| Irrev53x35(mZero,mPushC,pPushC,pZero,mS6,pS6,mS10,pS10,mZero,mPushC,pPushC,pZero)
| Irrev53x3(mBot,mPushC,pPushC,pBot,mS7,pS7,mRej,pRej)
| Irrev53x35(mOne,mPushC,pPushC,pOne,mS7,pS7,mS10,pS10,mZero,mPushC,pPushC,pZero)
| Irrev53x35(mZero,mPushC,pPushC,pZero,mS7,pS7,mS11,pS11,mZero,mPushC,pPushC,pZero)
| Irrev33x35(mPopC,pPopC,mS8,pS8,mS13,pS13,mOne,mPushX,pPushX,pOne)
| Irrev33x35(mPopC,pPopC,mS9,pS9,mS12,pS12,mZero,mPushX,pPushX,pZero)
| Irrev33x35(mPopC,pPopC,mS10,pS10,mS13,pS13,mOne,mPushX,pPushX,pOne)
| Irrev33x35(mPopC,pPopC,mS11,pS11,mS12,pS12,mZero,mPushX,pPushX,pZero)
| Irrev33x33(mPopX,pPopX,mS12,pS12,mS1,pS1,mPopA,pPopA)
| Irrev33x33(mPopX,pPopX,mS13,pS13,mS1,pS1,mPopA,pPopA))
"""
  let bundle = Dsd.parse text
  let calc   = makeDsdCalculus bundle
  let jit = match Dsd.to_jit bundle with Choice1Of2 c -> c | _ -> failwith "this test should not produce a Rules bundle"
  let spcount = ref 15;
  let output (row:row) = if row.values.Length <> !spcount then failwith "mismatched row length"
  let outputplottable sp = spcount := !spcount+1
  Microsoft.Research.CRNEngine.Jit.simulate_callback (ref false) output outputplottable jit calc
  
let dsdSemantics = """
(*********************************)
(*********************************)
(*********************************)
  // binding rule
reaction([P], Rate, Q) :-
  P = C [D][D'],
  compl(D, D'),
  bindingRate(D, Rate),
  freshBond(D!i,P),
  Q = C [D!i] [D'!i],
  not ignore(Q).

reaction([P1; P2], Rate, Q) :-
  P1 = C1 [D],
  P2 = C2 [D'],
  compl(D, D'),
  bindingRate(D, Rate),
  freshBond(D!i, P1 | P2),
  Q = C1 [D!i] | C2 [D'!i],
  not ignore(Q),
  not hidden(D!i, Q). // avoids pseudo-knots

(*********************************)
(*********************************)
(*********************************)
// unbinding rule
reaction([P], Rate, Q) :-
  P = C [X!i] [X'!i],
  toehold(X),
  not junction(X!i, P),
  unbindingRate(X, Rate),
  Q = C [X] [X'].

(*********************************)
(*********************************)
(*********************************)
// strand displacement rule
reaction([P], Rate, Q) :-
  P = C [E!j D] [D!i] [D'!i F!k],
  Q = C [E!j D!i] [D] [D'!i F!k],
  Rate is 20,
  junction(E!j, F!k, Q).
  // junction(E!j, Q).

reaction([P], Rate, Q) :-
  P = C [D E!j] [D!i] [F!k D'!i],
  Q = C [D!i E!j] [D] [F!k D'!i],
  Rate is 20,
  junction(E!j, F!k, Q).
  // junction(E!j, Q).
(*********************************)
(*********************************)
(*********************************)
// 4-way branch migration  
//reaction(P, Rate, Q) :-
//  P = C [D!i1] [D'!i1] [D!i2] [D'!i2],
//  Q = C [D!i1] [D'!i2] [D!i2] [D'!i1],
//  Rate is 99, // TODO
//  junction(D!i1, Q),
//  junction(D!i2, Q).

(*********************************)
(*********************************)
(*********************************)
// ignore complexes with more than four strands
ignore(P) :- 
  toList(P, Q),
  length(Q, N),
  N > 4.

junction(E!j,F!j,_).
junction(E!j,F!k,Q):- 
  Q = C [F!k] [G!l E'!j] [E!j],
  junction(G!l,F!k,Q).
  
// Helper functions
(* Encoding of the junction predicate using the path predicate *)
findLoop(E!j, Q, Loop) :-
  Q = C [E!j @ Start] [E'!j @ End],
  path(E!j, Start, End, Q, "any", [], [_ # Loop] (* discards E!j from the loop*)),  
  not (Loop = []).

branchesOnly([]).
branchesOnly([[E!j; _]; [E'!j; _] # Rest]) :-
  branchesOnly(Rest).
  
junction(E!j, Q) :-
  findLoop(E!j, Q, Loop),
  branchesOnly(Loop).
  
hidden(D!i, P) :-
  // TODO: P = C [D!i] [A@X D'!i@Y]
  P = C [D!i] [A @ Start  D'!i @ End],
  path(A, Start, End, P, "left", [], Path),
  not (member(D!i, Path)).

path(_, End, End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X, Start, End, P, "left", Visited, Path) :-
  not (Start = End),
  prevDomain(Start, [Y; Start'], P),
  not member([Y; Start'], Visited),
  path(Y, Start', End, P, "left", [[X; Start] # Visited], Path).

path(X, Start, End, P, "right", Visited, Path) :-
  not (Start = End),
  nextDomain(Start, [Y; Start'], P),
  not member([Y; Start'], Visited),
  path(Y, Start', End, P, "right", [[X; Start]# Visited], Path).
  
path(X, Start, End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X, Start, End, P, "left", Visited, Path).

path(X, Start, End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X, Start, End, P, "right", Visited, Path).

path(D!i, Start, End, P, _, Visited, Path) :-
  not (Start = End),
  P = C [D!i] [D'!i @ Start'],
  not member([D'!i; Start'], Visited),
  path(D'!i, Start', End, P, "any", [[D!i; Start]# Visited], Path).
(*********************************)
(*********************************)
(*********************************)
  """

[<Fact(DisplayName="Logic DSD - Classic semantics - Catalytic")>]
let testClassicCatalytic() =
  let text = sprintf """
directive rules {

bindingRate(c^, 0.00042).
unbindingRate(c^, 0.04).
bindingRate(c^*, 0.00042).
unbindingRate(c^*, 0.04).
bindingRate(e^, 0.00065).
unbindingRate(e^, 0.004).
bindingRate(e^*, 0.00065).
unbindingRate(e^*, 0.004).

%s
}

( 13 [ <b c^ d> ]
| 10 [ <d e^>   ]
| 10 [ <a b!1> | <f c^!2 d!3> | <e^* d*!3 c^*!2 b*!1> ]) 
  """
  let crn = Dsd.compile (text dsdSemantics)
  // let debug1 = crn.to_string ()
  // let debug2 = crn.to_string_structure ()
  ()

[<Fact(DisplayName="Logic DSD - Classic semantics - Binding test")>]
let testBinding () = 
  let code = sprintf """
    directive rules {
bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).
  
displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

slow([P1;P2], 0.00065,Q) :- bind(P1,P2,Q,_).
fast([P],1,Q) :- displace(P,Q,_,_).
fast([P],1,Q) :- displaceL(P,Q,_,_).
fast([P],0.004,Q) :- unbind(P,Q,_).

merge(P,P,V) :- not fast([P],_,_).
merge(P,R,V) :- 
  fast([P],_,Q), not member(Q,V), merge(Q,R,[Q#V]).

reaction([P1;P2],Rate,R) :- 
  slow([P1;P2],Rate,R).
reaction([P],Rate,R) :- 
  fast([P],Rate,R).
}
    ( 1 [ <e^* d*!0 c^*!1 b*!2> | <b!2 c^!1 d!0>]
    | 1 [ <d e^>]
  )"""
  let crn = Dsd.compile code
  let e = Parameters.to_env crn.settings.parameters
  let f = Rate.map2 (Expression.map (Environment.find e)) id
                                                                 
  // let debug1 = crn.to_string ()
  // let debug2 = crn.to_string_structure ()
  
  Assert.Equal(2, crn.reactions.Length) // one reaction binds e^, and one reaction displaces d

  let bRate = Microsoft.Research.CRNEngine.MassAction (Microsoft.Research.CRNEngine.Expression.Float 0.00065)
  let uRate = Microsoft.Research.CRNEngine.MassAction (Microsoft.Research.CRNEngine.Expression.Float 0.004)
  let dRate = Microsoft.Research.CRNEngine.MassAction (Microsoft.Research.CRNEngine.Expression.Float 1.0)
  
  // check that there is a binding rule
  Assert.True(crn.reactions |> List.exists (fun r -> r.reactants.Length = 2 
                                                     && r.products.Length = 1
                                                     && f r.rate = bRate
                                                     && r.reverse |> Option.isSome
                                                     && (r.reverse |> Option.get |> f) = uRate))
  // check that there is a displacement rule
  Assert.True(crn.reactions |> List.exists (fun r -> r.reactants.Length = 1 
                                                     && r.products.Length = 1
                                                     && r.rate = dRate 
                                                     && Option.isSome r.reverse
                                                     && Option.get r.reverse = dRate))// check that 



let simplifiedDsdSemantics extra =
  sprintf """
directive rules {

// binding rule
reaction([P], Rate, Q) :-
  P = C [D][D'],
  Q = C [D!i] [D'!i],
  compl(D, D'),
  bindingRate(D, Rate),
  freshBond(D!i,P).

reaction([P1; P2], Rate, Q) :-
  P1 = C1 [D], P2 = C2 [D'],
  Q = C1 [D!i] | C2 [D'!i],
  compl(D, D'),
  bindingRate(D, Rate),
  freshBond(D!i, P1 | P2).
  
// unbinding rule
reaction([P], Rate, Q) :-
  P = C [D!i] [D'!i],
  Q = C [D] [D'],
  toehold(D),
  not adjacent(D!i,P),
  unbindingRate(D, Rate).

adjacent(D!i,P) :- P = C [E!j D!i].
adjacent(D!i,P) :- P = C [D!i E!j].

// displacement rule
reaction([P], Rate, Q) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j],
  Rate is 20.

reaction([P], Rate, Q) :-
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i],
  Rate is 20.
bindingRate(D, B) :-
  findRate(D, B, _).

unbindingRate(D, U) :-
  findRate(D, _, U).

findRate(D, B, U) :-
  dom(D, B, U).

findRate(D, B, U) :-
  compl(D, D'),
  dom(D', B, U).

%s
}""" extra

[<Fact(DisplayName="Logic DSD - Classic semantics - Join")>]
let testClassicJoin() =
  let rates = """dom(_, "k", "u")."""
  let semantics = simplifiedDsdSemantics rates
  let text = sprintf """
// Simplified DSD semantics
%s

directive simulation { final = 600; }
directive simulator deterministic
directive parameters [ 
  k = 0.003;
  u = 0.1; 
]

( 10 [<tb^ b>]
| 10 [<tx^ x>]
| 100 [<to^*!1 x*!2 tx^*!3 b*!4 tb^*> | <x!2 to^!1> | <b!4 tx^!3>]
| 100 [ <fl^ x!5 > | <to^* x*!5>]
)
  """
  let crn = Dsd.compile (text semantics)
  //let debug1 = crn.to_string ()
  //let debug2 = crn.to_string_structure ()
  Assert.Equal(8, crn.reactions.Length)

[<Fact(DisplayName="Logic DSD - Logic DSD - Pseudoknot fuel")>]
let testPseudoknotFuel() =
  let text = """directive rules {
bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).

unibind(P,Q,_) :-
  P = C [D][D'], compl(D, D'), 
  Q = C [D!i][D'!i], freshBond(D!i, P).
  

displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j]. // [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i]. // [D'!i E'!j].

// branch migration predicates
migrate(P, Q) :-
  P = C [D!i1] [D'!i1 E'!j] [E!j D!i2] [D'!i2],
  Q = C [D!i1] [D'!i2 E'!j] [E!j D!i2] [D'!i1].

migrateR(P, Q) :-
  P = C [D!i1] [E'!j D'!i1] [D!i2 E!j] [D'!i2],
  Q = C [D!i1] [E'!j D'!i2] [D!i2 E!j] [D'!i1].
  

// branch migration predicates
migrateDisplace(P, Q) :-
  P = C [<D!i1] [D'!i1 E'!j] [F'!k E!j D!i2 ] [D'!i2 F!k],
  Q = C [<D]    [D'!i1 E'!j] [F'!k E!j D!i1 ] [D' F!k].

migrateDisplaceR(P, Q) :-
  P = C [D!i1>] [E'!j D'!i1] [D!i2 E!j F'!k] [F!k D'!i2],
  Q = C [D>]    [E'!j D'!i1] [D!i1 E!j F'!k] [F!k D'   ].

//slow([P1;P2], "bind",Q) :- bind(P1,P2,Q,_).
//fast([P],"displace",Q) :- displace(P,Q,_,_).
//fast([P],"displace",Q) :- displaceL(P,Q,_,_).
fast([P],"bind",Q) :- unibind(P,Q,_).
fast([P],"unbind",Q) :- unbind(P,Q,_).
//slow([P],"migrate",Q) :- migrate(P,Q).
//slow([P],"migrate",Q) :- migrateR(P,Q).

slow([P],"migDisplace",Q) :- migrateDisplace(P,Q).
slow([P],"migDisplace",Q) :- migrateDisplaceR(P,Q).

merge(P,P,V) :- not fast([P],_,_).
merge(P,R,V) :- fast([P],_,Q), not member(Q,V), merge(Q,R,[Q#V]).

reaction([P1;P2],Rate,R) :- slow([P1;P2],Rate,Q), merge(Q,R,[(P1|P2);Q]).
reaction([P],Rate,R) :-     slow([P],Rate,Q), merge(Q,R,[P;Q]).

}

 // (10 [<r g!0> | <g*!1 r*> | <g*!0 b*!2> | <t^*> | <t^ b!2 g!1>])
 // (10 [<t^*!0> | <r g!1> | <g*!2 r*> | <g*!1 b*!3> | <t^!0 b!3 g!2>]) 
 (10 [<g*!0 r*> | <g*!1 b*!2 r*!3> | <r*!4 t^*!5> | <r!3 g!1> | <t^!5 r!4 b!2 g!0>])
(*
(10 [<t^ r!0 b!2 g!1> | <g*!3 b*!2 r*!4> | <r!4 g!3 > | <g*!1 r*!0>]
| 1 [<r* t^*>]
)*)"""
  let crn = Dsd.compile text
  () //Assert.Equal(8, crn.reactions.Length)


[<Fact(DisplayName="Logic DSD - zero size hairpin displacement")>]
let testZeroHairpin() =
  let text = """directive rules {

bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D@X], compl(D, D'), P2 = C2 [D'@Y], 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2),
  not hidden(D@X, P1),
  not hidden(D'@Y, P2).

bind(P,Q,D!i) :- 
  P = C[D @ X][D' @ Y], compl(D, D'),
  Q = C [D!i][D'!i], freshBond(D!i, P),
  not hidden(D  @ X,  P),
  not hidden(D' @ Y,  P).
  
displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!k], junction(E!j, E'!k, P),
  Q = C [E!j D!i] [D] [D'!i E'!k].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!k D'!i], junction(E!j, E'!k, P),
  Q = C [D] [D!i E!j] [E'!k D'!i].

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
 

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).

binds(P,R,D!i,[D#L]) :- bind(P,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P,Q,D!i,[D])   :- bind(P,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).

displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displace(Q,_,D!i,_).

displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displaceL(Q,_,D!i,_).

covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not cover(Q,_,D!i,_).

coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coverL(Q,_,D!i,_).

unbinds(P,R,D!i,[D#L]) :- 
  P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
  Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).

(* auxiliary functions *)
hidden(D@X, P) :-
  unbound(D),
  P = C [A@End D@X B@Start], 
  path(B@Start, A@End, P, "right", [], Path).

hidden(D@X, P) :-
  unbound(D),
  P = C [B@Start D@X A@End], 
  path(B@Start, _@End, P, "left", [], Path).

hidden(D!i, D'!i, P) :-
  P = C [D!i@End A@Start],
  path(A@Start, _@End, P, "right", [], Path),
  not (last(Path, D'!i)).

hidden(D!i, D'!i, P) :-
  P = C [A@Start D!i@End],
  path(A@Start, _@End, P, "left", [], Path),
  not (last(Path, D'!i)).

(* path predicate: find a @Path from @Start to @End *)
path(_@End, _@End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X@Start, _@End, P, "left", Visited, Path) :-
  not (Start = End),
  P = C[Y@Start' X@Start],
  not member(Y@Start', Visited),
  path(Y@Start', _@End, P, "left", [X@Start # Visited], Path).

path(X@Start, _@End, P, "right", Visited, Path) :-
  not (Start = End),
  P = C[X@Start Y@Start'],
  not member(Y@ Start', Visited),
  path(Y@Start', _@End, P, "right", [X@Start # Visited], Path).
  
path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "left", Visited, Path).

path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "right", Visited, Path).

path(D!i@Start, _@End, P, _, Visited, Path) :-
  not (Start = End),
  P = C [D!i@Start] [D'!i@Start'],
  not member(D'!i@Start', Visited),
  path(D'!i@Start', _@End, P, "any", [D!i@Start # Visited], Path).

junction(A, B, P) :- junctionR(A, B, P, []).
junction(A, B, P) :- junctionL(A, B, P, []).
junctionR(_!j,_!j,_,_). 
junctionR(E!j,F!k,Q,V):- 
  Q = C [F!k] [G!l@X E'!j@Y] [E!j],
  not member(X, V),
  not member(Y, V),
  junctionR(G!l,F!k,Q,[X;Y#V]).

junctionL(_!j,_!j,_, _).
junctionL(E!j,F!k,Q, V):- 
  Q = C [F!k] [E'!j@X G!l@Y] [E!j],
  not member(X, V),
  not member(Y, V),
  junctionL(G!l,F!k,Q,[X;Y#V]).


(* infinite semantics *)
find(D,Type,Rate):- rate(D,Type,Rate).
find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).

//reaction([P1; P2], Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), productive(Q,_,D!i,L).
//reaction([P1; P2], Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), not unbind(Q, _, D!i, _).

reaction([P], "displace", Q) :- displace(P,Q,_,_).//, find(L, "displace", Rate).
reaction([P], "displace", Q) :- displaceL(P,Q,_,_).//, find(L, "displace", Rate).
reaction([P], "covers", Q) :- cover(P,Q,_,_).//, find(L, "cover", Rate).
reaction([P], "covers",Q) :- coverL(P,Q,_,_).//, find(L, "cover", Rate).
reaction([P], "unbind",Q) :- unbind(P,Q,_).//, find(L, "unbind", Rate).
reaction([P], "bind", Q) :- bind(P,Q,_).

productive(P,Q,E!j,L) :- displace(P,Q,E!j,L).
productive(P,Q,E!j,L) :- displaceL(P,Q,E!j,L).
productive(P,Q,E!j,L) :- cover(P,Q,E!j,L).
productive(P,Q,E!j,L) :- coverL(P,Q,E!j,L).

merge(P,P,V) :- not fast(P,_).
merge(P,R,V) :- fast(P,Q), not member(Q,V), merge(Q,R,[Q#V]).

reaction([P1; P2], "bind", R) :- bind(P1, P2, R, _). // , merge(Q,R,[(P1|P2);Q]).

default([_],"unbind",0.1).
default(_,"bind","bind").
default(_,"displace",1.0).
default(_,"cover",1.0).

}

( 
10 [<a^ b c>] |
100 
[<b!1 c!2>
| 
<c*!2 d>
| 
<d!3 e!4 z
 e*!4 d*!3 b*!1
 a^*>] 
)

"""
  let crn = Dsd.compile text
  Assert.Equal(9, crn.reactions.Length)


[<Fact(DisplayName="Logic DSD - Printing - cas9")>]
let testCas9() =
  // this code works if [A; P] is replaced by [P; A]
  let code = """directive rules{
reaction([P1; P2], 1, Q) :-
                member(A, [P1; P2]),
                member(P, [P1; P2]),
                not(A = P),
                A = D [<X YPam XPam Y>],
                P = C [X!i YPam!j      XPam!k Y!l] [Y'!l XPam'!k      YPam'!j X'!i ],
                Q = C [X!i YPam!j> | < XPam!k Y!l] [Y'!l XPam'!k > | <YPam'!j X'!i ].
}
( 1 [ <a bPam aPam b> ]
| 1 [ < a !0 bPam !1 aPam !2 b !3> 
    | < b*!3 aPam*!2 bPam*!1 a*!0> ] )"""
  let crn = Dsd.compile code
  //let debug1 = crn.to_string ()
  //let debug2 = crn.to_string_structure ()
  Assert.False(crn.reactions.IsEmpty)

[<Fact(DisplayName="Logic DSD - PEN toolbox - Oligator")>]
let testPenToolboxOligator() =
  let code = """
directive rules {
bind1(P,Q,D!i) :- 
  P = C [D][D'], compl(D, D'), 
  Q = C [D!i] [D'!i], freshBond(D!i,P).

bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].
displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displaces(Q,_,D!i,_).

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].
displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displacesL(Q,_,D!i,_).

reaction([P], Rate, Q) :- bind1(P,Q,D!i), bindingRate(D, Rate).
reaction([P1; P2], Rate, Q1|Q2) :- bind(P1,P2,Q1,Q2,D!i), bindingRate(D, Rate).
reaction([P], Rate, Q) :- unbind(P,Q,D!i), unbindingRate(D, Rate).
reaction([P], "migrate", Q) :- displaces(P,Q,E!j,_).
reaction([P], "migrate", Q) :- displacesL(P,Q,E!j,_).

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).
covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not covers(Q,_,D!i,_).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).
coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coversL(Q,_,D!i,_).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).

unbind2(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'].
unbinds(P,R,D!i,[D#L]) :- unbind2(P,Q,D!i), not boundL(D!i,_,P), unbinds(Q,R,E!j,L), adjacent(D!i,E!j,P). 
unbinds(P,Q,D!i,[D])   :- unbind2(P,Q,D!i), not adjacent(D!i,_,P).
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

reaction([P1; P2], Rate, Q) :- bindingRates(L, Rate), binds(P1,P2,Q,_,L).
reaction([P], Rate, Q) :- unbindingRates(L, Rate), unbinds(P,Q,D!i,L).

polymerase(P,Q,A!i,B!j) :- 
  P = C [A!i>]     [B'   A'!i],
  Q = C [A!i B!j>] [B'!j A'!i], compl(B, B'), freshBond(B!j, P).

polymerase(P,Q,A!i,B!j) :- //strand displacing polymerase
  P = C [A!i>]     [B'!j A'!i] [B!j],
  Q = C [A!i B!j>] [B'!j A'!i] [B].

polymerases(P,R,A!i) :- polymerase(P,Q,A!i,B!j), polymerases(Q,R,B!j).
polymerases(P,Q,A!i) :- polymerase(P,Q,A!i,B!j), not polymerases(Q,_,_).

exonuclease(P,R,A@p) :- P = C[<A@p B@q], unbound(A), Q = C[<B@p], not protected(A), exonuclease(Q,R,B@q).
exonuclease(P,Q,A@p) :- P = C[<A@p>] , unbound(A), Q = C[nil], not protected(A).

reaction([P], "kpol", Q) :- polymerases(P,Q,_).

reaction([P], "knick", Q) :- // nickase
  P = C [A1!i1 A2!i2 B!j] [B'!j A2'!i2 A1'!i1],
  Q = C [A1!i1 A2!i2> | <B!j] [B'!j A2'!i2 A1'!i1],
  recognition(A1,A2).

reaction([P], "kexo", Q) :- exonuclease(P,Q,_).

protected(b2^*).
protected(a2^*).
protected(x^*).
recognition(a1^,a2^).
recognition(b1^,b2^).
bindingRates([_; _], "kbind").
bindingRates([_; _; _], "kbind").
unbindingRates([_; _], "kunbind").
unbindingRates([_; _; _], "kunbind").
unbindingRate(_, "kunbind").
}
directive parameters [
  kpol = 1;
  kbind = 0.003;
  migrate = 1;
  knick = 1;
  kexo = 1; 
]

( 5 [<b2^* b1^* a2^* a1^*>]
| 30 [<a2^* a1^* a2^* a1^*>]
| 30 [<x^* a1^* a2^* b2^* b1^*>]
| 1 [<a1^ a2^>]
| 1 [<b1^ b2^>]
| 1 [<a2^ a1^ x^>]
)"""
  let crn = Dsd.compile code
  //let debug1 = crn.to_string ()
  //let debug2 = crn.to_string_structure ()
  ()

[<Fact(DisplayName="Logic DSD - PEN toolbox - Oligator (nucleotides)")>]
let testPenToolboxOligatorNucleotides() =
  let code = """
directive rules {
unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).
  
displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not covers(Q,_,D!i,_).

displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displaces(Q,_,D!i,_).

displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displacesL(Q,_,D!i,_).

covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not covers(Q,_,D!i,_).

coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coversL(Q,_,D!i,_).

unbinds(P,R,D!i,[D#L]) :- 
  P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
  Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).

find(D,Type,Rate):- rate(D,Type,Rate).
find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).

reaction([P1; P2], Rate, Q) :- find(L, "bind", Rate), binds(P1,P2,Q,_,L).
reaction([P], Rate, Q) :- displaces(P,Q,_,L), find(L, "displace", Rate).
reaction([P], Rate, Q) :- displacesL(P,Q,_,L), find(L, "displace", Rate).
reaction([P], Rate, Q) :- covers(P,Q,_,L), find(L, "cover", Rate).
reaction([P], Rate, Q) :- coversL(P,Q,_,L), find(L, "cover", Rate).
reaction([P], Rate, Q) :- unbinds(P,Q,_,L), find(L, "unbind", Rate).

polymerase(P,Q,A!i,B!j) :- 
  P = C [A!i>]     [B'   A'!i], compl(B, B'), 
  Q = C [A!i B!j>] [B'!j A'!i], freshBond(B!j, P).

polymerase(P,Q,A!i,B!j) :- //strand displacing polymerase
  P = C [A!i>]     [B'!j A'!i] [B!j],
  Q = C [A!i B!j>] [B'!j A'!i] [B].

polymerases(P,R,A!i,[B#L]) :- polymerase(P,Q,A!i,B!j), polymerases(Q,R,B!j,L).
polymerases(P,Q,A!i,[B]) :- polymerase(P,Q,A!i,B!j), not polymerase(Q,_,_,_).

exonuclease(P,R,A@p,[A#L]) :- 
  P = C[<A@p B], unbound(A), Q = C[<B@q], not protected(A), exonuclease(Q,R,B@q,L).
exonuclease(P,Q,A@p,[A]) :- P = C[<A@p>] , unbound(A), Q = C[nil], not protected(A).
protected(_{"O"}).

reaction([P], Rate, Q) :- polymerases(P,Q,_,L), find(L,"polymerase", Rate).
reaction([P], Rate, Q) :- exonuclease(P,Q,_,L), find(L,"exonuclease",Rate).
reaction([P], Rate, Q) :- 
  recognition([A1;A2;A3;A4;A5]), find([A1;A2;A3;A4;A5],"nickase",Rate),
  P = C [A1!i1 A2!i2 A3!i3 A4!i4 A5!i5 N1 N2 N3 N4 B!j]     [B'!j N4' N3' N2' N1' A5'!i5 A4'!i4 A3'!i3 A2'!i2 A1'!i1],
  Q = C [A1!i1 A2!i2 A3!i3 A4!i4 A5!i5 N1 N2 N3 N4> | <B!j] [B'!j N4' N3' N2' N1' A5'!i5 A4'!i4 A3'!i3 A2'!i2 A1'!i1].

default([_],"unbind",0.1).
//default(_,"bind",0.0003).
//Require at least two consecutive domains to bind, to prevent polymers
default([_; _; _; _; _; _; _; _; _; _ ; _ # L ],"bind",0.0003). 
default(_,"displace",1.0).
default(_,"cover",1.0).
default(_,"polymerase",0.2833).
default(_,"exonuclease",0.0053).
default(_,"nickase",0.05).

recognition([c^*; a^; c^*; a^*; c^]).
alpha([a^*; c^; c^*; a^; c^*; a^*; c^; a^*; c^*; a^*; a^*]).
beta([a^; a^*; c^*; a^; c^*; a^*; c^; a^; a^*; c^*; c^]).
inhibitor([a^; c^*; a^*; c^; a^*; c^*; a^*; a^*; a^*; c^; c^*; a^; c^*; a^*; a^; a^]).
inhibit([a^; c^*; a^*; c^; a^*; c^*; a^*; a^*; a^*; c^; c^*; a^; c^*; a^*]).

rate(L,"polymerase", "kIpol") :- inhibitor(L).
rate(L,"polymerase", "kpol") :- alpha(L).
rate(L,"polymerase", "kpol") :- beta(L).
rate(L,"nickase", "knick") :- recognition(L).
rate(L,"exonuclease", "kexoa") :- alpha(L).
rate(L,"exonuclease", "kexob") :- beta(L).
rate(L,"exonuclease", "kexoInh") :- inhibitor(L).
rate(L, "unbind", "kda") :- alpha(L).
rate(L, "unbind", "kdb") :- beta(L).
rate(L, "unbind", "kdInhTthree") :- inhibitor(L).
rate(L, "unbind", "kdInhTone") :- inhibit(L).
rate([_; _; _; _; _; _; _; _; _; _ ; _ # L ],"bind","ka").
}
directive parameters [
  ka = 4.3333e-04;
  kda = 0.0383;
  kdb = 0.0135;
  kdInhTone = 9.5e-5;
  kdInhTthree = 3.5e-5;
  kpol = 0.2833;
  kIpol = 0.1150;
  knick = 0.05;
  kexoa = 0.0053;
  kexob = 0.0062;
  kexoInh = 0.02;
]
directive simulation { final=100000 }
directive deterministic { stiff=true }
directive simulator deterministic
(  5 [<c^*{"O"} c^ a^ a^* c^* a^ c^ a^* c^ a^ a^*    a^ a^ c^ a^ c^* a^ c^ a^* c^ c^* a^>]
| 30 [<a^{"O" } a^ c^ a^ c^* a^ c^ a^* c^ c^* a^    a^ a^ c^ a^ c^* a^ c^ a^* c^ c^* a^>] 
| 30 [<a^*{"O" } a^* a^ c^ a^* c^ c^* a^ a^ a^ c^ a^ c^* a^ c^ a^*   c^* c^ a^ a^* c^* a^ c^ a^* c^ a^ a^*>]
| 1 [<a^* c^ c^* a^ c^* a^* c^ a^* c^* a^* a^* >] // A
| 1 [<a^ a^* c^* a^ c^* a^* c^ a^ a^* c^* c^ >]  // B
| 1 [<a^ c^* a^* c^ a^* c^* a^* a^* a^* c^ c^* a^ c^* a^* a^ a^>] // Inh
)
"""
  let crn = Dsd.compile code
  //let debug1 = crn.to_string ()
  //let debug2 = crn.to_string_structure ()
  ()

[<Fact(DisplayName="Logic DSD - PEN toolbox - Polymerase")>]
let polyTest  () = 
  let code = """
  directive rules {
  // polymerase
  reaction([P], Rate, Q) :-
    P = C [<A!i>]     [<B'   A'!i>],
    compl(A, A'),
    compl(B, B'),
    freshBond(B!j, P),
    Q = C [<A!i B!j>] [<B'!j A'!i>],
    Rate is 1.
  }
  (1 [<b^* a^*!0> | <a^!0>] )
  """
  let crn = Dsd.compile code
  Assert.False(crn.reactions.IsEmpty)

[<Fact(DisplayName="Logic DSD - PEN toolbox - Nickase")>]
let nickTest  () = 
  let code = """
  directive rules {
  // nickase
  reaction([P], Rate, Q) :-
    P = C [a1^!i1 a2^!i2 B!j] [B'!j a2^*!i2 a1^*!i1],
    Q = C [a1^!i1 a2^!i2> | <B!j] [B'!j a2^*!i2 a1^*!i1],
    //not complementary(A),
    Rate is 1. // nick the template only

  }
  (1 [ <a1^!1 a2^!2 b1^!3> | <b2^* b1^*!3 a2^*!2 a1^*!1>] )
  """
  let crn = Dsd.compile code
  Assert.False(crn.reactions.IsEmpty)



(***********************************)
(***********************************)
(***********************************)
(* Localised semantics and walkers *)
(***********************************)
(***********************************)
(***********************************)

let localisedSemantics = """
(****** Localised semantics ********)
  (*** tethers(P, L): find all the tethers L in process P ***)
  filterTethers([], []).
  filterTethers([T#Ds], [Ls#Ts]) :-
    tethered(T, Ls),
    filterTethers(Ds, Ts).
  
  filterTethers([D#Ds], Ts) :-
    not tethered(D, _),
    filterTethers(Ds, Ts).
  
  tethers(P, Ts) :-
    toDomains(P, Ds),        // in-built predicate
    filterTethers(Ds, Ts).

  (*** tetheredDomains(P, TDs) :  find all tethered domains in P ***)
  filterTetheredDomains([], []).
  filterTetheredDomains([D#Ds], [D#Ts]) :-
    tethered(D, _),
    filterTetheredDomains(Ds, Ts).
  
  filterTetheredDomains([D#Ds], Ts) :-
    not tethered(D, _),
    filterTetheredDomains(Ds, Ts).
  
  tetheredDomains(P, TDs) :-
    toDomains(P, Ds),                 // in-built predicate
    filterTetheredDomains(Ds, TDs).

  (*** proximal(P1, P2):  P1 and P2 are close enough if either one of them is not tethered, 
                          or if they are tethered in the same region. ***)
  hasIntersection(Ts1, Ts2) :- 
  	concat(Ts1, T1),
    concat(Ts2, T2),
    not intersection(T1, T2, []).
    
  shared([], _).
  shared(_, []).
  shared(Ts1, Ts2) :-
    not (Ts1 = []), not (Ts2 = []),
    hasIntersection(Ts1, Ts2).

  proximal(P1, P2) :-
    tethers(P1, Ts1), 
    tethers(P2, Ts2), 
    shared(Ts1, Ts2),
    
    (* CRN pruning: bind P1 and P2 only if they have a most one tether each. 
                    Tethers are also unique, so discard P1 and P2 if they both share a tether *)
    tetheredDomains(P1, D1), length(D1, N1), N1 <= 1,
    tetheredDomains(P2, D2), length(D2, N2), N2 <= 1,
    intersection(D1, D2, []).
  
  bind(P1,P2,Q,D!i) :- 
    proximal(P1, P2),
    P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
    Q  = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).

  (****** /Localised semantics ********)
  

  bind1(P,Q,D!i) :- 
    P = C [D][D'], compl(D, D'), 
    Q = C [D!i] [D'!i], freshBond(D!i,P).

  unbind(P,Q,D!i) :-
    P = C [D!i] [D'!i], toehold(D), 
    Q = C [D] [D'], not adjacent(D!i,_,P).

  adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
  adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

  displace(P,Q,E!j,D!i) :-
    P = C [E!j D] [D!i] [D'!i E'!j],
    Q = C [E!j D!i] [D] [D'!i E'!j].
  displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
  displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displaces(Q,_,D!i,_).

  displaceL(P,Q,E!j,D!i) :- 
    P = C [D!i] [D E!j] [E'!j D'!i],
    Q = C [D] [D!i E!j] [E'!j D'!i].
  displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
  displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displacesL(Q,_,D!i,_).

  reaction([P], Rate, Q) :- bind1(P,Q,D!i), bindingRate(D, Rate).
  reaction([P1; P2], Rate, Q) :- bind(P1,P2,Q,D!i), bindingRates(D, Rate).
  reaction([P], Rate, Q) :- unbind(P,Q,D!i), unbindingRate(D, Rate).
  reaction([P], "migrate", Q) :- displaces(P,Q,E!j,_).
  reaction([P], "migrate", Q) :- displacesL(P,Q,E!j,_).

  cover(P,Q,E!j,D!i) :-
    P = C [E!j D] [D' E'!j], compl(D, D'), 
    Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).
  covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
  covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not covers(Q,_,D!i,_).

  coverL(P,Q,E!j,D!i) :- 
    P = C [D E!j] [E'!j D'], compl(D, D'), 
    Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).
  coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
  coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coversL(Q,_,D!i,_).

  binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).

  unbind2(P,Q,D!i) :-
    P = C [D!i] [D'!i], toehold(D), 
    Q = C [D] [D'].
  unbinds(P,R,D!i,[D#L]) :- unbind2(P,Q,D!i), not boundL(D!i,_,P), unbinds(Q,R,E!j,L), adjacent(D!i,E!j,P). 
  unbinds(P,Q,D!i,[D])   :- unbind2(P,Q,D!i), not adjacent(D!i,_,P).
  boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

  reaction([P1; P2], Rate, Q) :- bindingRates(L, Rate), binds(P1,P2,Q,_,L).
  reaction([P], Rate, Q) :- unbindingRates(L, Rate), unbinds(P,Q,D!i,L).


  // nickase
  reaction([P], "knick", Q) :- 
    P = C [A1!i1 A2!i2 B!j] [B'!j A2'!i2 A1'!i1],   recognition(A1',A2'),
    Q = C [A1!i1 A2!i2 B!j] [B'!j> | <A2'!i2 A1'!i1].
"""

[<Fact(DisplayName="Logic DSD - Localised - Walker (Wickham'11)")>]
let walkerTest  () = 
  let code = localisedSemantics |> sprintf "directive rules {
  %s

  // nicks after b* a^* (e.g. nicks X off in <X b* a^*>)
  recognition(a^*, b*).

  // rates
  bindingRates(_, \"kbind\").
  unbindingRate(_, \"kunbind\").
  
  tethered(tA,[\"a\"]).
  tethered(tB,[\"a\";\"b\"]).
  tethered(tC,[\"b\"]).
  
}

directive parameters [
  kbind   = 0.003;
  kunbind = 0.1;
  migrate = 1;
  knick   = 1;
]

( 
//  1 [< a^ !0 b !1 c^ !2>
//    |< c^*!2 b*!1 a^*!0 tA>]
//| 1 [< c^* b* a^* tB>]
//| 1 [< c^* b* a^* tC>]
//)
  1 [ <a^!0 b!1 c^> | <b*!1 a^*!0 tA> ]
| 1 [ <c^* b* a^* tB> ]
)" 
  let crn = Dsd.compile code
  let debug1 = crn.to_string ()
  let debug2 = crn.to_string_structure ()
  Assert.False(crn.reactions.IsEmpty)


[<Fact(DisplayName="Logic DSD - Localised - A cargo-sorting DNA robot (Thubagere et al., Science'17)")>]
// this example models the reference 
let cargoSortingTest  () = 
  let code = "directive rules {\n" + localisedSemantics + """
  
  // rates
  bindingRates(_, "kbind").
  unbindingRate(_, "kunbind").
  
  (* robot starting point: reaches the drop zone for the first cargo, and the cargo pickup area *)
  tethered(start,["dropZoneA"; "toCargo"]).
  
  (* tracks between starting point, cargos pickup area and drop zones *)
  tethered(track1,["toCargo"; "transitA"]).
  tethered(track2,["transitA";"pickupArea"; "transitB"]).
  tethered(track3,["transitB"; "dropZoneB"]).
  
  (* cargo dropping points *)
  tethered(goal1p,["dropZoneA"]).
  tethered(goal2p,["dropZoneB"]).
  
  (* cargo starting points *)
  tethered(cargo1p*,["pickupArea"; "transitA"]).
  tethered(cargo2p*,["pickupArea"; "transitB"]).
  
  }

  directive simulation {
    final = 60000; 
  }

  directive parameters [
    kbind   = 0.003;
    kunbind = 0.1;
    migrate = 1;
  ]

  ( 
  // robot
    1 [ <hand^ arm foot^ leg!0 foot^!1> | <start*!3 foot^*!1 leg*!0> | <start!3> ]
  // goals
  | 1 [ <hand^ arm cargo1^* goal1p*!0> | <goal1p!0> ] 
  | 1 [ <hand^ arm cargo2^* goal2p*!0> | <goal2p!0> ]

  // cargos
  | 1 [ <cargo1^ arm*!0 hand^* cargoA> | <arm!0 cargo1p!1> | <cargo1p*!1> ]
  | 1 [ <cargo2^ arm*!0 hand^* cargoB> | <arm!0 cargo2p!1> | <cargo2p*!1> ]
  //tracks  
  | 1 [ <track1 leg* foot^*> ]
  | 1 [ <track2 foot^* leg*> ]
  | 1 [ <track3 leg* foot^*> ]
  )""" 
  
  // run the simulation and check that the cargos are sorted
  let crn = Dsd.compile code
  let debug1 = crn.to_string ()
  let debug2 = crn.to_string_structure ()
  let crn' = crn.to_ssa().simulate_callback (ref false) ignore ignore None
  
  // expected final states
  let provider   = RulesDSD.Syntax.makeProvider ()
  let siteParser = Microsoft.Research.DNA.LogicDSD.psite cle provider true
  let canonical = Parser.from_string (RulesDSD.Parser.psystem siteParser)>> (RulesDSD.Syntax.Process.Canonical Microsoft.Research.DNA.LogicDSD.engine)>> (RulesDSD.Syntax.Process.ToStringWith Microsoft.Research.DNA.LogicDSD.engine)
  let final1 = canonical "<hand^!2 arm!3 cargo1^*!4 goal1p*!0> | <goal1p!0> | <cargo1^!4 arm*!3 hand^*!2 cargoA>"
  let final2 = canonical "<hand^!2 arm!3 cargo2^*!4 goal2p*!0> | <goal2p!0> | <cargo2^!4 arm*!3 hand^*!2 cargoB>"
  let findSpeciesName x = crn.attributes 
                          |> Map.toList 
                          |> List.find (fun (_, attr) -> attr.structure = x)
                          |> fst
  let sp1 = crn.initials |> List.find (fun x -> x.species.name = findSpeciesName final1)
  let sp2 = crn.initials |> List.find (fun x -> x.species.name = findSpeciesName final2)

  let i1 = crn'.simulator.populations.species_to_index.[sp1.species]
  let i2 = crn'.simulator.populations.species_to_index.[sp2.species]

  let pop1 = crn'.simulator.populations.get_population i1
  let pop2 = crn'.simulator.populations.get_population i2

  Assert.True( (pop1 = 1.0) )
  Assert.True( (pop2 = 1.0) )


[<Fact(DisplayName="Logic DSD - Localised - cargo binding")>]
// this example models the reference 
let cargoBindingTest  () = 
  let code = "directive rules {\n" + localisedSemantics + """
  
  // rates
  bindingRates(_, "kbind").
  unbindingRate(_, "kunbind").
  
  (* robot starting point: reaches the drop zone for the first cargo, and the cargo pickup area *)
  tethered(start,["dropZoneA"; "toCargo"]).
  
  (* tracks between starting point, cargos pickup area and drop zones *)
  tethered(track1,["toCargo"; "transitA"]).
  tethered(track2,["transitA";"pickupArea"; "transitB"]).
  tethered(track3,["transitB"; "dropZoneB"]).
  
  (* cargo dropping points *)
  tethered(goal1p,["dropZoneA"]).
  tethered(goal2p,["dropZoneB"]).
  
  (* cargo starting points *)
  tethered(cargo1p*,["pickupArea"; "transitA"]).
  tethered(cargo2p*,["pickupArea"; "transitB"]).
  
  }

  directive simulation {
    final = 60000; 
  }

  directive parameters [
    kbind   = 0.003;
    kunbind = 0.1;
    migrate = 1;
  ]

  ( 
   1 [ <cargo2 arm*!1 hand^*!0 cargoB> | <track1 leg*!2 foot^*!3> | <arm!1 cargo1p!4> | <cargo1p*!4> | <hand^!0 arm foot^!3 leg!2 foot^> ]
  )""" 

  let crn = Dsd.compile code
  //let debug1 = crn.to_string ()
  //let debug2 = crn.to_string_structure ()
  Assert.True(crn.reactions |> List.exists (fun r -> r.rate = MassAction (Expression.Key "migrate")))


[<Fact(DisplayName="Logic DSD - Example - Ribocomputing with toehold switches")>]
// this example models the reference 
let testRiboSwitch() = 

  let code = """
directive rules {

(* Strand graph semantics*)
hidden(D@X, P) :-
  unbound(D),
  P = C [A@End D@X B@Start], 
  path(B@Start, _@End, P, "right", [], Path).

hidden(D@X, P) :-
  unbound(D),
  P = C [B@Start D@X A@End], 
  path(B@Start, _@End, P, "left", [], Path).

hidden(D!i, D'!i, P) :-
  P = C [D!i@End A@Start],
  path(A@Start, _@End, P, "right", [], Path),
  not (last(Path, D'!i)).

hidden(D!i, D'!i, P) :-
  P = C [A@Start D!i@End],
  path(A@Start, _@End, P, "left", [], Path),
  not (last(Path, D'!i)).

path(_@End, _@End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X@Start, _@End, P, "left", Visited, Path) :-
  not (Start = End),
  P = C[Y@Start' X@Start],
  not member(Y@Start', Visited),
  path(Y@Start', _@End, P, "left", [X@Start # Visited], Path).

path(X@Start, _@End, P, "right", Visited, Path) :-
  not (Start = End),
  P = C[X@Start Y@Start'],
  not member(Y@ Start', Visited),
  path(Y@Start', _@End, P, "right", [X@Start # Visited], Path).
  
path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "left", Visited, Path).

path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "right", Visited, Path).

path(D!i@Start, _@End, P, _, Visited, Path) :-
  not (Start = End),
  P = C [D!i@Start] [D'!i@Start'],
  not member(D'!i@Start', Visited),
  path(D'!i@Start', _@End, P, "any", [D!i@Start # Visited], Path).

junction(A, B, P) :- junctionR(A, B, P).
junction(A, B, P) :- junctionL(A, B, P).
junctionR(_!j,_!j,_). 
junctionR(E!j,F!k,Q):- 
  Q = C [F!k] [G!l E'!j] [E!j],
  junctionR(G!l,F!k,Q).

junctionL(_!j,_!j,_).
junctionL(E!j,F!k,Q):- 
  Q = C [F!k] [E'!j G!l] [E!j],
  junctionL(G!l,F!k,Q).

bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], compl(D, D'), P2 = C2 [D'], 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2),
  not hidden(D!i,  D'!i, Q),
  not hidden(D'!i, D!i,  Q).

displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!k], junction(E!j, E'!k, P),
  Q = C [E!j D!i] [D] [D'!i E'!k].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!k D'!i], junction(E!j, E'!k, P),
  Q = C [D] [D!i E!j] [E'!k D'!i].
(*/ Strand graph semantics*)

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
 

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).

displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displace(Q,_,D!i,_).

displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displaceL(Q,_,D!i,_).

covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not cover(Q,_,D!i,_).

coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coverL(Q,_,D!i,_).

unbinds(P,R,D!i,[D#L]) :- 
  P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
  Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).

find(D,Type,Rate):- rate(D,Type,Rate).
find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).

slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), productive(Q,_,D!i,L).
slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), not unbind(Q, _, D!i, _).
fast(P, Q) :- displaces(P,Q,_,L).//, find(L, "displace", Rate).
fast(P, Q) :- displacesL(P,Q,_,L).//, find(L, "displace", Rate).
fast(P, Q) :- covers(P,Q,_,L).//, find(L, "cover", Rate).
fast(P, Q) :- coversL(P,Q,_,L).//, find(L, "cover", Rate).
fast(P, Q) :- unbinds(P,Q,_,L).//, find(L, "unbind", Rate).

productive(P,Q,E!j,L) :- displace(P,Q,E!j,L).
productive(P,Q,E!j,L) :- displaceL(P,Q,E!j,L).
productive(P,Q,E!j,L) :- cover(P,Q,E!j,L).
productive(P,Q,E!j,L) :- coverL(P,Q,E!j,L).

merge(P,P,V) :- not fast(P,_).
merge(P,R,V) :- fast(P,Q), not member(Q,V), merge(Q,R,[Q#V]).

reaction([P1; P2], Rate, R) :- slow(P1, P2, Rate, Q), merge(Q,R,[(P1|P2);Q]).
reaction([P], Rate, R) :- slow(P, Rate, Q), merge(Q,R,[P;Q]).

default([_],"unbind",0.1).
default(_,"bind",0.0003).
default(_,"displace",1.0).
default(_,"cover",1.0).

(* ribosome binding and expression *)
reaction([P], "expression", Q) :-
    P = C [rbs@X D aug^] [nil],  not hidden(rbs@X, P), 
    Q = C [rbs D aug^]   [<gfp>].

}
directive parameters [
  ka = 4.3333e-04;
  kda = 0.0383;
  kdb = 0.0135;
  kdInhTone = 9.5e-5;
  kdInhTthree = 3.5e-5;
  rbsBinding = 1;
  expression = 1;
  translation = 1;
  transcription = 1;
  folding = 1;
  migrate = 1;
  ku = 0.1;
]

directive simulation { final=5000; plots=[<gfp>] }
 
(*
// Switch design I: OR
( 1 [<a4 a3 a2 a1>]
| 1 [<b4 b3 b2 b1>]
| 1 [<a1* a2*!0 a3* a4*!1 rbs a4!1 aug^ a2!0 linker 
      b1* b2*!2 b3* b4*!3 rbs b4!3 aug^ b2!2 linker gfp>]
)
*)
(*
// Switch design II: AND
( 1 [<u* a1*>]
| 1 [<a2* u  >]
| 1 [<a1 a2!0 blank top^!1 rbs top^*!1 aug^ a2*!0 linker gfp>]
)

*)

( 1 [<a4 a3 a2 a1>]
| 1 [<b4 b3 b2 b1>]
| 1 [<a1* a2*!0 a3* a4*!1 rbs a4!1 aug^ a2!0 linker 
      b1* b2*!2 b3* b4*!3 rbs b4!3 aug^ b2!2 linker gfp>]
)
"""

  let crn = Dsd.compile code
  let debug1 = crn.to_string ()
  let debug2 = crn.to_string_structure ()
  // check that some reaction is produced
  Assert.True(crn.reactions |> List.exists (fun r -> r.rate = MassAction (Expression.Key "expression")))



[<Fact(DisplayName="Logic DSD - Resolution - hidden (toehold switch)")>]
let testPath() =
  let prog = Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> Microsoft.Research.DNA.LogicDSD.psite cle x false)) """
last([H], H).
last([H#Tail], Last) :- last(Tail, Last).

hidden(D!i, D'!i, P) :-
  P = C [D!i@End A@Start],
  path(A@Start, _@End, P, "right", [], Path),
  not (last(Path, D'!i)).

hidden(D!i, D'!i, P) :-
  P = C [A@Start D!i@End],
  path(A@Start, _@End, P, "left", [], Path),
  not (last(Path, D'!i)).

path(_@End, _@End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X@Start, _@End, P, "left", Visited, Path) :-
  not (Start = End),
  P = C[Y@Start' X@Start],
  not member(Y@Start', Visited),
  path(Y@Start', _@End, P, "left", [X@Start # Visited], Path).

path(X@Start, _@End, P, "right", Visited, Path) :-
  not (Start = End),
  P = C[X@Start Y@Start'],
  not member(Y@ Start', Visited),
  path(Y@Start', _@End, P, "right", [X@Start # Visited], Path).
  
path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "left", Visited, Path).

path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "right", Visited, Path).

path(D!i@Start, _@End, P, _, Visited, Path) :-
  not (Start = End),
  P = C [D!i@Start] [D'!i@Start'],
  not member(D'!i@Start', Visited),
  path(D'!i@Start', _@End, P, "any", [D!i@Start # Visited], Path).
  """
  let provider   = RulesDSD.Syntax.makeProvider ()
  let siteParser = Microsoft.Research.DNA.LogicDSD.psite cle provider true
  let strand = Parser.from_string (RulesDSD.Parser.psystem siteParser) "<topA* blankA*!7 bottomA*!0 a*!1> | <a!1 bottomA!0 blankA!7 topA!2 rbs topA*!2 aug^ bottomA* linker b!3 bottomB!4 blankB!5 topB!6 rbs topB* aug^ bottomB* linker gene> | <topB*!6 blankB*!5 bottomB*!4 b*!3>"
  let di  = RulesDSD.Syntax.Process.ToList(strand) |> List.head |> List.item 1
              |> fun x -> RulesDSD.Syntax.Pattern.Inner [x, RulesDSD.Syntax.Location.Var (-1, "_")]
              |> RulesDSD.Syntax.Term.Pat
  let di' = RulesDSD.Syntax.Process.ToList(strand) |> List.tail |> List.head   |> List.item 2
              |> fun x -> RulesDSD.Syntax.Pattern.Inner [x, RulesDSD.Syntax.Location.Var (-1, "_")]
              |> RulesDSD.Syntax.Term.Pat
  match Resolution.resolveAll 
        (RulesDSD.Syntax.Neg 
          (RulesDSD.Syntax.Pred 
            ("hidden", [di; di'; RulesDSD.Syntax.Term.Proc strand]))) prog Microsoft.Research.DNA.LogicDSD.engine (*ref Map.empty*) with
  | Some sols -> ()
  | None -> failwith "No path found."




[<Fact(DisplayName="Logic DSD - Example - Abstraction hierarchies")>]
// this example models the reference 
let testAbstractionHierarchies() = 

  let code = """
directive rules {
unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].

bind(P1,P2,Q,D!i) :- 
  P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
  Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).
  
displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!j],
  Q = C [E!j D!i] [D] [D'!i E'!j].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!j D'!i],
  Q = C [D] [D!i E!j] [E'!j D'!i].

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not covers(Q,_,D!i,_).

displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displaces(Q,_,D!i,_).

displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displacesL(Q,_,D!i,_).

covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not covers(Q,_,D!i,_).

coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coversL(Q,_,D!i,_).

unbinds(P,R,D!i,[D#L]) :- 
  P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
  Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).

find(D,Type,Rate):- rate(D,Type,Rate).
find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).

slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,_,L),find(L, "bind", Rate). //, productive(Q,_,_!i,L).
fast(P, Rate, Q) :- displaces(P,Q,_,L), find(L, "displace", Rate).
fast(P, Rate, Q) :- displacesL(P,Q,_,L), find(L, "displace", Rate).
fast(P, Rate, Q) :- covers(P,Q,_,L), find(L, "cover", Rate).
fast(P, Rate, Q) :- coversL(P,Q,_,L), find(L, "cover", Rate).
fast(P, Rate, Q) :- unbinds(P,Q,_,L), find(L, "unbind", Rate).

productive(P,Q,E!j,L) :- displaces(P,Q,E!j,L).
productive(P,Q,E!j,L) :- displacesL(P,Q,E!j,L).
productive(P,Q,E!j,L) :- covers(P,Q,E!j,L).
productive(P,Q,E!j,L) :- coversL(P,Q,E!j,L).

mergestep(P,Q,V) :- fast(P,_,Q), not member(Q,V).
merge(P,P,V) :- not mergestep(P,_,V).
merge(P,R,V) :- mergestep(P,Q,V), merge(Q,R,[Q#V]).

reaction([P1; P2], Rate, R) :- slow(P1, P2, Rate, Q), merge(Q,R,[(P1|P2);Q]).
reaction([P], Rate, R) :- slow(P, Rate, Q), merge(Q,R,[P;Q]).

default([_],"unbind",0.1).
default(_,"bind",0.0003).
default(_,"displace",1.0).
default(_,"cover",1.0).

rate([s3^], "bind", 0.00042).
rate([s3^], "unbind", 0.04).
rate([s5^], "bind", 0.00065).
rate([s5^], "unbind", 0.004).

}

directive simulator deterministic
directive deterministic {stiff = true}
directive simulation {final = 7000}
( 13 [ <s1 s2!0> | <s4!1 s5^!2> | <s5^*!2 s4*!1 s3^* s2*!0> ]
| 10 [ <s2 s3^ s4>   ]
)  """
  
  let crn = Dsd.compile code
  let debug1 = crn.to_string ()
  let debug2 = crn.to_string_structure ()
  // check that some reaction is produced
  Assert.True(crn.reactions |> List.exists (fun r -> r.rate = MassAction (Expression.Float 0.00042))) // bind rate



[<Fact(DisplayName="Logic DSD - Localised - A cargo-sorting DNA Robot (strand graphs, semantic abstractions)")>]
// this example models the reference 
let testDNARobot() = 
  let code = """
directive rules {

(****** Localised semantics ********)
  (*** tethers(P, L): find all the tethers L in process P ***)
  filterTethers([], []).
  filterTethers([T#Ds], [Ls#Ts]) :-
    tethered(T, Ls),
    filterTethers(Ds, Ts).
  
  filterTethers([D#Ds], Ts) :-
    not tethered(D, _),
    filterTethers(Ds, Ts).
  
  tethers(P, Ts) :-
    toDomains(P, Ds),        // in-built predicate
    filterTethers(Ds, Ts).

  (*** tetheredDomains(P, TDs) :  find all tethered domains in P ***)
  filterTetheredDomains([], []).
  filterTetheredDomains([D#Ds], [D#Ts]) :-
    tethered(D, _),
    filterTetheredDomains(Ds, Ts).
  
  filterTetheredDomains([D#Ds], Ts) :-
    not tethered(D, _),
    filterTetheredDomains(Ds, Ts).
  
  tetheredDomains(P, TDs) :-
    toDomains(P, Ds),                 // in-built predicate
    filterTetheredDomains(Ds, TDs).

  (*** proximal(P1, P2):  P1 and P2 are close enough if either one of them is not tethered, 
                          or if they are tethered in the same region. ***)
  hasIntersection(Ts1, Ts2) :- 
  	concat(Ts1, T1),
    concat(Ts2, T2),
    not intersection(T1, T2, []).
    
  shared([], _).
  shared(_, []).
  shared(Ts1, Ts2) :-
    not (Ts1 = []), not (Ts2 = []),
    hasIntersection(Ts1, Ts2).

  proximal(P1, P2) :-
    tethers(P1, Ts1), 
    tethers(P2, Ts2), 
    shared(Ts1, Ts2),
    
    (* CRN pruning: bind P1 and P2 only if they have a most one tether each. 
                    Tethers are also unique, so discard P1 and P2 if they both share a tether *)
    tetheredDomains(P1, D1), length(D1, N1), N1 <= 1,
    tetheredDomains(P2, D2), length(D2, N2), N2 <= 1,
    intersection(D1, D2, []).
  
  bind(P1,P2,Q,D!i) :- 
    proximal(P1, P2),
    P1 = C1 [D], P2 = C2 [D'], compl(D, D'), 
    Q  = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2).

  (****** /Localised semantics ********)
  
(* Strand graph semantics*)
hidden(D@X, P) :-
  unbound(D),
  P = C [A@End D@X B@Start], 
  path(B@Start, _@End, P, "right", [], Path).

hidden(D@X, P) :-
  unbound(D),
  P = C [B@Start D@X A@End], 
  path(B@Start, _@End, P, "left", [], Path).

hidden(D!i, D'!i, P) :-
  P = C [D!i@End A@Start],
  path(A@Start, _@End, P, "right", [], Path),
  not (last(Path, D'!i)).

hidden(D!i, D'!i, P) :-
  P = C [A@Start D!i@End],
  path(A@Start, _@End, P, "left", [], Path),
  not (last(Path, D'!i)).

path(_@End, _@End, _, _, Visited, Path) :-
  reverse(Visited, Path).

path(X@Start, _@End, P, "left", Visited, Path) :-
  not (Start = End),
  P = C[Y@Start' X@Start],
  not member(Y@Start', Visited),
  path(Y@Start', _@End, P, "left", [X@Start # Visited], Path).

path(X@Start, _@End, P, "right", Visited, Path) :-
  not (Start = End),
  P = C[X@Start Y@Start'],
  not member(Y@ Start', Visited),
  path(Y@Start', _@End, P, "right", [X@Start # Visited], Path).
  
path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "left", Visited, Path).

path(X@Start, _@End, P, "any", Visited, Path) :-
  not (Start = End),
  path(X@Start, _@End, P, "right", Visited, Path).

path(D!i@Start, _@End, P, _, Visited, Path) :-
  not (Start = End),
  P = C [D!i@Start] [D'!i@Start'],
  not member(D'!i@Start', Visited),
  path(D'!i@Start', _@End, P, "any", [D!i@Start # Visited], Path).

junction(A, B, P) :- junctionR(A, B, P, []).
junction(A, B, P) :- junctionL(A, B, P, []).
junctionR(_!j,_!j,_,_). 
junctionR(E!j,F!k,Q,V):- 
  Q = C [F!k] [G!l E'!j] [E!j],
  not member(X, V),
  not member(Y, V),
  junctionR(G!l,F!k,Q,[X;Y#V]).

junctionL(_!j,_!j,_, _).
junctionL(E!j,F!k,Q, V):- 
  Q = C [F!k] [E'!j@X G!l@Y] [E!j],
  not member(X, V),
  not member(Y, V),
  junctionL(G!l,F!k,[X;Y#V]).

displace(P,Q,E!j,D!i) :-
  P = C [E!j D] [D!i] [D'!i E'!k], junction(E!j, E'!k, P),
  Q = C [E!j D!i] [D] [D'!i E'!k].

displaceL(P,Q,E!j,D!i) :- 
  P = C [D!i] [D E!j] [E'!k D'!i], junction(E!j, E'!k, P),
  Q = C [D] [D!i E!j] [E'!k D'!i].
(*/ Strand graph semantics*)

unbind(P,Q,D!i) :-
  P = C [D!i] [D'!i], toehold(D), 
  Q = C [D] [D'], not adjacent(D!i,_,P).

adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
 

cover(P,Q,E!j,D!i) :-
  P = C [E!j D] [D' E'!j], compl(D, D'), 
  Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).

coverL(P,Q,E!j,D!i) :- 
  P = C [D E!j] [E'!j D'], compl(D, D'), 
  Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).

binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).

displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displace(Q,_,D!i,_).

displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displaceL(Q,_,D!i,_).

covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not cover(Q,_,D!i,_).

coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coverL(Q,_,D!i,_).

unbinds(P,R,D!i,[D#L]) :- 
  P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
  Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).

find(D,Type,Rate):- rate(D,Type,Rate).
find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).

slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), productive(Q,_,D!i,L).
slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), not unbind(Q, _, D!i, _).
fast(P, Q) :- displaces(P,Q,_,L).//, find(L, "displace", Rate).
fast(P, Q) :- displacesL(P,Q,_,L).//, find(L, "displace", Rate).
fast(P, Q) :- covers(P,Q,_,L).//, find(L, "cover", Rate).
fast(P, Q) :- coversL(P,Q,_,L).//, find(L, "cover", Rate).
fast(P, Q) :- unbinds(P,Q,_,L).//, find(L, "unbind", Rate).

productive(P,Q,E!j,L) :- displace(P,Q,E!j,L).
productive(P,Q,E!j,L) :- displaceL(P,Q,E!j,L).
productive(P,Q,E!j,L) :- cover(P,Q,E!j,L).
productive(P,Q,E!j,L) :- coverL(P,Q,E!j,L).

merge(P,P,V) :- not fast(P,_).
merge(P,R,V) :- fast(P,Q), not member(Q,V), merge(Q,R,[Q#V]).

reaction([P1; P2], Rate, R) :- slow(P1, P2, Rate, Q), merge(Q,R,[(P1|P2);Q]).
reaction([P], Rate, R) :- slow(P, Rate, Q), merge(Q,R,[P;Q]).

default([_],"unbind",0.1).
default(_,"bind",0.0003).
default(_,"displace",1.0).
default(_,"cover",1.0).
  




  
  (* robot starting point: reaches the drop zone for the first cargo, and the cargo pickup area *)
  tethered(start,["dropZoneA"; "toCargo"]).
  
  (* tracks between starting point, cargos pickup area and drop zones *)
  tethered(track1,["toCargo"; "transitA"]).
  tethered(track2,["transitA";"pickupArea"; "transitB"]).
  tethered(track3,["transitB"; "dropZoneB"]).
  
  (* cargo dropping points *)
  tethered(goal1p,["dropZoneA"]).
  tethered(goal2p,["dropZoneB"]).
  
  (* cargo starting points *)
  tethered(cargo1p*,["pickupArea"; "transitA"]).
  tethered(cargo2p*,["pickupArea"; "transitB"]).
  
  }

  directive simulation {
    final = 60000; 
  }

  directive parameters [
    kbind   = 0.003;
    kunbind = 0.1;
    migrate = 1;
  ]

  ( 
  // robot
    1 [ <hand^ arm foot^ leg!0 foot^!1> | <start*!3 foot^*!1 leg*!0> | <start!3> ]
  // goals
  | 1 [ <hand^ arm cargo1^* goal1p*!0> | <goal1p!0> ] 
  | 1 [ <hand^ arm cargo2^* goal2p*!0> | <goal2p!0> ]

  // cargos
  | 1 [ <cargo1^ arm*!0 hand^* cargoA> | <arm!0 cargo1p!1> | <cargo1p*!1> ]
  | 1 [ <cargo2^ arm*!0 hand^* cargoB> | <arm!0 cargo2p!1> | <cargo2p*!1> ]
  //tracks  
  | 1 [ <track1 leg* foot^*> ]
  | 1 [ <track2 foot^* leg*> ]
  | 1 [ <track3 leg* foot^*> ]
  )
"""
  let crn = Dsd.compile code
  let debug1 = crn.to_string ()
  let debug2 = crn.to_string_structure ()
  // check that some reaction is produced
  Assert.False(List.isEmpty crn.reactions)


[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Plotting - module in plots")>]
let moduleInPlots () =
  let code = """directive simulation {
  final=600; 
  plots=[Input1(b);Input2(x);Output(x);Signal(x)];
}
directive simulator deterministic
directive parameters [k=0.003;u=0.1]
directive compilation infinite
dom tb = {bind=k;unbind=u;colour="red"}
dom tx = {bind=k;unbind=u;colour="green"}
dom to = {bind=k;unbind=u;colour="blue"}
def Input1(b) = <tb^ b>
def Input2(x) = <tx^ x>
def Output(x) = <x to^>
def Join(b,x) = {tb^*}[b tx^]:[x to^]
def Reporter(x) = <fl^>[x]{to^*}
def Signal(x) = <fl^ x>
def System() = 
  new b new x
  ( 10 Input1(b)
  | 10 Input2(x)
  | 0 Output(x)
  | 100 Join(b,x)
  | 100 Reporter(x)
  | 0 Signal(x)
  )
( System() | System() )"""
  Dsd.compile code |> ignore



[<Trait("Category", "Parsing")>]
[<Fact(DisplayName="Classic DSD - Plotting - Plot species should not influence CRN generation")>]
let plotSpeciesInfluenceCrnGeneration () =
  let code = """directive simulation {
  final = 600; 
  plots = [Input1(); Input2(); Output(); Signal()];
}
directive simulator deterministic
directive parameters [ 
  k = 0.003;
  u = 0.1; 
]
directive compilation infinite
dom tb = {bind = k; unbind = u; colour = "red"}
dom tx = {bind = k; unbind = u; colour = "green"}
dom to = {bind = k; unbind = u; colour = "blue"}
def Input1() = <tb^ b>
def Input2() = <tx^ x>
def Output() = <x to^>
def Join() = {tb^*}[b tx^]:[x to^]
def Reporter() = <fl^>[x]{to^*}
def Signal() = <fl^ x>
( <x tx^>
| 100 Join()
)"""
  let crn = Dsd.compile code

  Assert.True(crn.reactions.Length = 0)

// Initial condition examples
let initial_past_example () = 
  "directive simulation { initial=-3.0; final=-2.0; plots=[<x>] }
  directive compilation infinite
  dom t = {bind = 1.0}
  ( <t^ x>
  | {t^*}[x]
  )"
  |> Dsd.compile

// Test initial condition example. The solution to X + Y -> B is similar to 
//   X + X -> B when X and Y are initialized to the same value.
//   That is, dX/dt = -k * X^2 => X(t) = 1/(k*t + c)
//   Subbing in initial conditions, we have X(0) = 1 = 1/c, so c = 1.
//   B = 1 - X = 1 - 1/(k*t + 1) = k*t / (k*t + 1)
[<Fact(DisplayName="Simulation - Initials before t=0")>]
let initials_past () = 
    let crn = initial_past_example ()
    crn.initials 
    |> List.iter (fun i -> 
        Assert.Equal (None, i.time)
    )
    let table = crn.to_oslo().simulate() 
    let final_point = List.last table.columns.Head.values
    Assert.Equal(0.5, final_point, 2)

let times_overwrite_example () = 
  "directive simulation {initial=-200.0; final=600}
directive simulator deterministic
directive compilation infinite
def Input1() = <t^ b> @ 300.0
def Input2() = <t^ x> @ 200.0
def Join() = {t^*}[b t^]:[x t^]
( 10 Input1()
| 10 Input2() @ 100.0
| 100 Join()
)"
  |> Dsd.compile

[<Fact(DisplayName="Simulation - Timed initials overwritten")>]
let times_overwrite () = 
    let crn = times_overwrite_example ()
    let initial_map = 
        crn.initials 
        |> List.map (fun i -> i.species.name, Option.map (Expression.eval (fun _ -> 0.0)) i.time)
        |> Map.ofList
    
    Assert.Equal(Some 300.0, initial_map.["Input1"])
    Assert.Equal(Some 100.0, initial_map.["Input2"])
    Assert.Equal(None, initial_map.["Join"])




[<Fact(DisplayName="Logic DSD - Parsing - processes")>]
let parsingProcessesTest() =
  let text = """customReaction([ <l k^* s!0 bl s*!0 z^* l*!1 z^*!2> | <s*!3 l!1> | <z^!2 s!3>; <z^ s!0> | <s*!0 k^> ], my_rate, 
<l k^*!0 s!1 bl s*!2 z^*!3 l*!4 z^*!5> | <s*!1 k^!0> | <s*!6 l!4> | <z^!3 s!2> | <z^!5 s!6>)."""

  Parser.from_string (RulesDSD.Parser.pprogram cle (fun x -> LogicDSD.psite cle x true)) text |> ignore




[<Fact(DisplayName="Logic DSD - Localized - HCR")>]
let testRibo() =
  let code = """directive rules {
  
  bind(P1,P2,Q,D!i) :- 
    P1 = C1 [D@X], compl(D, D'), P2 = C2 [D'@Y], 
    Q = C1 [D!i] | C2 [D'!i], freshBond(D!i, P1|P2),
    not hidden(D@X, P1),
    not hidden(D'@Y, P2).
  
  bind(P,Q,D!i) :- 
    P = C[D @ X][D' @ Y], compl(D, D'),
    Q = C [D!i][D'!i], freshBond(D!i, P),
    not hidden(D  @ X,  P),
    not hidden(D' @ Y,  P).
    
  displace(P,Q,E!j,D!i) :-
    P = C [E!j D] [D!i] [D'!i E'!k], junction(E!j, E'!k, P),
    Q = C [E!j D!i] [D] [D'!i E'!k].
  
  displaceL(P,Q,E!j,D!i) :- 
    P = C [D!i] [D E!j] [E'!k D'!i], junction(E!j, E'!k, P),
    Q = C [D] [D!i E!j] [E'!k D'!i].
  
  unbind(P,Q,D!i) :-
    P = C [D!i] [D'!i], toehold(D), 
    Q = C [D] [D'], not adjacent(D!i,_,P).
  
  adjacent(D!i,E!j,P) :- P = C [D!i E!j] [E'!j D'!i].
  adjacent(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
   
  
  cover(P,Q,E!j,D!i) :-
    P = C [E!j D] [D' E'!j], compl(D, D'), 
    Q = C [E!j D!i] [D'!i E'!j], freshBond(D!i,P).
  
  coverL(P,Q,E!j,D!i) :- 
    P = C [D E!j] [E'!j D'], compl(D, D'), 
    Q = C [D!i E!j] [E'!j D'!i], freshBond(D!i,P).
  
  binds(P1,P2,R,D!i,[D#L]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
  binds(P1,P2,Q,D!i,[D]) :- bind(P1,P2,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).
  
  binds(P,R,D!i,[D#L]) :- bind(P,Q,D!i), not coverL(Q,_,D!i,_), covers(Q,R,D!i,L).
  binds(P,Q,D!i,[D])   :- bind(P,Q,D!i), not coverL(Q,_,D!i,_), not cover(Q,_,D!i,_).
  
  displaces(P,R,E!j,[D#L]) :- displace(P,Q,E!j,D!i), displaces(Q,R,D!i,L).
  displaces(P,Q,E!j,[D]) :- displace(P,Q,E!j,D!i), not displace(Q,_,D!i,_).
  
  displacesL(P,R,E!j,[D#L]) :- displaceL(P,Q,E!j,D!i), displacesL(Q,R,D!i,L).
  displacesL(P,Q,E!j,[D]) :- displaceL(P,Q,E!j,D!i), not displaceL(Q,_,D!i,_).
  
  covers(P,R,E!j,[D#L]) :- cover(P,Q,E!j,D!i), covers(Q,R,D!i,L).
  covers(P,Q,E!j,[D]) :- cover(P,Q,E!j,D!i), not cover(Q,_,D!i,_).
  
  coversL(P,R,E!j,[D#L]) :- coverL(P,Q,E!j,D!i), coversL(Q,R,D!i,L).
  coversL(P,Q,E!j,[D]) :- coverL(P,Q,E!j,D!i), not coverL(Q,_,D!i,_).
  
  unbinds(P,R,D!i,[D#L]) :- 
    P = C [D!i E!j] [E'!j D'!i], toehold(D), not boundL(D!i,_,P), 
    Q = C [D E!j] [E'!j D'], unbinds(Q,R,E!j,L). 
  boundL(D!i,E!j,P) :- P = C [E!j D!i] [D'!i E'!j].
  unbinds(P,Q,D!i,[D]) :- unbind(P,Q,D!i).
  
  (* auxiliary functions *)
  hidden(D@X, P) :-
    unbound(D),
    P = C [A@End D@X B@Start], 
    path(B@Start, A@End, P, "right", [], Path).
  
  hidden(D@X, P) :-
    unbound(D),
    P = C [B@Start D@X A@End], 
    path(B@Start, _@End, P, "left", [], Path).
  
  hidden(D!i, D'!i, P) :-
    P = C [D!i@End A@Start],
    path(A@Start, _@End, P, "right", [], Path),
    not (last(Path, D'!i)).
  
  hidden(D!i, D'!i, P) :-
    P = C [A@Start D!i@End],
    path(A@Start, _@End, P, "left", [], Path),
    not (last(Path, D'!i)).
  
  (* path predicate: find a @Path from @Start to @End *)
  path(_@End, _@End, _, _, Visited, Path) :-
    reverse(Visited, Path).
  
  path(X@Start, _@End, P, "left", Visited, Path) :-
    not (Start = End),
    P = C[Y@Start' X@Start],
    not member(Y@Start', Visited),
    path(Y@Start', _@End, P, "left", [X@Start # Visited], Path).
  
  path(X@Start, _@End, P, "right", Visited, Path) :-
    not (Start = End),
    P = C[X@Start Y@Start'],
    not member(Y@ Start', Visited),
    path(Y@Start', _@End, P, "right", [X@Start # Visited], Path).
    
  path(X@Start, _@End, P, "any", Visited, Path) :-
    not (Start = End),
    path(X@Start, _@End, P, "left", Visited, Path).
  
  path(X@Start, _@End, P, "any", Visited, Path) :-
    not (Start = End),
    path(X@Start, _@End, P, "right", Visited, Path).
  
  path(D!i@Start, _@End, P, _, Visited, Path) :-
    not (Start = End),
    P = C [D!i@Start] [D'!i@Start'],
    not member(D'!i@Start', Visited),
    path(D'!i@Start', _@End, P, "any", [D!i@Start # Visited], Path).
  
  junction(A, B, P) :- junctionR(A, B, P, []).
  junction(A, B, P) :- junctionL(A, B, P, []).
  junctionR(_!j,_!j,_,_). 
  junctionR(E!j,F!k,Q,V):- 
    Q = C [F!k] [G!l@X E'!j@Y] [E!j],
    not member(X, V),
    not member(Y, V),
    junctionR(G!l,F!k,Q,[X;Y#V]).
  
  junctionL(_!j,_!j,_, _).
  junctionL(E!j,F!k,Q, V):- 
    Q = C [F!k] [E'!j@X G!l@Y] [E!j],
    not member(X, V),
    not member(Y, V),
    junctionL(G!l,F!k,Q,[X;Y#V]).
  
  
  (* infinite semantics *)
  find(D,Type,Rate):- rate(D,Type,Rate).
  find(D,Type,Rate):- default(D,Type,Rate), not rate(D,Type,_).
  
  slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), productive(Q,_,D!i,L).
  slow(P1, P2, Rate, Q) :- binds(P1,P2,Q,D!i,L),find(L, "bind", Rate), not unbind(Q, _, D!i, _).
  
  fast(P, Q) :- displaces(P,Q,_,L).//, find(L, "displace", Rate).
  fast(P, Q) :- displacesL(P,Q,_,L).//, find(L, "displace", Rate).
  fast(P, Q) :- covers(P,Q,_,L).//, find(L, "cover", Rate).
  fast(P, Q) :- coversL(P,Q,_,L).//, find(L, "cover", Rate).
  fast(P, Q) :- unbinds(P,Q,_,L).//, find(L, "unbind", Rate).
  fast(P, Q) :- binds(P,Q, _, L).
  
  productive(P,Q,E!j,L) :- displace(P,Q,E!j,L).
  productive(P,Q,E!j,L) :- displaceL(P,Q,E!j,L).
  productive(P,Q,E!j,L) :- cover(P,Q,E!j,L).
  productive(P,Q,E!j,L) :- coverL(P,Q,E!j,L).
  
  merge(P,P,V) :- not fast(P,_).
  merge(P,R,V) :- fast(P,Q), not member(Q,V), merge(Q,R,[Q#V]).
  
  reaction([P1; P2], Rate, R) :- slow(P1, P2, Rate, Q), merge(Q,R,[(P1|P2);Q]).
  
  default([_],"unbind",0.1).
  default(_,"bind","bind").
  default(_,"displace",1.0).
  default(_,"cover",1.0).
  
  }
  
  (10 [<c1 s1^>]
  |1 [ <a2!3 a1!0>
     | <a1*!0 s1^* c1*!1 c0* s2^ c2 c1!1>
     | <c2!4 c3 s3^ c1* c2*!4 s2^* a2*!3>
     ]
  )"""
  Dsd.compile code
  |> ignore
