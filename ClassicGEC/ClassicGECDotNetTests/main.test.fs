module Microsoft.Research.GEC.MainTest

open Microsoft.Research.GEC.Program

open Xunit
open FsUnit.Xunit
open System.Diagnostics


[<Fact(DisplayName="GEC - Parser Directives")>]
let ``directivesParserTest``() = 

    let sampledir = "directive sample 100000.0"
    let sampledir2 = "directive sample 100000.0 1"
    let sampledir3 = "directive sample 100000.0 all"
    let plotPredprey = "directive plot predator[ccdB]; prey[ccdB]"
    let plotab = "directive plot A; B; C"
    let plotcellsig = "directive plot cell[gfp]; Signal"
    let plotrec = "directive plot receiver[gfp]"

    let parseGECDirective (s:string) = Parser.from_string Program.directiveParser s

    let sampledirfs = parseGECDirective sampledir
    let sampledir2fs = parseGECDirective sampledir2
    let sampledir3fs = parseGECDirective sampledir3
    let plotPredpreyfs = parseGECDirective plotPredprey
    let plotabfs = parseGECDirective plotab
    let plotcellsigfs = parseGECDirective plotcellsig
    let plotrecfs = parseGECDirective plotrec


    Debug.WriteLine("Test completed")


[<Fact(DisplayName="GEC - Parser Expression")>]
let ``expressionParserTest``() = 
    let exp0 = "a+ c"
    let exp1 = "a+b"
    let exp2 = "a+b+c"
    let exp3 = "a+b+c+d"
    let exp4 = "a+b-c"

    let parseExpression (s:string) = Parser.from_string Program.expressionParser s

    let exp0p = parseExpression exp0
    let exp1p = parseExpression exp1
    let exp2p = parseExpression exp2
    let exp3p = parseExpression exp3
    let exp4p = parseExpression exp4

    Debug.WriteLine("Test Completed")

[<Fact(DisplayName="GEC - Parser Brick")>]
let ``brickParserTest``() = 
    let test (s:string) = Parser.from_string Program.parse_brick s
    let a = test "r0051:prom"
    let b = test "rbs"
    let c = test "pcr<codes(Q2b)>"
    let d = test "r0051:prom<pos(Q2b-H2)>"
    let e = test "ter"
    let f = test "prom<con(RT), neg(i, RB, RUB, RTB)>"
    let g = test "rbs<rate(R)>"
    let h = test "pcr<codes(o,RD)>"

    Debug.WriteLine("End of Test")

[<Fact(DisplayName="GEC - Parser New")>]
let ``newparsertest``()=
    let prog = "new RB. new RUB.\n" +
               "prom<con(RT), neg(i, RB, RUB, RTB)>"
    
    let test (s:string) = Parser.from_string Program.parse_new_outer s
    let a = test prog
    match a with
    | Ast.New(id1,prog1) -> 
        Assert.Equal(id1,"RB")
        match prog1 with 
        | Ast.New(id2,prog2) ->
            Assert.Equal(id2,"RUB")
            match prog2 with
            | Ast.Brick(t,v,propLst) -> 
                Debug.WriteLine(v)
            | _ -> failwith ""
        | _ -> failwith ""
    | _ -> failwith ""

    Debug.WriteLine("end of test")

[<Fact(DisplayName="GEC - Parser Reaction")>]
let ``ReactionCodeParserTest``() = 
    
    let rxn = "luxR + Signal -> luxR::Signal"
    let test (s:string) = Parser.from_string Program.parse_reaction s
    let res = test rxn
    Debug.WriteLine("end of test")

[<Fact(DisplayName="GEC - Parser Module Invocation ")>]
let modInvTest() = 
    let miprog = "gate(A,B)"
    let test (s:string) = Parser.from_string Program.parse_ast_template_inv s
    let a = test miprog

    Debug.WriteLine("End of test")


