(*
Provides top-level access to LSB translation. Functions in this module
will invoke the LSB compiler, invoke the Prolog engine and parse the results
in to an appropriate .NET data structure.

Author: Michael Pedersen.
Copyright © Microsoft Research, 2008-2009.
*)

[<JavaScript>]
module Microsoft.Research.GEC.Main

open Microsoft.Research.GEC.Ast
open Microsoft.Research.GEC.Trans
open Microsoft.Research.CRNEngine
open Microsoft.Research.GEC.DirectivesParser

open Parser
//open Microsoft.FSharp.Compatibility.OCaml
//open Microsoft.Research.ModellingEngine
open System.IO
open Printf



// make LBS rate definition strings
let mkSingleLBSRateDef (r,f) = "rate " + r + " = " + (Lib.display_float f) + ";"
let mkLBSRateDefs rateDecs =
  (Lib.string_of_list mkSingleLBSRateDef Lib.newline (expandRateDecs rateDecs)) + Lib.newline

// produce a string representation of an assignment...
let stringOfAss (xs:(string * string) list) =
  Lib.brack (Lib.string_of_list (fun (x,y) -> Lib.paren(Lib.quote x + ", " + Lib.quote y)) "; " xs)

/// Input position range, this will likely be replaced by something from FParsec
type pos = {l1 : int; c1 : int; l2 : int; c2 : int}

// a solution type for passing back to a C# client:
type tSolution = 
    {
           bbDevices  : tBbDevices;
           lbsProgram : tLBSProg;
           rateDecs   : tRateDecs;

           // variable assignments represent solutions to constraints:
           varAss     : ((string * string) list * (string * string) list * (string * string) list) list;
           substs     : Subst.t list;

           // an error option for reporting errors:
           error      : (string * pos option) option;

           // the number of solutions to constraints:
           numSolutions : int;
    }

    // get a specific instance of the device of a solution:
    member v.getDevicesInstance(num)  =
        let devices = v.bbDevices
        let (subst,_,_) = List.item num v.varAss
        //let devices' = devices |> List.map (fun device -> device |> List.map (fun var -> if (List.mem_assoc var subst) then List.assoc var subst else var))
        let devices' = devices |> List.map (fun device -> device |> List.map (fun var -> match Lib.try_assoc var subst with | Some x -> x | None -> var))
        Lib.brack (Lib.string_of_list (fun xs -> Lib.brack(Lib.string_of_list Lib.id "; " xs)) ";\r " devices')

    // get a specific instance of the device of a solution as a string list list:
    member v.getDevicesInstanceStructured(num)  =
        let devices = v.bbDevices
        let (subst,_,_) = List.item num v.varAss
        let devices' = devices |> List.map (fun device -> device |> List.map (fun var -> match Lib.try_assoc var subst with | Some x -> x | None -> var))  
        let devicesArray =  List.toArray(List.map (fun lst -> List.toArray(lst)) devices' )
        devicesArray
        
    // get a specific instance of the program of a solution:    
    member v.getProgramInstance(num) =
        let prog = v.lbsProgram
        let (_,substSpec,substRates) = List.item num v.varAss
        let lbsProgStr = lbsProgToStr prog (substSpec@substRates) 
        (mkLBSRateDefs v.rateDecs) + lbsProgStr
        // add a default declaration for mrna degradation:
        //let mrnaDegRate = "rate RMRNADeg = " + (Lib.display_float default_RMRNADeg) + ";\n"
        //mrnaDegRate + lbsProgStr        
        
    // get the program instance with declared rates assigned:    
    member v.getProgramDefault() =
        //let lbsProgStr = lbsProgToStr v.lbsProgram v.rateDecs
        //lbsProgStr
        let lbsProgStr = lbsProgToStr v.lbsProgram []
        (mkLBSRateDefs v.rateDecs) + lbsProgStr
        
    // get a specific instance of a species assignment:    
    member v.getSpecAss(num) =
        let (l1, specVarAss, l3) = List.item num v.varAss
        stringOfAss specVarAss

    // get a specific instance of a species assignment:    
    member v.getRateAss(num) =
        let (_, _, rateVars) = List.item num v.varAss
        stringOfAss rateVars

    member v.getVarAssString() =
        Lib.brack (Lib.string_of_list (fun (xs,ys,zs) -> Lib.paren (stringOfAss xs + ", " + stringOfAss ys + "; " + stringOfAss zs )) ",\r " v.varAss)

// define an empty solution:                  
let emptySolution = { bbDevices    = [];
                      lbsProgram   = LBSNil
                      rateDecs     = [];
                      varAss       = [];
                      substs       = [];
                      error        = None;
                      numSolutions = 0; }

(*
#if JavaScript
let Lexing_from_string = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString
#else
let Lexing_from_string = Lexing.from_string
#endif

(* Parse the LSB program from the given string. *)
let parse (text:string) =
    let prog = 
        // Create the lexer, presenting the bytes to the lexer as ASCII regardless of the original
        // encoding of the string (the lexer specification is designed to consume ASCII)
        let lexbuf = Lexing_from_string text

        // Call the parser 
        try 
            let prog = GEC.Pars.start Lex.token lexbuf
            prog

        with e -> 
            let bufPos1 = lexbuf.StartPos
            let bufPos2 = lexbuf.EndPos
            let pos = (bufPos1, bufPos2)
            let err = "Parse error near line " + bufPos1.Line.ToString() + ", character " + bufPos2.Column.ToString() + "\n\n"
            raise (LBS.Error.CompilerExPos(err, Some pos))
    prog
    *)


let parse (text:string) = Parser.from_string Program.parse text