module Tests.Test1

open Xunit
open FsUnit

open Microsoft.Research.Biology.StabilityZ3
module LA = Microsoft.Research.Biology.StabilityZ3.LinearAlgebra
open Microsoft.Research.CRNEngine.Expression

//open Microsoft.R

(*
let numericalMatrix3x3 = 
  [ [ 0.9; 0.9; 0.1 ]
  ; [ 0.1; 0.4; 0.4 ]
  ; [ 0.9; 0.8; 0.9 ] ]
  |> List.map (List.map Float >> Array.ofList)
  |> Array.ofList
  |> Matrix.Create

let numericalMatrix4x4 = 
  [ [ 0.9; 0.9; 0.1; 0.2 ]
  ; [ 0.1; 0.4; 0.4; 0.0 ]
  ; [ 0.9; 0.8; 0.9; 0.3 ] 
  ; [ 0.2; 0.3; 0.4; 0.5 ] ]
  |> List.map (List.map Float >> Array.ofList)
  |> Array.ofList
  |> Matrix.Create
*)

let CheckDet (M:Matrix) (vexp:float) = 
    let v = 
        try eval (fun _ -> 0.0) M.Det
        with _ -> failwith "ERROR\n\tExpected a number"
    if v = vexp  then printfn "done"
    else failwithf "ERROR\n\tExpected %f but calculated %f" vexp v


let Crn2Dynamical crn =                    
    let odes_string = 
        crn
        |> SymbolicODEs.ofCrn
        |> SymbolicODEs.toOdeString
        
    let odes = 
        odes_string
        |> Array.map(fun l -> 
                    let eq = l.Split('=')
                    let v = eq.[0].Replace("d(","").Replace(")","").Trim()
                    let e = eq.[1] |> ExpressionFunctions.Parse
                    v,e)
    
    let species = odes |> Array.map fst |> List.ofArray

    Dynamical.Create(odes,species)    
    |> Dynamical.setAllLB 0.0   

let MinchevaCrn = 
    """
    directive parameters [
        k1=1.0, {variation=Random};
        k2=1.0, {variation=Random};        
        k3=1.0, {variation=Random};
        k4=1.0, {variation=Random};
        k5=1.0, {variation=Random};
        k6=1.0, {variation=Random};
        k7=1.0, {variation=Random};
        k8=1.0, {variation=Random};
        k9=1.0, {variation=Random};
        k10=1.0, {variation=Random};
    ]

         ->{k1} A2 
    | A2 ->{k2} 
    | A1 + A2 ->{k3} A1 + A4
    | A4 + A6 ->{k4} A2 + A1
    | A5 + A1 ->{k5} A5 + A6
    | A5 + A4 ->{k6} A3
    | A3 ->{k7} A5 + A4
    | A5 + A2 ->{k8} A7
    | A7 ->{k9} A5 + A2
    | A4 ->{k10}    
    """
    |> Microsoft.Research.CRNEngine.Crn.from_string

//let ExportTuringSMTLIB solver settings filename S = 
//    let z3, solver = Solver.MkSolver solver    
//    let expr, vars = TuringSymbolic.TuringZ3 settings z3 S
//    let smt = z3.BenchmarkToSMTString("","","","",[||], expr).[2..].Replace("(set-info :status )\n", "").Replace("+zero", "0")
//    System.IO.File.WriteAllText(sprintf "%s.smt2" filename ,smt)
//    z3.Dispose()
//    S

[<Trait("Category","Slow")>]
[<Fact(DisplayName="Mincheva", Skip="Legacy test, but it fails")>]
let MinchevaTest () =     

    let solver   = Solver.solverType.SmtTO 240000u
    let settings = TuringSymbolic.TuringAnalysisSettings.Default        

    let C1 = Key "C1"
    let C2 = Key "C2"
    let A = Array.init 8 (fun i -> sprintf "A%i" i |> Key)

    let p =
        [ "A3", C1 - A.[5] - A.[7]
        ; "A1", C2 - A.[6]
        ]
        |> Map.ofList
 
    let p2 = 
        [ "A7", A.[2]*A.[5] ]
        |> Map.ofList
    
    let p3 = 
        [ "A6", A.[5]*C2/(A.[5]+A.[4])] 
        |> Map.ofList
    
    let p4 = 
        [ "A5", C1/(A.[4]+A.[2] + Float 1.0)] 
        |> Map.ofList
        
    let result = 
        MinchevaCrn
        |> Crn2Dynamical
        |> Dynamical.reduceStateWR p
        //|> Dynamical.addCst (Eq(A.[3], C1 - A.[5] - A.[7]))
        //|> Dynamical.addCst (Eq(A.[1], C2 - A.[6]))
        |> Dynamical.reduceStateWR p2
        |> Dynamical.reduceStateWR p3
        // |> Dynamical.reduceStateWR p4
        //|> DisplayEquations    
        |> Solver.CheckTuring true solver settings
        //|> ExportTuringSMTLIB solver settings @"S:\Source\Repos\SynBio\TuringPatterns\Analysis\Notebooks\Mincheva2" 
        //|> fun (s,_,_,_) -> s.solution.IsSome
    Assert.Equal("SAT", result.solution.Status)

[<Fact>]
let GammaTest () = 
    let solver = Solver.solverType.PortfolioTO 240000u
    let settings = TuringSymbolic.TuringAnalysisSettings.Default
        
    let Gamma =
        let C6 = Key "C6"
        let C12 = Key "C12"
        let aiiA = Key "aiiA"
        let k6 = Key "k6"
        let k4 = Key "k4"
        let k5 = Key "k5"
        let k3 = Key "k3"
        let k1 = Key "k1"
        let k2 = Key "k2"
        let k8 = Key "k8"
        let k7 = Key "k7"    
    
        let odes = 
            [| "C6", (k4*C12*C12/(C12*C12+k7))-k1*C6*aiiA
             ; "C12", k5*(C12*C12/(C12*C12+k7))-k2*C12*aiiA
             ; "aiiA", k6*(C6*C6/(C6*C6+k8))-k3*aiiA
             |]
    
        //Dynamical.Create(odes, ["C6"; "C12"; "CI"])
        Dynamical.Create(odes, Map.ofList ["C6",Key "Diff6"; "C12",Key "Diff12"]) 
        |> Dynamical.setAllLB 0.0
        //|> Dynamical.setUB "Dy" 1.01

    let res= Gamma |> Solver.CheckTuring true solver settings    
    Assert.Equal("SAT", res.solution.Status)

let test_model_reduction () = 
  
  let full = Examples.brusselator ()  
  let reduced, replacements = full.ModelReduce ()
  reduced


let test_beta_reduction () = 
    Examples.betaCrn ()


    //printf "\nChecking determinant of a 3x3 matrix..."
    //CheckDet numericalMatrix3x3 0.2510
        
    //printf "\nChecking determinant of a 4x4 matrix..."
    //CheckDet numericalMatrix4x4 0.1100
    


    (*
    0.9900    1.2500    0.5400
    0.4900    0.5700    0.5300
    1.7000    1.8500    1.2200
    *)
    //printf "\nChecking power of 3x3 matrix..."
    //(numericalMatrix3x3.Pow 2).values |> Array.map(fun v -> v.values |> Array.map (fun e -> e.Eval.Value.ToString()) |> String.concat ",\t") |> String.concat ";\n"    
    //|> printfn "\n%A"
    
    (* 2.2000 *)
//    printf "\nChecking trace of 3x3 matrix..."
//    numericalMatrix3x3.Trace.Eval.Value
//    |> printfn "\n%A"

    (* 2.7000 *)
//    printf "\nChecking trace of 4x4 matrix..."
//    numericalMatrix4x4.Trace.Eval.Value
//    |> printfn "\n%A"
    

//    (Matrix.sI 3 (Float 66.6)).ToString()
//    |> printfn "%A"

    //(Matrix.Zero 3).ToString()
    //|> printfn "%A"


//    let M1 = Matrix.sI 3 (Float 2.0)
//    let M2 = Matrix.sI 3 (Float 3.0)
//    (M1+M2).ToString()
//    |> printfn "\n\n%s"


    (* [1, -11/5, 103/100, -251/1000] *)
//    printf "\nChecking characteristic polynomial coefficients of a 3x3 matrix..."
//    (numericalMatrix3x3 |> LA.characteristic_polynomial LA.Faddeev).values
//    |> Array.map(fun e -> e.Eval.Value)
//    |> printfn "\n\t%A"

    
    (* [-251/1000; 1; -323/100; -30759/10000] *)
//    numericalMatrix3x3 
//    |> LA.characteristic_polynomial LA.Faddeev
//    |> LA.routh_hurwitz LA.RouthArray
//    |> Array.map(fun e -> e.Eval.Value)
//    |> printfn "\n\t%A"



    (* [1; -11/5; 403/440; -251/1000] *)
//    numericalMatrix3x3 
//    |> LA.characteristic_polynomial LA.Faddeev
//    |> LA.routh_hurwitz LA.RouthTable
//    |> Array.map(fun e -> e.Eval.Value)
//    |> printfn "\n\t%A"



    //    numericalMatrix3x3 
//    |> LA.characteristic_polynomial LA.Faddeev
//    |> LA.routh_hurwitz LA.RouthArray
//    |> Array.map(fun e -> e.Eval.Value)
//    |> printfn "\n\t%A"




    (* [1, -27/10, 197/100, -339/500, 11/100]] *)
//    numericalMatrix4x4
//    |> LA.characteristic_polynomial LA.Faddeev
//    |> fun x -> x.values  
//    |> Array.map(fun e -> e.Eval.Value)
//    |> printfn "\n\t%A"


    (* [1; -27/10; 1547/900; -390783/773500; 11/100] *)
    (*
    numericalMatrix4x4 
    |> LA.characteristic_polynomial LA.Faddeev
    |> LA.routh_hurwitz LA.RouthTable
    |> Array.map(fun e -> e.Eval.Value)
    |> printfn "\n\t%A"
    *)

    //printfn (.Det.Eval)

(*

open Microsoft.Research.Biology.TuringZ3.Encoding
open Microsoft.Research.Biology.TuringZ3.LinearAlgebra
open Microsoft.Research.Biology.TuringZ3.Matrix
open Microsoft.Research.Biology.TuringZ3.Complex


// Test the characteristic polynomial and routh array
let test_char_pol_numerical jac = 
  let cs = characteristic_polynomial jac
  let routh_array, routh_matrices = routh_hurwitz cs
  let solver = z3.MkSolver()
  solver.Check() |> ignore
  let model = solver.Model

  jac
  |> Matrix.eval model
  |> Matrix.toString
  |> printfn "\n\nJacobian\n%s"

  cs
  |> Vector.eval model
  |> Vector.toString  
  |> printfn "\n\nCharacteristic polynomial: [%s]"

  routh_matrices
  |> Array.last 
  |> Matrix.eval model
  |> Matrix.toString
  |> printfn "\n\nRouth matrices\n%s"

  routh_array 
  |> Array.map (fun c -> (c |> Eval solver.Model).ToString()) |> String.concat "; "
  |> printfn "\n\nRouth array: [%s]"



// Test the characteristic polynomial and routh array
let test_char_pol jac = 
  let cs = characteristic_polynomial jac
  let routh_array, routh_matrices = routh_hurwitz cs
  
  jac
  |> Matrix.toString
  |> printfn "\n\nJacobian\n%s"

  cs 
  |> Vector.toString
  |> printfn "\n\nCharacteristic polynomial: [%s]"

  routh_matrices
  |> Array.last 
  |> Matrix.toString
  |> printfn "\n\nRouth matrices\n%s"

  routh_array 
  |> Array.map (fun c -> c.Simplify().ToString()) |> String.concat "; "
  |> printfn "\n\nRouth array: [%s]"





let symbolic2 () = 
  [ [ "a11"; "a12" ]
  ; [ "a21"; "a22" ]  
  ]
  |> List.map (List.map MkKey >> Array.ofList)
  |> Array.ofList
  |> Matrix.Create

let symbolic3 () = 
  [ [ "a11"; "a12"; "a13" ]
  ; [ "a21"; "a22"; "a23" ]
  ; [ "a31"; "a32"; "a33" ]
  ]
  |> List.map (List.map MkKey >> Array.ofList)
  |> Array.ofList
  |> Matrix.Create



let var_matrix n = Array.init n (fun i -> Array.init n (fun j -> MkKey (sprintf "a%d%d" i j)))

let print_det_charpol_algebraic n =
  let A = var_matrix n |> Matrix.Create
  (A.Det).ToString()
  |> printfn "Determinant:\n%A\n"
  
  let c = characteristic_polynomial A 
  c |> printfn "Characteristic polynomial:\n%A"
 *)