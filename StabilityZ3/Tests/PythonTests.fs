module Tests.Python

open Xunit
open FsUnit
open Python.Runtime
open FSharp.Interop.Dynamic
open FSharp.Interop.Dynamic.Operators

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.CRNEngine.Expression

let eqns_for_python eqns = eqns |> String.concat ", " |> sprintf "[%s]" 

[<Fact(DisplayName = "Python - from expression", Skip="Strategy not working currently")>]
let expressionToPy () = 

    use gil = Py.GIL()

    let eqns = [|Key "A" - Key "B"|]    // Very simple example

    // Convert NumExpr to a PyObject, using a map of known symbols
    let rec ToPy (symbs:Map<string,PyObject>) expr = 
        match expr with      
        | Key v         -> symbs.[v]
        | Float f       -> (PyObject.FromManagedObject f)
        | Divide d -> (ToPy symbs d.div1) ?/? (ToPy symbs d.div2)
        //| Power (a, b)  -> (a.ToPy symbs) ?^? b                        
        | Power p  -> failwith "Power not implemented"
        | Minus m  -> (ToPy symbs m.sub1) ?-? (ToPy symbs m.sub2)
        | Plus terms    -> terms |> List.fold (fun acc t -> acc ?+? (ToPy symbs t)) (PyObject.FromManagedObject 0.0) 
        | Times terms   -> terms |> List.fold (fun acc t -> acc ?*? (ToPy symbs t)) (PyObject.FromManagedObject 1.0)
        | Ceiling e     -> failwith "Conversion of Expression.Ceiling to Python not implemented"
        | Absolute _    -> failwith "Conversion of Expression.Absolute to Python not implemented"
        | Floor _       -> failwith "Conversion of Expression.Floor to Python not implemented"
        | Round _       -> failwith "Conversion of Expression.Round to Python not implemented"
        | Log _         -> failwith "Conversion of Expression.Log to Python not implemented"
        | Modulo _      -> failwith "Conversion of Expression.Modulo to Python not implemented"
        | If _          -> failwith "Conversion of Expression.If to Python not implemented"

    // Python libraries
    let symbol = Py.Import("sympy.core.symbol")    
    let solveset = Py.Import("sympy.solvers.solveset")

    let symbs = ["A";"B";"C"] |> List.map (fun s -> s, symbol?symbols(s) |> PyObject.FromManagedObject) |> Map.ofList
    //let py_eqns = eqns |> Array.map (fun e -> e.ToPy symbs)
    let py_eqns = eqns |> Array.map (ToPy symbs)
    let res = solveset?nonlinsolve(py_eqns,symbs.["A"]) |> PyList.AsList
    ()


[<Fact(DisplayName = "Python - simple call", Skip="Python.Net reduction not working at present")>]
let pythonCall () = 
    
    let symbstrs = ["x"; "y"; "z"; "w"] 
    let eqns = [|"x*y - 1"; "w*x*x + y*y - 5"|]
    let solution = PythonWrapper.nonlinsolve (Array.zip [|"x";"y"|] eqns) symbstrs
    ()


[<Fact(DisplayName = "Model reduction - Brusselator", Skip="Python.Net reduction not working at present")>]
let model_reduction_Brusselator () =     
    let sys = Examples.brusselator ()
    let _, repl = sys.ModelReduce ()       // Took 4 seconds with Python.Net code (17/11/2017)
    let expected = ["Z", (Key "X"*Key "X")] |> Map.ofList
    Assert.Equal<Map<string,NumExpr>> (expected, repl)
