module Tests.LinearAlgebra

open Xunit
open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.TuringSymbolic
open Microsoft.Research.CRNEngine
open Microsoft.Research.CRNEngine.Expression


[<Fact(DisplayName="Linear Algebra - Neutral stability")>]
let neutral_stability () = 
    let z3, solver = Solver.MkSolver Solver.Nlsat
    let x = Key "x"
    let vars = ["x"] |> List.map (fun v -> v, z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr) |> Map.ofList
    let m = Matrix.Create [|[| zero; zero |]; [|zero; x|]|] |> Matrix.encode vars z3
    let num_conservations = 0
    let stab_cst = Stability.NeutrallyStableZ3 z3 num_conservations m
    let pos_cst = BGeq (x, zero) |> Encoding.BoolExpr2RealRec vars z3
    let result = Solver.CheckNoPrint (z3,solver) vars (Array.append stab_cst [| pos_cst |])
    Assert.True result.isSAT
    z3.Dispose()

[<Fact(DisplayName="Linear Algebra - Strict stability")>]
let strict_stability () = 
    let z3, solver = Solver.MkSolver Solver.Nlsat
    let x = Key "x"
    let vars = ["x"] |> List.map (fun v -> v, z3.MkFreshConst(v, z3.RealSort) :?> Microsoft.Z3.ArithExpr) |> Map.ofList
    let m = Matrix.Create [|[| zero; zero |]; [|zero; x|]|] |> Matrix.encode vars z3
    let stab_cst = Stability.StableZ3 Stability.Default z3 m
    let neg_cst = BLT (x, zero) |> Encoding.BoolExpr2RealRec vars z3
    let result = Solver.CheckNoPrint (z3,solver) vars (Array.append stab_cst [| neg_cst |])
    Assert.True result.isUNSAT
    z3.Dispose()

[<Fact(DisplayName="Linear Algebra - Differentiate polynomial")>]
let diff () = 
    let x = Key "X"
    let test = eval (fun _ -> 0.0) x
    let expr = x * x
    let differentiated = ExpressionFunctions.differentiate expr "X"
    let dx = Float 2.0 * x
    Assert.Equal (dx, differentiated)

[<Fact(DisplayName="Invariants of A -> B: A + B is conserved")>]
let farkas_test1 () = 
    let crn = Crn.from_string "A -> B"
    let S, _ = SymbolicODEs.Stoich crn
    let F = Invariants.FarkasArray S
    Assert.NotEmpty F

[<Fact(DisplayName="Invariants of A -> A + B: A is conserved")>]
let farkas_test2 () = 
    let crn = Crn.from_string "A -> A + B"
    let S, _ = SymbolicODEs.Stoich crn
    let F = Invariants.FarkasArray S
    Assert.NotEmpty F


[<Fact(DisplayName="Invariants of A -> B | B ->: No conservation laws")>]
let farkas_test3 () = 
    let crn = Crn.from_string "A -> B | B ->"
    let S, _ = SymbolicODEs.Stoich crn
    let F = Invariants.FarkasArray S
    Assert.Empty F