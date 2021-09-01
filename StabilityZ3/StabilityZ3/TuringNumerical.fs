// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.Biology.StabilityZ3.TuringNumerical

open MathNet.Numerics.RootFinding
open Microsoft.Research.CRNEngine.Expression
open MathNet.Numerics
open System

open Microsoft.Research.Biology.StabilityZ3

type Vec = MathNet.Numerics.LinearAlgebra.Vector<float>
type EquilibriumSolver = Broyden | Ode | OdeBroyden
type Classification = 
    Unstable | Stable | NoiseAmplifying | TuringStable | TuringOscillatory
    override this.ToString() = 
        match this with
        | Unstable -> "Unstable"
        | Stable   -> "Stable"
        | NoiseAmplifying -> "Noise-amplifying"
        | TuringStable -> "Turing (Stable)"
        | TuringOscillatory -> "Turing (Oscillatory)"
    static member parse str = 
        match str with
        | "Unstable" -> Unstable
        | "Stable" -> Stable
        | "Noise-amplifying" -> NoiseAmplifying
        | "Turing (Stable)" -> TuringStable
        | "Turing (Oscillatory)" -> TuringOscillatory
        | _ -> failwith "Unrecognized classification"

type Result = 
    { parameters : Map<string,float>
    ; e0         : float
    ; eigs       : float []
    ; classification : Classification
    ; time       : float
    }


let Vec_ofArray v = MathNet.Numerics.LinearAlgebra.DenseVector.ofArray v

let prepare_eqns (S:Dynamical) p =
    let states = S.State
    let x0 = states |> Array.map (fun s -> match Map.tryFind s S.initials with Some v -> v | None -> 1.0)
    let eqns_p = S.Eqns |> Array.map (fun eq -> substitute p eq)

    let lambdas_ints =
        eqns_p
        |> Array.map (Microsoft.Research.CRNEngine.Expression.map (fun str -> Array.IndexOf(states, str)))
        |> Array.map Microsoft.Research.CRNEngine.Expression.simplify
        |> Array.map Microsoft.Research.CRNEngine.Expression.to_lambda

    let eqns (x:float[]) =
        let f_int = Array.get x
        let f_int_ref = ref f_int
        lambdas_ints |> Array.map(fun y -> y.keyref f_int_ref)
    eqns, x0


/// Obtain a stable equilibrium using ODE simulation
let equilibrium mode (S:Dynamical) parameters : Map<string,float> = 
    // Start by inlining the parameter values
    let p = parameters |> Map.map (fun _ v -> Float v)
    let eqns, x0 = prepare_eqns S p
    let t0 = 0.0
    let tF = 1000.0

    let doBroyden x0 = 
        let sol = ref x0
        if Broyden.TryFindRoot (System.Func<float[],float[]>(eqns), !sol, 1e-4, 10000, sol)
        then !sol
        else x0
    let doOde x0 = 
        Oslo.GearBDF.solve t0 tF (Oslo.Vector.ofArray x0) (fun t x -> eqns (Oslo.Vector.toArray x) |> Oslo.Vector.ofArray) (Oslo.GearBDF.defaults()) 
        |> List.last 
        |> snd 
        |> Oslo.Vector.toArray
        
    // If we're using Broyden only, then initialise from x0, otherwise run the Ode solver to find a better initial point, then run Broyden
    let sol = 
        match mode with
        | Broyden    -> doBroyden x0
        | Ode        -> doOde x0            
        | OdeBroyden -> doOde x0 |> doBroyden
    
    // Test whether solution is valid
    if Array.exists System.Double.IsNaN sol
    then failwith "Didn't find a valid solution"
    else Array.zip S.State sol |> Map.ofArray

/// Maximum of the real part of the eigenvalues
let emax (J:Matrix) = 
    let A = 
        J.values 
        |> Array.map (fun v -> v.values |> Array.map (eval (fun _ -> nan)))
        |> MathNet.Numerics.LinearAlgebra.DenseMatrix.ofColumnArrays
    if A.Exists (fun v -> System.Double.IsNaN(v)) 
    then System.Double.NaN
    else
        (MathNet.Numerics.LinearAlgebra.Matrix.eigen A).EigenValues.ToArray()
        |> Array.map (fun c -> c.Real)
        |> Array.max


/// Numerically analyze a Dynamical system
let analyze mode (prob:Dynamical) parameters ws console = 

    let sw = System.Diagnostics.Stopwatch.StartNew()
    if console then printfn "- Finding equilibrium"
    let eq = equilibrium mode prob parameters
    let eqStr = eq |> Map.toList |> List.map (fun (sp,v) -> sprintf "%s = %1.3g" sp v) |> String.concat ", "
    if console then printf "- Checking stability of equilibrium [ %s ]: " eqStr
    let values = Map.fold (fun p xi xv -> Map.add xi xv p) parameters eq |> Map.map (fun k v -> Float v)
    let J = prob.J () |> Matrix.map (substitute values)
    let e0 = emax J
    if console then if e0 >= 0.0 then printf "UNSTABLE\n" else printf "STABLE\n"
    
    if console then printf "- Checking instability with diffusion: "
    let es = 
        ws |> Array.map (fun w -> 
            let D = Matrix.map (substitute values) (prob.D (Float (w*w)))
            emax (J - D)
        )
    let imax, esup = es |> Array.indexed |> Array.maxBy snd
    let ediff = esup - Array.last es
    let tolerance = 1e-6
    let classification = 
        if e0 > 0.0
        then Unstable
        else
          if esup < 0.0
          then Stable
          else
            if ediff > tolerance
            then 
              let wmax = ws.[imax]
              let Dmax = Matrix.map (substitute values) (prob.D (Float (wmax*wmax)))
              let cs = 
                  LinearAlgebra.characteristic_polynomial LinearAlgebra.Faddeev (J - Dmax)
                  |> fun v -> v.values |> Array.map (eval (fun _ -> nan))
              let n = cs.Length
              if (cs.[n-1] < 0.0) && (Array.forall (fun c -> c > 0.0) cs.[0..n-2])
              then TuringStable
              else TuringOscillatory
            else NoiseAmplifying
    
    let time = (float sw.ElapsedMilliseconds) / 1000.0
    sw.Stop()
    
    if console then 
        match Array.tryFindIndex (fun e -> e > 0.0) es with
        | Some i -> printf "UNSTABLE\n"
        | None   -> printf "STABLE\n"
    eq, e0, es, classification, time

let internal _dispersion X D J = 
  X 
  |> Array.map(fun q -> 
    let p = ["w", Float q**2.0] |> Map.ofSeq
    let D' = D |> Matrix.map(substitute p)
    emax (J-D')
  )

let dispersion X (prob:Dynamical) =
  let res =
    match prob.solution with
    | SAT (r,_) -> r
    | _ -> failwith "Numerical Turing analysis is supported only for systems with a solution"
  
  let vals = res |> Map.map (fun _ v -> v |> Float)        
  let V = vals.Remove("w") |> substitute
  let J = prob.J () |> Matrix.map V
  let D = Key "w" |> prob.D |> Matrix.map V
  _dispersion X D J


/// Numerically check the solution of a Dynamical system  
let check (prob:Dynamical) = //ratios = 
  let res =
    match prob.solution with
    | SAT (r,_) -> r
    | _ -> failwith "Numerical Turing analysis is supported only for systems with a solution"
  
  printfn "\nCheck with numerics"
  let vals = res |> Map.map (fun _ v -> v |> Float)        
  
  let J = prob.J () |> Matrix.map (substitute vals)    // Inlined equilibrium
  let lmax = emax J
  if lmax > 0.0 then "Unstable" else "Stable"
  |> printfn "max Lambda %1.3g; %s WITHOUT diffusion" lmax
    
  let D = Key "wc" |> prob.D |> Matrix.map (substitute vals)    // Inlined diffusion values
  let lmax' = emax (J-D)
  if lmax' > 0.0 then "Unstable" else "Stable"
  |> printfn "max Lambda %1.3g; %s WITH diffusion" lmax'

  let isCorrect = (lmax < 0.0 && lmax' > 0.0)  
  if isCorrect then printfn "CORRECT" else printfn "INCORRECT"
  
  let X = MathNet.Numerics.Generate.LogSpaced(101,-2.0,2.0)
  isCorrect, (X, dispersion X prob)

/// Sample parameter sets for a dynamical system
let sample rng distributions =
    let generators = 
        distributions
        |> Map.toList
        |> List.map (fun (p,d) -> 
            match d with
            | Normal n -> p, fun () -> (Distributions.Normal (n.mu, n.sigma,rng)).Sample()
            | LogNormal n -> p, fun () -> (Distributions.LogNormal (n.mu, n.sigma,rng)).Sample()
            | Uniform u -> p, fun () -> (Distributions.ContinuousUniform (u.min, u.max, rng)).Sample()
            | LogUniform u -> p, fun () -> (Distributions.ContinuousUniform (log u.min, log u.max, rng)).Sample() |> System.Math.Exp
            | TruncatedNormal n -> p, fun () -> 
                  let mutable v = n.min - 1.0
                  while (v < n.min) || (v > n.max) do v <- (Distributions.Normal (n.mean, n.stdev, rng)).Sample()
                  v
            | LogTruncatedNormal n -> p, fun () -> 
                  let mutable v = n.min - 1.0
                  while (v < n.min) || (v > n.max) do v <- (Distributions.LogNormal (log n.mean, n.stdev, rng)).Sample()
                  v
            | Fixed v -> p, fun () -> v
        )
  
    generators 
    |> List.map (fun (name, sample) -> 
        name, sample ()
    )
    |> Map.ofList

let sample_first rng mode (sys:Dynamical) condition ws nmax console =
    let mutable failures = 0
    let rec run count =
        if console 
        then printfn "Sample %d" count
        else 
            if count % 50 = 0 then System.Console.Error.WriteLine(count)
            System.Console.Error.Write('.')

        let p = sample rng sys.distributions
        try 
            let eq, e0, eigs, cl, time = analyze mode sys p ws console
        
            // Check whether we succeeded or not
            if condition cl
            then 
                printfn "Sampler returned a solution after %d attempts, with %d failures" (count+1) failures
                Some { parameters=p; e0=e0; eigs=eigs; classification=cl; time=time }, count
            else 
                if count < nmax
                then run (count+1)
                else 
                    printfn "Didn't find any parameters satisfying DDI after %d samples" nmax
                    None, count
        with _ -> 
            failures <- failures + 1
            run count
    run 0

let sample_space rng mode (sys:Dynamical) ws n = 
    let res = 
        Array.init n (fun i ->
            if i % 50 = 0 then System.Console.Error.WriteLine(i)
            System.Console.Error.Write('.')

            let p = sample rng sys.distributions
            try 
              let eq, e0, eigs, cl, time = analyze mode sys p ws false
              Some { parameters=p; e0=e0; eigs=eigs; classification=cl; time=time }
            with _ -> 
              None
        )
    let successes, failures = res |> Array.partition (fun i -> i.IsSome)
    printfn "Sampler completed with %d failures" failures.Length
    successes |> Array.choose id

(*let sample_first_async rng mode (sys:Dynamical) ws nmax = 
    let cancel = Async.CancellationToken
    let computation = 
        Array.init nmax (fun i -> 
            let p = sample rng sys.distributions
            async {
                if i % 50 = 0 then System.Console.Error.WriteLine(i)
                System.Console.Error.Write('.')
                try 
                    let eq, e0, eigs, cl = analyze mode sys p ws false
                    let emax = Array.max eigs        
                    return Some (p,e0,emax,cl)
                with _ -> return None
            }
        )
        |> Async.Parallel
    //let results = Async.RunSynchronously (computation, cancellationToken = cancel)
    let successes, failures = results |> Array.partition (fun r -> r.IsSome)
    printfn "Sampler completed with %d failures" failures.Length
    successes |> Array.choose id *)

let sample_space_async rng mode (sys:Dynamical) ws n = 
    let ps = Array.init n (fun i -> sample rng sys.distributions)        
    let results = 
        ps
        |> Array.mapi (fun i p -> 
            async {
                if i % 50 = 0 then System.Console.Error.WriteLine(i)
                System.Console.Error.Write('.')
                try 
                    let eq, e0, eigs, cl, time = analyze mode sys p ws false
                    return Some { parameters=p; e0=e0; eigs=eigs; classification=cl; time=time }
                with _ -> return None
            }
        )
        |> Async.Parallel
        |> Async.RunSynchronously
    let successes, failures = results |> Array.partition (fun r -> r.IsSome)
    printfn "Sampler completed with %d failures" failures.Length
    successes |> Array.choose id 
    

/// Confirm that the Turing instability of a dynamical system is preserved following a model reduction
let Confirm_Full_System (full_system:Dynamical) (reduced_system:Dynamical) (replace_rule:Map<string,NumExpr>) =
    let species=full_system.State|>Array.toList

    let remain_list = full_system.diffusion|>Map.toList|>List.map fst
    let remove_list = Set.ofList species - Set.ofList remain_list|>Set.toList
    let nd_locs = remove_list|>List.map (fun a -> List.findIndex (fun b->a=b) species)|>Array.ofList
    
    let non_diffusible_sub_Jacobian = (full_system.J ()).Slice nd_locs nd_locs
    //let full_Jacobian=full_system.J

    let reduced_map = 
        match reduced_system.solution with 
        | SAT (pars,_) -> pars |> Map.map (fun k v -> NumExpr.Float v)
        | _ -> failwithf "Numerical analysis can be applied only to SAT systems (currently %A)" reduced_system.solution

    let replace_rule_map = replace_rule |> Map.map (fun _ s -> substitute reduced_map s)
    let full_map = Map.fold (fun acc key value -> Map.add key value acc) replace_rule_map reduced_map

    //let full_J_eval = full_Jacobian |> Matrix.map (fun expr -> expr.Subst full_map)
    let sub_J_eval = non_diffusible_sub_Jacobian |> Matrix.map (substitute full_map)
    emax sub_J_eval<0.0 && emax sub_J_eval<0.0


/// Store results summary in a CSV file
let write_results (res:(Map<string,float> * Classification)[]) fname = 
    let p0 = fst res.[0] |> Map.toList
    let headers = "Classification" :: (p0 |> List.map fst) |> String.concat ","
    let body = 
        res
        |> Array.map (fun (p,cl) -> 
            cl.ToString() :: (p |> Map.toList |> List.map (snd >> sprintf "%1.6g")) |> String.concat ","
        )
    let contents = Array.append [|headers|] body
    System.IO.File.WriteAllLines (fname, contents)
    
/// Read results summary from a CSV file
let read_results fname = 
    let contents = System.IO.File.ReadAllLines fname
    let headers = contents.[0]
    let parameter_names = headers.Split(',').[1..]
    contents.[1..]
    |> Array.map (fun line ->
        let elems = line.Split(',') 
        Array.zip parameter_names (elems.[1..] |> Array.map float) |> Map.ofArray, Classification.parse elems.[0]
    )
