module Microsoft.Research.Biology.StabilityZ3.Invariants


let Transpose (S: int[][]) = Array.init S.[0].Length (fun i -> Array.init S.Length (fun j -> S.[j].[i]))


(************************************************************************************************************* 
    Implementation of the Farkas algorithm (aka Fourier-Motzkin elimination) to find invariants in CRNs.    
    More info at:
        http://www.inf.ed.ac.uk/teaching/courses/mlcsb/2010-2011/mlcsb-slides15-handout.pdf
        http://www.labri.fr/perso/anca/FDS/Pn-ESTII.pdf
    
    Note that the returned invariants are not minimal (i.e. not basis for null space).

    The code was adapted from BiologyGit\ReasoningEngine\ReasoningEngine\Client.fs
***************************************************************************************************************)
let Farkas (S : int list list) =
  let S = S |> List.map (List.map int64)

  let (n, m) = (S.Length, S.[0].Length)

  // Generates a row of identify matrix, consisting of all zeros except for position i, which is 1.  
  let elemI i = List.init n (fun ic -> if ic = i then 1L else 0L)

  //1) Construct an augmented matrix S|I where I is the identify matrix of dimension n.
  let Sa = List.mapi (fun i x -> List.concat([x; elemI i])) S

  //2) Perform the eliminations  
  let D = ref Sa
  let Dn = ref Sa
  for i in 0 .. m - 1 do
    Dn := !D
    let r = (!D).Length - 1 // Largest row number in D.
    for j in 0 .. r - 1 do
      for k in j + 1 .. r do // Partition matrix into two sets of rows.
        let Dc = !D
        let d1 = Dc.[j]
        let d2 = Dc.[k]
        if (d1.[i] > 0L && d2.[i] < 0L) || (d1.[i] < 0L && d2.[i] > 0L) then
          let d = 
            List.zip d1 d2
            |> List.map (fun (x, y) -> (abs d2.[i]) * x + (abs d1.[i]) * y)           
            
            
          let gcd = 
                d 
                |> Array.ofList
                |> Array.filter (fun x -> x<>0L)                
                |> MathNet.Numerics.Euclid.GreatestCommonDivisor
                          
          let dp = List.map (fun x -> x / gcd) d          
          Dn := dp :: !Dn //FIXME changes the order?

    // Delete all rows whose i-th component is different from 0.
    D :=
      !Dn
      |> List.fold (fun acc x -> if x.[i] = 0L then x :: acc else acc) []
      |> List.rev    

  //3) Return the resulting matrix (removing the first m columns).
  !D
  |> List.map (Seq.skip m >> Seq.toList >> List.map int) 

/// Array-based implementation of Farkas (more efficient, and removes need for Array/List reversal)
let FarkasArray (stoich : int[][]) =
    let stoich64 = stoich |> Array.map (Array.map int64)
    let (n, m) = (stoich64.Length, stoich64.[0].Length)

    // Generates a row of identify matrix, consisting of all zeros except for position i, which is 1.  
    let elemI i = Array.init n (fun ic -> if ic = i then 1L else 0L)

    //1) Construct an augmented matrix S|I where I is the identify matrix of dimension n.
    let sI = Array.mapi (fun i x -> Array.concat([x; elemI i])) stoich64

    //2) Perform the eliminations  
    let Dout : int64[][] = 
        [0..m-1]
        |> List.fold (fun (D:int64[][]) i -> 
            let r = D.Length - 1
    
            // Partition matrix into two sets of rows.
            let Dj' = 
                List.fold (fun Dj j -> 
                    List.fold (fun Dk k ->    
                        let d1 = D.[j]
                        let d2 = D.[k]
                        if (d1.[i] > 0L && d2.[i] < 0L) || (d1.[i] < 0L && d2.[i] > 0L) 
                        then
                            let d = Array.map2 (fun x y -> (abs d2.[i]) * x + (abs d1.[i]) * y) d1 d2   
                            let gcd = d |> Array.filter (fun x -> x<>0L) |> MathNet.Numerics.Euclid.GreatestCommonDivisor                          
                            let dp = Array.map (fun x -> x / gcd) d
                            Array.append Dk [|dp|]
                        else 
                            Dk
                    ) Dj [j+1..r]
                ) D [0..r-1]
    
            // Delete all rows whose i-th component is different from 0.
            Dj' |> Array.fold (fun acc x -> if x.[i] = 0L then Array.append [|x|] acc else acc) [||]
        ) sI 

    //3) Return the resulting matrix (removing the first m columns).
    Dout |> Array.map (Array.skip m >> Array.map int)   


// Gaussian-Jordan Elimination
let inline flipValues (v: NumExpr[]) (i: int) (j: int) =
    let temp = v.[i]
    v.[i] <- v.[j]
    v.[j] <- temp

let inline flipRows (A: float[][]) (r1: int) (r2: int) =
    let r1 = A.[r1]
    let r2 = A.[r2]
    for k = 0 to r1.Length - 1 do
        let x = r1.[k]
        r1.[k] <- r2.[k]
        r2.[k] <- x

/// Gauss-Jordan elimination
let GaussJordanElimination (A:float[][]) (b:NumExpr[]) =     
    let m = A.Length
    let n = A.[0].Length
    let a = Array.init m (fun i -> Array.copy A.[i])
    let x = Array.copy b

    // Implementation follows algorithm described here: http://people.math.carleton.ca/~kcheung/math/notes/MATH1107/wk04/04_gaussian_elimination.html
    let mutable p = 0
    while p < m do
      for k in 0 .. n-1 do
        match a.[p..m-1] |> Array.map (Array.item k) |> Array.tryFindIndex (fun v -> v <> 0.0) with
        | Some loc ->
          let i = loc + p
          // Swap rows
          if i <> p 
          then
            flipRows a i p
            flipValues x i p
          // Scale
          let alpha = a.[p].[k]
          a.[p] <- a.[p] |> Array.map (fun x -> x/alpha)
          x.[p] <- x.[p] / alpha
          // Subtract
          for q in 0..m-1 do
            if q<>p
            then  
              let alpha = a.[q].[k]
              a.[q] <- Array.map2 (fun aq ap -> aq - alpha*ap) a.[q] a.[p]
              x.[q] <- x.[q] - alpha*x.[p]         
          // Increment and loop
          p <- p + 1
        | None -> ()
    
    a, x |> Array.map MathNetWrapper.SimplifyNum 

/// Evaluate replacement rules from RREF A*x = b
let ReplacementRules x Ar b = 
    Array.map2 (fun a c ->
        let prods = Array.zip a x |> Array.filter (fun (ai,_) -> ai <> 0.0)
        snd prods.[0], c - NumExpr.Plus (List.ofArray (Array.map (fun (coeff,sp) -> coeff*(Microsoft.Research.CRNEngine.Expression.Key sp)) prods.[1..])) |> MathNetWrapper.SimplifyNum
    ) Ar b
    |> Map.ofArray