[<JavaScript>]
module Microsoft.Research.ReactionDiffusion.Solver

let DotProduct a b = Array.fold2 (fun s ai bi -> s + ai * bi) 0.0 a b

let TridiagonalAlgorithm (a:float[]) (b:float[]) (c:float[]) (rhs:float[]) =

    let nn = rhs.Length;
    let cprime = Array.zeroCreate nn
    let d = Array.copy rhs
    let x = Array.zeroCreate nn

    cprime.[0] <- c.[0] / b.[0]
    d.[0] <- d.[0] / b.[0]

    for i in 1..(nn-2) do
        let temp = b.[i] - a.[i] * cprime.[i - 1]
        cprime.[i] <- c.[i] / temp
        d.[i] <- (d.[i] - a.[i] * d.[i - 1]) / temp

    d.[nn - 1] <- (d.[nn - 1] - a.[nn - 1] * d.[nn - 2]) / (b.[nn - 1] - a.[nn - 1] * cprime.[nn - 2])

    x.[nn - 1] <- d.[nn - 1];
    //for (int i = nn - 2; i >= 0; i--)
    for i in (nn-2).. -1 .. 0 do //Check this handles boundary conditions correctly
        x.[i] <- d.[i] - cprime.[i] * x.[i + 1]

    x

let ApplySMInvertMatrix (a:float[]) (b:float[]) (c:float[]) (u:float[]) (w:float[]) (z:float[]) =
    let y = TridiagonalAlgorithm a b c u
    let coeff = (DotProduct w y) / (1.0 + (DotProduct w z))
    Array.map2 (fun yi zi -> yi - coeff*zi) y z
    
    (*let ym =
        [|y|]
        |> Oslo.Matrix.ofArray 
        |> Oslo.Matrix.transpose*)    
    
    //let answer = ym - ((v * ym)[0,0] / (1 + (v * z)[0,0])) * z;
    (*let v_ym =  v ym
    let w_z = DotProduct w z
    let answer = Oslo.Matrix.subtract ym (Oslo.Matrix.scalMultMatr (v_ym.[0,0] / (1.0 + w_z.[0,0])) z)
    answer*)