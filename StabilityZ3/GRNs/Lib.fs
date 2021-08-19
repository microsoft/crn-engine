module Microsoft.Research.Biology.StabilityZ3.GRNs.Lib

/// Rotates a list by one place forward.
let rotate lst =
    List.tail lst @ [List.head lst]

/// Gets all rotations of a list.
let getRotations lst =
    let rec getAll lst i = if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
    getAll lst (List.length lst)

/// Gets all permutations (without repetition) of specified length from a list.
let rec getPerms n lst = 
    match n, lst with
    | 0, _ -> seq [[]]
    | _, [] -> seq []
    | k, _ -> lst |> getRotations |> Seq.collect (fun r -> Seq.map ((@) [List.head r]) (getPerms (k - 1) (List.tail r)))