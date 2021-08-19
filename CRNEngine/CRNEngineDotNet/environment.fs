[<JavaScript>] 
module Microsoft.Research.CRNEngine.Environment

type t = Map<string,float>

let create (l:(string*float) list) = Map.ofList l
let find (e:t) (s:string) =
    match Map.tryFind s e with
    | Some value -> value
    | _ -> failwithf "The environment does not contain: %s" s
let empty:t = Map.empty

let extend (e1:t) (e2:t) = 
  let f (e:t) (s:string,n:float) = Map.add s n e in
    List.fold f e2 (Map.toList e1)

let remove_list (names:string list) (e:t) = 
  let f (e:t) (name:string) = Map.remove name e in
    List.fold f e names

//let to_string (e:t) = "[" + Lib.string_of_list (fun (k, v) -> k + " = " + (Lib.display_float v)) "; " (Map.toList e) + "]"
let to_string (e:t) = "[" + Lib.string_of_list (fun (k, v) -> k + " = " + (v.ToString())) "; " (Map.toList e) + "]"

