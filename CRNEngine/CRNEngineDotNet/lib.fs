[<JavaScript>]
module Microsoft.Research.CRNEngine.Lib

let commit_number =
#if JavaScript
    // This method of storing the hash is not available in JavaScript.
    "0"
#else
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let versionFile = assembly.GetManifestResourceNames() |> Array.find (fun n -> n.EndsWith("version.txt"))
    use stream = assembly.GetManifestResourceStream(versionFile)
    use streamReader = new System.IO.StreamReader(stream)
    streamReader.ReadToEnd().Trim()
#endif

type choice<'a,'b> = Left of 'a | Right of 'b 

(* Another helper function for removing option types! *)
let unoption = function None -> failwith "unoption" | Some x -> x
   
(* Constants for the rest of the project. *)
let default_matching_degree = 1.0 (* Deprecated *)
let default_toehold_bind_rate = 3.0E-4 (* /nM/s *)
let default_toehold_unbind_rate = 0.1126 (* /s *)
let default_leak_rate_l = 1.0E-9 (* /nM/s *)
let default_leak_rate_w = default_leak_rate_l / 100.0 (* /M/s Currently not used, since "slow leaks" are disabled. *)
let default_tau_rate = default_toehold_unbind_rate
let default_toehold_length = 6
let default_specificity_length = 20
let default_elementary_migration_rate = 1.0 / (125E-6) (* /s elementary step time scale of 125 /s/step *)
let default_tolerance = 1.0e-6

(* For some unknown reason, some list fold functions have different names in F#. *)
let fold_left f acc xs  = List.fold f acc xs
let fold_right f xs acc = List.foldBack f xs acc
let zip xs ys           = List.zip xs ys
let unzip xys           = List.unzip xys
let forall p xs         = List.forall p xs
let rec contains x xs   = match xs with [] -> false | h::t -> x = h || contains x t
let collect f xs        = List.collect f xs
let iteri f xs          = List.iteri f xs
let curry f (x, y)      = f x y
let uncurry f x y       = f (x, y)

(* List.forall2 but without the exceptions... *)
let forall2 p xs ys =
  if (List.length xs) = (List.length ys) then
    List.forall2 p xs ys
  else false

(* map in linear time and a single traversal, but the result is reversed *)
let rev_map m l =
  let rec loop acc = function
    | [] -> acc
    | e::es -> loop (m e :: acc) es in
  loop [] l

(* tryFind in an associative list *)
let try_assoc e l = l |> List.tryPick (fun (k,v) -> if k = e then Some v else None)

(* The same applies to some array functions. *)
let array_of_list = Array.ofList
  
(* Helper function for printing lists. *)
let string_of_list (f:'a -> string) (sep:string) (xs:'a list) : string =
  let rec inner acc xs =
    match xs with
    | [] -> acc
    | [x] -> acc + (f x)
    | (x::xs) -> inner (acc + (f x) + sep) xs
  inner "" xs

(* Modified version of string_of_list which also includes the list index. *)
let string_of_listi (f:int -> 'a -> string) (sep:string) (xs:'a list) : string =
  let rec inner ctr acc xs =
    match xs with
    | [] -> acc
    | [x] -> acc + (f ctr x)
    | (x::xs) -> inner (ctr+1) (acc + (f ctr x) + sep) xs
  inner 0 "" xs

let tuple_to_string (l:string list) = 
  match l with
  | [] -> ""
  | [single] -> single
  | tuple -> "(" + (String.concat "," tuple) + ")"

let list_to_string (l:string list) = 
  match l with
  | [] -> ""
  | [single] -> single
  | tuple -> "[" + (String.concat ";" tuple) + "]"

(* Try to find a list element satisfying the predicate. *)
let tryFind (f:'a -> bool) (xs:'a list) : 'a option = List.tryFind f xs

(* Remove duplicates from a list. *)
let remove_duplicates eq s =
  let rec do_scan s acc =
    match s with
    | [] -> acc
    | (a::b) -> if List.exists (fun a' -> eq a a') acc then do_scan b acc else do_scan b (a::acc)
  List.rev (do_scan s [])

(* Find the first duplicate element in a list, if any. *)
let rec find_duplicate eq s =
  match s with
  | [] -> None
  | (a::b) -> if List.exists (fun a' -> eq a a') b then Some a else find_duplicate eq b

(* Get rightmost element of a list - get None if there are no elements. *)
let get_last_element (xs:'a list) =
  let rec get_last (xs:'a list) (acc:'a list) =
    match xs with
    | [] -> None
    | [x] -> Some(acc,x)
    | x::xs -> get_last xs (acc@[x])
  get_last xs []
  
(* Check whether a given element matches the first / last in a list. *)
let is_first (x:'a) (xs:'a list) = match xs with [] -> false | (x'::_) -> x = x'
let is_last (x:'a) (xs:'a list) = match (get_last_element xs) with None -> false | Some (_,x') -> x = x'

(* Get rightmost two elements of a list - get None if there are fewer than two elements. *)
let get_last_two_elements (xs:'a list) =
  match get_last_element xs with
  | Some(xs,y) ->
      (match get_last_element xs with
      | Some(xs,x) -> Some(xs,x,y)
      | None -> None)
  | None -> None
  
(* Get the first and last elements of a list - get None if there are fewer than two elements. *)
let get_first_and_last (xs:'a list) =
  match xs with
  | [] -> None
  | [_] -> None
  | x::xs ->
      match (get_last_element xs) with
      | Some(ys,y) -> Some(x,ys,y)
      | None -> None

(* Does a particular predicate hold of all characters in a string? *)
let string_forall (p:char -> bool) (s:string) =
  let ans = ref true
  String.iter (fun c -> if not(p c) then ans := false) s
  !ans

(* Remove the first corresponding element from a list, using the specified equality test. *)
let rec remove_first eq x ys =
  match ys with
  | [] -> []
  | y::ys' -> if (eq x y) then ys' else y::(remove_first eq x ys')

(* Check whether one list is a permutation of another, using the specified equality test. *)
(* RPL: This checks if xs has sublist xs' and xs' is permutation of ys *)
let is_permutation eq xs ys =
  let rec check xs ys =
    match xs,ys with
    | [],[] -> true
    | (x::xs),(_::_) -> let ys' = remove_first eq x ys in check xs ys'
    | _,_ -> false
  List.length xs = List.length ys && (* RLP: I think this check is necessay *)
  check xs ys

(* Take the intersection of two lists, using the given equality predicate. *)
(* More generally (eq could be any relation and it could be that 'a != 'b)
   it returns the elemensts of xs2 that have a representative in xs1 *)
let intersection (eq:'a -> 'b -> bool) (xs1:'a list) (xs2:'b list) =
  let f (x2:'b) = List.exists (fun x1 -> eq x1 x2) xs1
  List.filter f xs2

(* Checks if one multiset is a permutaion of another *)
(* Assumes that mxs and mys are without duplicates *)
let is_perm_multiset (eq:'a -> 'b -> bool) mxs mys =
  let same (sa:'a, na:int) (sb, nb) = na = nb && eq sa sb
  List.length mxs = List.length mys &&  (* same length    /\ *)
  intersection same mxs mys = mys       (* mys subset of mxs *)

(* Cons an element onto a list, if it's not there already. *)
let maybecons (eq:'a -> 'a -> bool) (x:'a) (xs:'a list) = if List.exists (fun x' -> eq x x') xs then xs else x::xs

(* Append an element onto a list, if it's not there already. *)
let maybeappend (eq:'a -> 'a -> bool) (xs:'a list) (x:'a) = if List.exists (fun x' -> eq x x') xs then xs else xs@[x]

(* Union two lists together, using the given equality predicate. *)
let union (eq:'a -> 'a -> bool) (xs:'a list) (ys:'a list) =
  let rec loop xs ys =
    match xs with
    | [] -> ys
    | (x::xs) -> loop xs (maybecons eq x ys)
  loop (List.rev xs) (remove_duplicates eq ys)

(* Collect using union. *)
let collect_union eq f xs = fold_left (fun acc x -> union eq acc (f x)) [] xs 

(* Construct a list by calling a given thunk n times. *)
let make_list (f:unit -> 'a) (n:int) =
  if n<0 then failwith "Lib.make_list: negative length" else
  let rec make = function 0 -> [] | n -> (f ())::(make (n-1))
  make n

(* Sort with a given comparision function. *)
let sort c xs = List.sortWith c xs

(* Convert a string into a char list. *)
let charlist_of_string (s:string) =
  let ans = ref []
  String.iter (fun c -> ans := !ans@[c]) s;
  !ans

(* Create a string from a character. *)
let string_of_char c = string c

let string_of_chars cs =
  let sb = Stringbuilder.empty ()
  List.iter (fun c -> Stringbuilder.append sb (string_of_char c)) cs;
  Stringbuilder.value sb

(* Splits string [s] in two around char [c] *)
let split_string c s =
  let chars = charlist_of_string s
  let rec inner seen = function
  | [] -> (List.rev seen, [])
  | x::xs -> if x = c then (List.rev seen, xs) else inner (x::seen) xs
  let before, after = inner [] chars
  (string_of_chars before, string_of_chars after)

(* Compare an option and a list for "equality". *)
let option_eq_list (xo:'a option) (xs:'a list) =
  match xo,xs with
  | None, [] -> true
  | Some x, [y] -> x=y
  | _,_ -> false

(* Identity function. *)
let id = fun x -> x

(* Map a function across (overlapping) pairs of list elements. *)
let rec map_pairs (f:('a * 'a) -> ('a * 'a)) (xs:'a list) =
  match xs with
  | [] -> []
  | [x] -> [x]
  | (x1::x2::xs) -> let (x1',x2') = f (x1,x2) in x1'::(map_pairs f (x2'::xs))

(* Are two lists disjoint? *)
let disjoint (eq:'a -> 'a -> bool) (xs:'a list) (ys:'a list) =
  forall (fun x -> not(List.exists (fun y -> eq x y) ys)) xs

(* Count number of list elements which satisfy a predicate. *)
let count (p:'a -> bool) (xs:'a list) = fold_left (fun n x -> if p x then n+1 else n) 0 xs

(* Convert an option into a list. *)
let list_of_option = function None -> [] | Some x -> [x]

(* Find the indices of list elements which match a given predicate. *)
let find_indices (p:'a -> bool) (xs:'a list) : int list =
  fst(fold_left (fun (acc,i) x -> if p x then (acc@[i], i+1) else (acc, i+1)) ([],0) xs)

(* Call a thunk n times. *)
let repeat (n:int) (f:unit -> 'a) : unit =
  let rec loop = function
    | 0 -> ()
    | n -> (ignore(f ()); loop(n-1))
  loop n

(* Create a list with n copies of an element. *)
let duplicate (n:int) (x:'a) =
  let rec f n xs = if (n<=0) then xs else f (n-1) (x::xs)
  f n []

(* Compute all combinations *)
let rec combinations = function
  | [] -> [[]]
  | l::ls ->
    let combs = combinations ls
    List.collect (fun x -> List.map (fun c -> x::c) combs) l

(* Find the index of a list element. *)
let rec find_index (x:'a) (zs:'a list) =
  let rec loop (n:int) (zs:'a list) =
    match zs with
    | [] -> failwith "findIndex: not found"
    | z::zs -> if x=z then n else loop (n+1) zs
  loop 0 zs

(* ******************************************************************* *)
(* Functions for splitting lists. *)
  
(** Returns all combinations of the sequences to the left and right of all occurences of domain n inside sequence ns *)
(** if ns = left@[n]@right then (left,right) appears in the resulting list. *)
let split_around_element (n:'a) (ns:'a list) : ('a list * 'a list) list =
  let rec split_around (acc:'a list) (n:'a) (ns:'a list) (accu:(('a list * 'a list) list)) =
    match ns with
    | [] -> accu
    | n'::ns -> if n=n' then split_around (acc@[n']) n ns ((acc,ns)::accu) else split_around (acc@[n']) n ns accu
  split_around [] n ns []

(** Returns all combinations of the sequences to the left and right of all occurences of a sequence s inside sequence s' *)
(** if s' = l@s@r then (left,right) appears in the resulting list. *)
let split_around_sublist (s:'a list) (*appearing within*) (s':'a list) : ('a list * 'a list) list =
  (* If ys = xs@zs then return zs. *)
  let rec match_initial (xs:'a list) (ys:'a list) =
    match (xs,ys) with
    | [],ys -> Some ys
    | (x::xs),(y::ys) -> if x=y then match_initial xs ys else None
    | _,_ -> None

  let rec find (acc:('a list * 'a list) list) (l:'a list) (s':'a list) =
    match s' with
    | [] -> acc
    | (n::ns) ->
        match (match_initial s s') with
          Some r -> find (acc@[(l,r)]) (l@[n]) ns
        | None -> find acc (l@[n]) ns

  find [] [] s'

(** Splits the list ns into left,right at the given position *)
(** If ns = left@right and position = (List.length left) then left,right else ns,[] *)
let split_at (position:int) (ns:'a list) =
  let rec split_at (accu:'a list) (position:int) (ns:'a list) =
    match ns with
    | [] -> accu,[]
    | n::ns -> 
      if position <= 0 then accu,(n::ns)
      else split_at (accu@[n]) (position-1) ns
  split_at [] position ns

(** Looks to see if the given list is at the right-hand side. *)
(** If ns = left@r then Some(left,r) *)
(** NB may behave strangely if given empty arguments. *)
let sub_right (r:'a list) (ns:'a list) =
  let (left:'a list), (right:'a list) = split_at (List.length ns - List.length r) ns
  if right=r then Some(left,r) else None

(** Looks to see if the given list is at the left-hand side. *)
(** If ns = l@right then Some(l,right) *)
(** NB may behave strangely if given empty arguments. *)
let sub_left (l:'a list) (ns:'a list) = 
  let (left:'a list), (right:'a list) = split_at (List.length l) ns
  if left=l then Some(l,right) else None

(* Separate a list into a list of singleton lists. *)
let rec separate (xs:'a list) : 'a list list =
  match xs with
  | [] -> [[]]
  | [x] -> [[x]]
  | (x::xs) -> [x]::(separate xs)

(* "Glue" two lists of lists together, appending the last list of the first argument with the first list of the second. *)
let rec glue (xs:'a list list) (ys:'a list list) : 'a list list =
  match xs,ys with
  | [],ys -> ys
  | xs,[] -> xs
  | [x],(y::ys) -> (x@y)::ys
  | (x::xs),(y::ys) -> x::(glue xs (y::ys))

(* Merge a second map over a first *)
#if JavaScript
// Workaround for W# bug https://github.com/dotnet-websharper/core/issues/1094. This conditional compilation can be removed once we upgrade to a version of W# that has a fix.
let update_map (a:Map<'k,'v>) (b:Map<'k,'v>) : Map<'k,'v> =
    let map = new Map<'k,'v>([])
    let map = Map.fold (fun m k v -> Map.add k v m) map a
    let map = Map.fold (fun m k v -> Map.add k v m) map b
    map
#else
let update_map a b = Map(Seq.concat [ (Map.toSeq a) ; (Map.toSeq b) ])
#endif

(* Contextual versions of map and collect. The higher-order argument also takes the context of the rest of the list as parameters. *)
let map_contextual (f:'a list -> 'a -> 'a list -> 'b) (xs:'a list) : 'b list =
  let rec loop (acc:'b list) (lhs:'a list) (rhs:'a list) =
    match rhs with
    | [] -> acc
    | x::rhs -> loop (acc@[f lhs x rhs]) (lhs@[x]) rhs
  loop [] [] xs
let collect_contextual (f:'a list -> 'a -> 'a list -> 'b list) (xs:'a list) : 'b list =
  let rec loop (acc:'b list) (lhs:'a list) (rhs:'a list) =
    match rhs with
    | [] -> acc
    | x::rhs -> loop (acc@(f lhs x rhs)) (lhs@[x]) rhs
  loop [] [] xs

(* Find commonality at the head of two lists. *)
(*let head_commonality xs ys =
  let rec loop common xs ys = match (xs,ys) with
    | [],[] -> (common,[],[])
    | [],ys_rest -> (common,[],ys_rest)
    | xs_rest,[] -> (common,xs_rest,[])
    | (x::xs),(y::ys) -> if x=y then loop (common@[x]) xs ys else (common,(x::xs),(y::ys))
  in
  loop [] xs ys

(* Find commonality at the tail of two lists. *)
let tail_commonality xs ys =
  let (common, xs_rest, ys_rest) = head_commonality (List.rev xs) (List.rev ys) in
  (List.rev common, List.rev xs_rest, List.rev ys_rest)*)

(* Evaluate a map function on an option type, returning a specified value in the case of "None". *)
let option_map (f:'a -> 'b) (ifnone:'b) (x:'a option) : 'b =
  match x with
  | None -> ifnone
  | Some x -> f x

(* Special case of "option_map" where the target type is also an option type. *)
let option_map2 (f:'a -> 'b) (x:'a option) : 'b option = option_map (fun x -> Some(f x)) None x

let option_and a l = fold_left (fun acc f -> option_map f None acc) (Some a) l
let rec option_and2 f a l1 l2 =
  match l1, l2 with
  | [], [] -> Some a
  | x::xs, y::ys ->
    (match f a x y with
     | None -> None
     | Some a -> option_and2 f a xs ys)
  | _ -> None

let rec option_or = function
  | [] -> None
  | (Some x)::_ -> Some x
  | None::xs -> option_or xs

(* Apply a function just to the first / last element of a list. *)
let map_first (f:'a -> 'a) (xs:'a list) =
  match xs with
  | [] -> []
  | x::xs -> (f x)::xs
let rec map_last (f:'a -> 'a) (xs:'a list) =
  match xs with
  | [] -> []
  | [x] -> [f x]
  | x::xs -> x::(map_last f xs)

(* Return the last element of a list, which is assumed to be non-empty. *)
let last (xs:'a list) = List.head(List.rev xs)

(* Return the head of a list, checking for non-emptyness! *)
let tryHead (xs:'a list) = match xs with [] -> None | _ -> Some(List.head xs)

(* Return the last element of a list, checking for non-emptyness! *)
let tryLast (xs:'a list) = match xs with [] -> None | _ -> Some(last xs)

(* Return the first "Some" element found. *)
let lookForSome (f:'a -> 'b option) (xs:'a list) : 'b option =
  let rec loop xs =
    match xs with
    | [] -> None
    | (x::xs) -> (match f(x) with None -> loop xs | Some z -> Some z)
  loop xs

(* String replacing function. *)
let string_replace (str:string) (old_str:string) (new_str:string) : string = str.Replace(old_str, new_str)

(* Set up the "newline" string. *)
 
#if JavaScript
let newline = "\r\n"
#else
let newline = System.Environment.NewLine
#endif

(* Quote a string. *)
let quote str = "\"" + str + "\""

(* Parenthesise a string. *)
let paren str = "(" + str + ")"

(* Put a string in square brackets. *)
(* CG: Perhaps name the method sq_brack? *)
let brack str = "[" + str + "]"

(* Put a string in curly braces. *)
let brace str = "{" + str + "}"

(* Subtract one list from another. *)
let difference (eq:'a -> 'a -> bool) (xs:'a list) (* - *) (ys:'a list) : 'a list =
  let rec loop (acc:'a list) (xs:'a list) =
    match xs with
    | [] -> acc
    | (x::xs) -> if List.exists (fun x' -> eq x x') ys then loop acc xs else loop (acc@[x]) xs
  loop [] xs

let list_diff rel xs ys =
  let in_ys x = List.exists (fun y -> rel x y) ys
  List.filter (fun x -> not (in_ys x)) xs

(* List map, but ignore any "None"s that appear. *)
let maybemap (f:'a -> 'b option) (xs:'a list) : 'b list =
  let rec loop (ys:'b list) (xs:'a list) =
    match xs with
    | [] -> ys
    | (x::xs) -> (match f x with None -> loop ys xs | Some y -> loop (ys@[y]) xs)
  loop [] xs

(* Produce all permutations of a list. Since this grows very rapidly with the size of the input list
   (as a factorial?) we might consider writing an alternative version which returns lazily as a stream... *)
let rec permutations (xs:'a list) : 'a list list =
  let rec insertAll (x:'a) (ys:'a list) : 'a list list =
    match ys with
    | [] -> [[x]]
    | (y::ys) -> (x::y::ys)::(List.map (fun zs -> y::zs) (insertAll x ys))
  match xs with
    | [] -> [[]]
    | (x::xs) -> collect (insertAll x) (permutations xs)

(* An "either" type. *)
type ('a,'b) either = This of 'a | That of 'b

(* OCaml and F# display floats differently! In F#, we add a '.0' at the end of floats that are whole numbers. This is a hack... *)
let display_float (f:float) : string =
  if f = infinity then "Infinity"
  else if f = nan then "NaN"
  else if System.Double.IsNaN f then "NaN"
  else
    #if JavaScript
    let s:string = f.ToString()
    #else
    let s:string = f.ToString(System.Globalization.CultureInfo.InvariantCulture)
    #endif
    if (s.Contains ".") || (s.Contains "e") || (s.Contains "E") then s else s + ".0"

(* Produce the cartesian product of two lists. *)
let cartesian (xs:'a list) (ys:'b list) : ('a * 'b) list =
  let rec loop acc (xs:'a list) =
    match xs with
    | [] -> acc
    | (x::xs) ->
        let rec loop2 acc (ys:'b list) =
          match ys with
          | [] -> acc
          | (y::ys) -> loop2 ((x,y)::acc) ys
        let acc = loop2 acc ys
        loop acc xs
  List.rev (loop [] xs)

(* Produce the upper triangle of the cartesian product of a list with itself, excluding the diagonal *)
let rec cart_ord = function
  | [] -> []
  | x::xs -> 
    (List.map (fun y -> x, y) xs)
    @ cart_ord xs

let fac n = fold_left (*) 1 [2..n]

#if JavaScript
//WebSharper lacks the checked conversion and ideally we'd use in64 but we lack int64_of_float elsewhere
let binom (n:int) (k:int) : int = int32(List.fold (fun s (i) -> s * (int64(n)-i+1L)/i ) 1L [1L..int64(k)])
#else
let binom (n:int) (k:int) : int = Checked.int32(List.fold (fun s (i) -> s * (int64(n)-i+1L)/i ) 1L [1L..int64(k)])
#endif

//let binom (n:int64) (k:int64) : int64 = List.fold (fun s (i) -> s * (n-i+1L)/i ) 1L [1L..k]

(* RLP: Next three functions shall fall to kill-append, but possibly before... *)
(* Only apply map function to list elements satisfying the predicate. *)
let selective_map (f:'a -> 'a) (p:'a -> bool) (xs:'a list) =
  let rec loop acc xs =
    match xs with
    | [] -> acc
    | (x::xs) -> if p x then loop (acc@[f x]) xs else loop (acc@[x]) xs
  loop [] xs

(**********************************************************************)
(* Produce string output either in "text" or "html" mode. *)

let symbol (html:bool) (s:string) = if html then "<font color=#990000>" + s + "</font>" else s 
let string (html:bool) (s:string) = if html then "<font color=#009900>\"" + s + "\"</font>" else "\"" + s + "\"" 
let char (html:bool) (s:string) = if html then "<font color=#009900>'" + s + "'</font>" else s 
let var (html:bool) (s:string) = if html then "<font color=#3333ff>" + s + "</font>" else s
let keyword (html:bool) (s:string) = if html then "<font color=#000075><b>" + s + "</b></font>" else s (*#ff6600 *)
let newlineHtml (html:bool) = if html then "<br>" else newline
let arrow (html:bool) = if html then symbol html " -&gt; " else " -> "
let space (html:bool) = if html then "&nbsp;" else " "

(**********************************************************************)
(* Functions for manipulating counters. *)

let create_counter () : int ref = ref 0
let reset_counter (c:int ref) : unit = c := 0
let fresh_id (c:int ref) : int =
  let id = !c
  incr c
  id

(**********************************************************************)
(* Linear interpolation between of values list in points list at grid list*)
exception InvalidListLength of string

let lin_interp (x0: float) (x1 : float) (y0 : float) (y1 : float) (x : float) =
    y0 + (y1-y0)*(x-x0)/(x1-x0)

let get_segment (value: float) (points : float list) (last_index : int) =
    let ar = Array.rev (List.toArray points)
    let mutable i = last_index
    while (i < ar.Length-1) && (ar.[i] <= value) do 
      i <- i+1
    i
    
let interp_on_grid(values : float list) (points: float list) (grid: float list) =
    let val_ar =Array.rev (List.toArray values)
    let points_ar = Array.rev (List.toArray points)
    if(not (List.length values = List.length points)) then  raise(InvalidListLength("Values and points lists must have equal length"))
    else 
        let p = ref 0
        List.map(fun x ->  p := get_segment x points !p; lin_interp points_ar.[!p-1] points_ar.[!p] val_ar.[!p-1] val_ar.[!p] x) grid

(* Define some 3-tuple accessor methods *)
let fst3 (x1,_,_) = x1
let snd3 (_,x2,_) = x2
let trd3 (_,_,x3) = x3


(* Andrew *)

let rec each n inputList lstlst lst i =
  match inputList with
  | [] -> List.rev lst::lstlst
  | cur::inputList ->
      if i = n 
      then each n inputList (List.rev lst::lstlst) [cur] 1   (* ND: Added a reverse to preserve ordering *) 
      else each n inputList lstlst (cur::lst) (i+1)
let split n inputList = each n inputList [] [] 0 |> List.rev (* CG: Added a reverse to preserve ordering *)

let check64bit () =
#if JavaScript
    ()
#else
    if System.IntPtr.Size = 4 then failwith "You must run as a 64bit as that's what we build native dependencies for."
#endif

(* Helper functions for emitting data structures *)
(* Should possibly be part of the parser combinator library *)
let emit_record default_value fields r =
  let lookup (field_name, field) =
    if field r = field default_value then None
    else sprintf "%s=%s" field_name (field r) |> Some
  fields
  |> List.choose lookup
  |> String.concat "; "
  |> sprintf "{%s}"