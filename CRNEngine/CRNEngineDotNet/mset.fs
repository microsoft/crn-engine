// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Mset

type entry<'a> = {
  element:'a;
  multiplicity:int
}

type 'a t = entry<'a> list //List is assumed to be without duplicates

let empty = [] : 'a t

let rec rev_map_acc f l acc =
    match l with 
    | [] -> acc
    | h::t -> rev_map_acc f t (f h :: acc)

let rev_map f l = rev_map_acc f l []

let from_list xs =
  let counts = Hashtable.empty ()
  let count_and_remove l element =
    match Hashtable.tryFind counts element with
      | None -> Hashtable.add counts element 1
                element::l
      | Some n -> Hashtable.add counts element (n+1)
                  l
  let uniques = Lib.fold_left count_and_remove [] xs
  rev_map (fun element -> {element=element; multiplicity=Hashtable.find counts element}) uniques 

let on_e f entry = f entry.element

let fold_left f acc mset = Lib.fold_left (fun a -> on_e (f a)) acc mset
let fold_left_m f acc (mset: 'a t) = Lib.fold_left f acc mset

let iter f mset = List.iter (on_e f) mset
let iterm f (mset: 'a t) = List.iter f mset

(* returns the elements with multiplicity *)
let to_list (mset: 'a t) = 
  let rec add_duplicates l (x: 'a) = function
    | 0 -> l
    | n -> add_duplicates (x::l) x (n-1) in
  let add_mult l (e:entry<_>) = add_duplicates l e.element e.multiplicity in
  let l = fold_left_m add_mult [] mset in
  List.rev l

let to_mlist (mset: 'a t) = mset
let to_map (mset: 'a t) = mset |> List.map(fun entry -> entry.element, entry.multiplicity) |> Map.ofList

let add (eq:'a -> 'a -> bool) (xs:'a entry list) (xm:'a entry) : entry<'a> list =
  let rec add new_entry = function
    | [] -> [new_entry]
    | entry::l -> if eq entry.element new_entry.element then
                    {element=entry.element; multiplicity=entry.multiplicity + new_entry.multiplicity}::l
                  else
                    entry::add new_entry l
  add xm xs

let from_mlist xs = Lib.fold_left (add (=)) empty xs

let union eq xs ys = Lib.fold_left (add eq) xs ys
let union_all eq msets = Lib.fold_left (union eq) empty msets


let forall f mset = List.forall f mset

let is_in_mys eq mys other_entry =
  match List.tryFind (fun entry -> eq other_entry.element entry.element) mys with
  | None -> false
  | Some entry -> other_entry.multiplicity <= entry.multiplicity
let is_contained (eq:'a -> 'b -> bool) (mxs: 'a t) (mys: 'b t) =
  forall (is_in_mys eq mys) mxs

let intersection eq xs ys = Lib.fold_left (fun acc y -> if is_in_mys eq xs y  
                                                          then add eq acc y
                                                          else acc ) empty ys

let is_perm (eq:'a -> 'a -> bool) (mxs: 'a t) (mys: 'a t) =
  is_contained eq mxs mys && is_contained eq mys mxs

let get_mult (eq:'a -> 'a -> bool) mset e =
  match List.tryFind (fun entry -> eq e entry.element) mset with
  | None -> 0
  | Some found_entry -> found_entry.multiplicity

(* returns the elements without multiplicity *)
let elements (mset: 'a t) = mset |> List.map (fun entry -> entry.element)

(* returns the elements without multiplicity *)
let nonzero_elements (mset: 'a t) =
  let chooser (entry:entry<'a>) = if entry.multiplicity = 0 then None else Some entry.element in
  List.choose chooser mset

let size mset = fold_left_m (fun s entry -> s+entry.multiplicity) 0 mset
let nr_uniques (mset: 'a t) = List.length mset

let to_string e_to_string sep mset =
  let sn_to_string = function
    | {multiplicity=0} -> ""
    | {multiplicity=1; element=s} -> e_to_string s
    | {multiplicity=n; element=s} -> (string n) (*^ "*"*) + (e_to_string s) in  
  Lib.string_of_list sn_to_string sep mset

let to_string_m e_to_string sep (mset: 'a t) = Lib.string_of_list e_to_string sep mset

let is_empty : 'a t -> bool = function [] -> true | _ -> false

let map f mset = List.map (fun entry -> {element=f entry.element; multiplicity=entry.multiplicity}) mset
let mapm (f: entry<'a> -> entry<'b>) mset = List.map f mset

let filter p mset = List.filter (on_e p) mset
let partition p mset = List.partition (on_e p) mset

let collect f mset = to_list (map f mset)
let collectm (f:(entry<'a> -> 'b)) (mset: 'a t) = List.map f mset

let natminus a b = if b > a then 0 else a-b
let minus (eq:'a -> 'a -> bool) (mxs: 'a t) (mys: 'a t) =
  let decrease entry = {element=entry.element; multiplicity=natminus entry.multiplicity (get_mult eq mys entry.element)}
  mapm decrease mxs

let difference (eq:'a -> 'a -> bool) (mxs: 'a t) (mys: 'a t) =
  let sub entry = {element=entry.element; multiplicity=entry.multiplicity - (get_mult eq mys entry.element)} in
  let cliff = mapm sub mxs in
  let sea = filter (fun y -> get_mult eq mxs y = 0 ) mys in
  cliff @ (mapm (fun entry -> {element=entry.element; multiplicity= -entry.multiplicity}) sea)

let ( |>> ) a b c = Parser.( |>>) a b c
let ( .>>. ) a b c = Parser.( .>>. ) a b c
let ( +>>+ ) a b c = Parser.( +>>+ ) a b c
let ( .>> ) a b c = Parser.( .>> ) a b c
let ( >>. ) a b c = Parser.( >>. ) a b c
let ( <|> ) a b c = Parser.( <|> ) a b c
let counted p =
  p |>> (fun s -> {element=s;multiplicity=1})
  <|>
  ((Parser.pint32 (* .>> Parser.skw "*" *)) +>>+ p |>> fun (n,s) -> {element=s;multiplicity=n})
let parse p = Parser.sepBy (counted p .>> Parser.spaces) (Parser.skw "+") |>> from_mlist
