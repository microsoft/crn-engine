[<JavaScript>]
module Microsoft.Research.CRNEngine.IPQ

(* A treap is either empty or a node with children and a priority *)
(* Treap(priority, left, value, right) *)

(* Invariant:
        1) smaller priorities nearer to root
        2) left values smaller than root smaller than right values (for all subtrees)
*)
type t<'prio, 'a> =
    | Empty
    | Treap of 'prio * t<'prio, 'a> * 'a * t<'prio, 'a>

let empty = Empty

let default_comp = compare

exception Not_found

let rec find_c comp value = function
    | Empty -> raise Not_found
    | Treap(p, left, v, right) ->
        let c = comp value v in
        if c < 0 then find_c comp value left else
        if c > 0 then find_c comp value right else
        p,v

let find v = find_c default_comp v

let get_next = function
    | Empty -> raise Not_found
    | Treap(p, _, v, _) -> p,v

(* Adding a known value overwrites the priority *)
(* Smallest priority at root *)
let add_c comp (prio, value) t =
  let rec add_inner =
    function
    | Empty ->
        (prio, Empty, value, Empty)
    | Treap(p, left, v, right) ->
        let c = comp value v in
        if c < 0 then
            let (lp, ll, lv, lr) as new_left = add_inner left
            if lp < p then (lp, ll, lv, Treap (p, lr, v, right))
            else (p, Treap new_left, v, right)
        else if c > 0 then
            let (rp, rl, rv, rr) as new_right = add_inner right
            if rp < p then (rp, Treap (p, left, v, rl), rv, rr)
            else (p, left, v, Treap new_right)
        else (p, left, value, right)
  Treap (add_inner t)

let add pv t = add_c default_comp pv t

(* Assumes all values of m1 < all values of m2 *)
(* as is the case for results of split *)
let rec join m1 m2 =
    match m1, m2 with
    | Empty, m
    | m, Empty -> m
    | Treap(p1, left, v, right), Treap(p2, _, _, _) when p1 > p2 ->
        Treap(p1, left, v, join right m2)
    | _, Treap(p2, left, v, right) ->
        Treap(p2, join m1 left, v, right)

let remove_next = function
    | Empty -> raise Not_found
    | Treap(p, left, v, right) -> (p,v), join left right

let rec split_c comp value = function
    | Empty -> (Empty, Empty)
    | Treap(p, left, v, right) ->
        let c = comp value v in
        if c < 0 then
            let (m1, m2) = split_c comp value left
            (m1, Treap (p, m2, v, right))
        else
            if c > 0 then
                let (m1, m2) = split_c comp value right
                (Treap (p, left, v, m1), m2)
            else
                (left, right)

let split v t = split_c default_comp v t

(* Removing from the empty map gives the empty map *)
(* in accordance with the OCaml Map module *)
let remove_c comp value m =
    let (m1, m2) = split_c comp value m in
    join m1 m2

let remove v t = remove_c default_comp v t

let replace_c comp (prio, value) m = add_c comp (prio, value) (remove_c comp value m)

let replace pv t = replace_c default_comp pv t

let rec map f = function
    | Empty -> Empty
    | Treap(p, left, v, right) -> Treap(p, map f left, f v, map f right)

let rec fold f m init =
    match m with
    | Empty -> init
    | Treap(_, left, v, right) ->
        let rl = fold f left init in
        fold f right (f v rl)

let filter pred = function
    | Treap(_, left, v, right) when not (pred v) -> join left right
    | m -> m

let replace_all_c comp t' t = fold (replace_c comp) t' t

let replace_all t' t = replace_all_c default_comp t' t

let from_list_c comp l = List.fold (fun t pv -> add_c comp pv t) empty l

let from_list l = from_list_c default_comp l
