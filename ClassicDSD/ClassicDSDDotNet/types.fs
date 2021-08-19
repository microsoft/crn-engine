[<JavaScript>]
module Microsoft.Research.DNA.Types
open Microsoft.Research.CRNEngine

(* Types for storing location information. Useful for error messages! *)
type pos = int * int //Microsoft.FSharp.Text.Lexing.Position
type range = (pos * pos) option (* THE OPTION LETS US ERASE THE POSITIONS SO STRUCTURAL EQUALITY CAN BE USED IN THE SIMULATOR! *)

let emprange : range = None
let mkRange (pp : pos * pos) : range = Some pp
let rangeBegin (r:range) = fst(Lib.unoption r)
let rangeEnd (r:range) = snd(Lib.unoption r)
let posLine (p:pos) = p |> fst
let posCol (p:pos) = p |> snd

(* Turn a (line,col) pairinto a string.
 * NB: by default, the line numbers start at 0 in Silverlight hence the +1. *)
let format_pos (p:pos) = 
  "Line " + (string ((posLine p) + 1 )) + ", char " + (string (posCol p))

(* Turn two positions (i.e. two (line,col) pairs) into a string. *)
let format_range (p:pos) (p':pos) =
  if ((posLine p) = (posLine p')) && ((posCol p) = (posCol p')) then
    format_pos p
  else
    begin
      if ((posLine p) = (posLine p')) && ((posCol p) <> (posCol p')) then
        (format_pos p) + " - " + (string (posCol p'))
      else
        (format_pos p) + " - " + (format_pos p')
    end

(* Type for representing the types assigned to syntax.*)
type t =
   | BottomT 
   | TiedT of t ref list * bool option * range list
   | StringT of range
   | IntT of range
   | BoolT of range
   | CharT of range
   | FloatT of range
   | DomainT of bool option * range
   | TupT of t ref list * range 
   | ModuleT of t ref list

type type_bind = (string * t ref)
type type_env = type_bind list

let update_range t r =
  match t with
  | BottomT -> BottomT
  | TiedT (tys, s, _) -> TiedT (tys, s, [r])
  | StringT _ -> StringT r
  | IntT _ -> IntT r
  | BoolT _ -> BoolT r
  | CharT _ -> CharT r
  | FloatT _ -> FloatT r
  | DomainT(t,_) -> DomainT(t,r)
  | TupT(ts,_) -> TupT(ts,r)
  | ModuleT ts -> ModuleT ts

(* compare two types *)
let rec equal (t1: t ref) (t2 :t ref) =
  System.Object.ReferenceEquals(t1, t2) ||
  match !t1,!t2 with
  | BottomT,BottomT -> true
  | TiedT(tys,b,_),TiedT(tys',b',_) -> b=b'&& Lib.forall2 equal tys tys'
  | StringT _,StringT _ | IntT _,IntT _ | BoolT _,BoolT _ | CharT _,CharT _ | FloatT _,FloatT _ -> true
  | DomainT(kind,_), DomainT(kind',_) -> kind = kind'
  | TupT(t1s,_),TupT(t2s,_) -> Lib.forall2 equal t1s t2s
  | ModuleT t1s,ModuleT t2s -> Lib.forall2 equal t1s t2s
  | _,_ -> false
  
let rec type_to_string = function
  | BottomT -> "unknown"
  | TiedT _ -> "unknown"
  | StringT _ -> "string"
  | IntT _ -> "int"
  | BoolT _ -> "bool"
  | CharT _ -> "char"
  | FloatT _ -> "float"
  | DomainT(None,_) -> "domain"
  | DomainT(Some(true),_) -> "normal domain"
  | DomainT(Some(false),_) -> "toehold domain"
  | TupT(ts,_) -> Lib.string_of_list type_to_string " * " (List.map (!) ts)
  | ModuleT ts -> Lib.string_of_list type_to_string ", " (List.map (!) ts)

let rec types_to_string ts = Lib.string_of_list type_to_string " or " ts

let is_numeric = function
  | IntT _ | FloatT _ | TiedT(_,Some(true),_)-> true
  | _ -> false

let is_num_or_bool = function
  | IntT _ | FloatT _ | BoolT _ | TiedT(_,Some _ ,_) -> true
  | _ -> false

let bindings_to_env bs = List.map (fun (n:string, _:float) -> (n, ref (FloatT None))) bs

(* These functions deal with the category encoded as a bool option in TiedT *)
let in_cat cat t =
  match cat with
  | Some b -> (b && is_numeric t) || (is_num_or_bool t)
  | None -> true

let merge_cat = function
  | (Some(b),Some(b')) -> Some(b || b')
  | Some(b),None -> Some(b)
  | None,Some(b) -> Some(b)
  | None,None -> None

let unify_possibilities rng possibles = function
  | BottomT -> possibles
  | TiedT(_,Some(true),_) -> [IntT rng;FloatT rng] |> Lib.intersection (=) possibles
  | TiedT(_,Some(false),_) -> [IntT rng; FloatT rng; BoolT rng] |> Lib.intersection (=) possibles
  | TiedT(_,None,_) -> possibles
  | t -> [t]

let rec unify rng t1 t2 =
  if System.Object.ReferenceEquals(t1, t2) then true else
  match !t1, !t2 with
  | BottomT, t -> t1 := update_range t rng; true
  | t, BottomT -> t2 := update_range t rng; true
  | TiedT(ts1,f1,_), TiedT(ts2,f2,_) -> 
    let f = merge_cat (f1,f2) in
    let tys = Lib.union (=) ts1 ts2 in
    let t = TiedT (tys, f, [rng]) in
    tys |> List.iter (fun tr -> tr := t);
    true
  | TiedT(ts, cat, _), t
  | t, TiedT(ts, cat, _) ->
    if in_cat cat t then
      let t = update_range t rng in
      ts |> List.iter (fun tr -> tr := t);
      true
    else false
  | TupT(t1s,_),TupT(t2s,_) -> Lib.forall2 (unify rng) t1s t2s
  | ModuleT t1s,ModuleT t2s -> Lib.forall2 (unify rng) t1s t2s
  | _, _ -> equal t1 t2


let rec getBeginPos t = 
  match t with
  | BottomT              -> None
  | TiedT (_, _, ranges) -> if ranges.IsEmpty then None else ranges.Head
  | StringT       range  -> range
  | IntT          range  -> range
  | BoolT         range  -> range
  | CharT         range  -> range
  | FloatT        range  -> range
  | DomainT   (_, range) -> range
  | TupT      (_, range) -> range
  | ModuleT     types    -> if types.IsEmpty then None else getBeginPos (!types.Head)