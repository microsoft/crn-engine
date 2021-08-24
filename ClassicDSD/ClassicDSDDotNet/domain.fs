// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Domain
open Microsoft.Research.DNA

module Lib = Microsoft.Research.CRNEngine.Lib
module Prim = Microsoft.Research.CRNEngine.Expression
module Stringmap = Microsoft.Research.CRNEngine.Stringmap
module Hashtable = Microsoft.Research.CRNEngine.Hashtable

type prim = Microsoft.Research.CRNEngine.Expression.t<string>

(* A domain is the basic building block of a DNA sequence *)
type value = Value.t
type value_env = Value.t_env
type match_env = (string * bool) Stringmap.t
type tag = string * float
type tether = (tag list * int) option
type t = 
  | Toe of value*value*bool*tether
  | Normal of value*bool*tether

(* Mapping list of domains to the domain they become when concatenated *)
type sub_map = Microsoft.Research.CRNEngine.Hashtable.t<string list, t>

(* Complement a domain. *)
let complement = function Toe(v,r,cmp,teth) -> Toe(v,r,(not cmp),teth) | Normal(v,cmp,teth) -> Normal(v,(not cmp),teth)

(* Put a "star" onto a domain. *)
let star = function Toe(v,r,_,teth) -> Toe(v,r,true,teth) | Normal(v,_,teth) -> Normal(v,true,teth)

(* Remove any "star" from a domain. *)
let unstar = function Toe(v,r,_,teth) -> Toe(v,r,false,teth) | Normal(v,_,teth) -> Normal(v,false,teth)

let get_value = function Toe(v,_,_,_) | Normal(v,_,_) -> v

(* Is a given domain complemented? *)
let is_complemented = function Toe(_,_,cmp,_) | Normal(_,cmp,_) -> cmp

(* Is a given domain tethered to an origami?*)
let is_tethered = function Toe(_,_,_,Some (_,i)) | Normal(_,_,Some (_,i)) -> true (*i > -2*) | _ -> false

let get_tags = function
  | Toe(_,_,_,Some (t, _))
  | Normal(_,_,Some (t, _)) -> Some t
  | _ -> None

let get_tethered_tags = function
  | Toe(_,_,_,Some(t,n))
  | Normal(_,_,Some(t,n)) when n >= 0 -> Some t
  | _ -> None

let tags_intersect to1 to2 =
  match to1, to2 with
  | Some t1, Some t2 -> Some (Lib.intersection (=) t1 t2)
  | _ -> None

let rec tags_in_common_list = function
  | [] -> None
  | [d] -> get_tags d
  | d::ds -> tags_intersect (get_tags d) (tags_in_common_list ds)

let all_gotten_tags_list get l =
  let combine a b =
    match a, get b with
    | Some la, Some lb -> Some (Lib.union (=) la lb)
    | Some l, None | None, Some l -> Some l
    | _ -> None in
  Lib.fold_left combine None l

let all_tags_list l = all_gotten_tags_list get_tags l

let all_tethered_tags_list l = all_gotten_tags_list get_tethered_tags l

let set_tags ts d =
  match d, ts with
  | Toe (v,r,c,Some (_,-2)), None      -> Toe (v,r,c,None)
  | Toe (v,r,c,Some (_,-2)), Some tags -> Toe (v,r,c, Some (tags,-2))
  | Toe (v,r,c,None),        Some tags -> Toe (v,r,c,Some (tags,-2))
  | Normal (v,c,Some (_,-2)), None      -> Normal (v,c,None)
  | Normal (v,c,Some (_,-2)), Some tags -> Normal (v,c,Some (tags,-2))
  | Normal (v,c,None),        Some tags -> Normal (v,c,Some (tags,-2))
  | _ -> d

let set_tags_list ts l = List.map (set_tags ts) l

let add_tags ts d =
  match d, ts with
  | _, None -> d
  | Toe (v,r,c,None), Some tags -> Toe (v,r,c,Some (tags,-2))
  | Normal (v,c,None), Some tags -> Normal (v,c,Some (tags,-2))
  | Toe (v,r,c,Some (ts1,-2)), Some tags -> Toe (v,r,c,Some (Lib.union (=) ts1 tags,-2))
  | Normal (v,c,Some (ts1,-2)), Some tags -> Normal (v,c,Some (Lib.union (=) ts1 tags,-2))
  | _ -> d

let add_tags_list ts l = List.map (add_tags ts) l

(* Given two domains, are they the complements of each other? *)
(*AP relaxed the constraint that degree of complementarity should be the same *)
let are_complements (d1:t) (d2:t) =
  match d1,d2 with
  | Toe(v1,_,cmp1,_), Toe(v2,_,cmp2,_) -> (v1=v2) (*&& (r1=r2)*) && ((not cmp1)=cmp2)
  | Normal(v1,cmp1,_), Normal(v2,cmp2,_) -> (v1=v2) && ((not cmp1)=cmp2)
  | _,_ -> false

(* Update the tether of a domain *)
let update_tether (d:t) (teth:tether) =
  match d with
  | Toe(v,r,cmp,_) -> Toe(v,r,cmp,teth)
  | Normal(v,cmp,_) -> Normal(v,cmp,teth)   

(* Given two domains, "stick" them together without losing the relevant anchor information *)
(* AP: Assumes that domains are complementary, though this is not checked here *)
let stick (d1:t) (d2:t) = 
  let bond_tethers (t1:tether) (t2:tether) =
    match t1,t2 with
    | Some(t1,i1),Some(_,i2) -> Some(t1,Operators.min i1 i2) (* RLP: This should probably not happen, if we use the convention of a special "tether" domain *)
    | Some i1,None       -> Some i1
    | None,Some i1       -> Some i1
    | _                     -> None
  in
  match d1,d2 with (* Assumes d1 and d2 are identical apart from their tether and degree of complementarity *)
  (* Retain degree of complementarity from complementary strand. Seems to require the first domain to be used. *)
  | Toe(n1,_,c1,teth1),Toe(_,r2,c2,teth2) -> update_tether (if c2 then (Toe(n1,r2,c1,teth1)) else d1) (bond_tethers teth1 teth2)
  (*| Toe(_,_,_,teth1),Toe(_,_,_,teth2)*)
  | Normal(_,_,teth1),Normal(_,_,teth2) -> update_tether d1 (bond_tethers teth1 teth2) 
  | _ -> d1

(*Given one 'bonded' domain, unstick into two with appropriate tethers and complements*)
let unstick (d:t) =
  match d with
  | Toe(n,r,c,teth) -> (* AP: Keep the degree of complementarity only on the complementary toehold *)
      if c then d,Toe(n,Value.Float (1.0, None),not c,teth) else Toe(n,Value.Float (1.0, None),c,teth),Toe(n,r,not c,teth)
  (*| Toe(_,_,_,teth) *)
  | Normal _ -> d, complement d
let unstick_list (ds:t list) = List.unzip (List.map unstick ds)  

(* Same as unstick, but keeps the degree of complementarity; used for SVG rendering purposes *)
let unstick_keep_doc (d:t) =
  match d with
  | Toe(n,r,c,teth) ->
      if c then d,Toe(n,r,not c,teth) else Toe(n,r,c,teth),Toe(n,r,not c,teth)
  | Normal _ -> d, complement d
let unstick_keep_doc_list (ds:t list) = List.unzip (List.map unstick_keep_doc ds)  

(* Check two domains for equality, ignoring whether or not they're tethered *)
let equal (d1:t) (d2:t) =
  match d1,d2 with
  | Toe(v1,r1,cmp1,_), Toe(v2,r2,cmp2,_) -> (v1 = v2) && (r1=r2) && (cmp1=cmp2)
  | Normal(v1,cmp1,_), Normal(v2,cmp2,_) -> (v1 = v2) && (cmp1=cmp2)
  | _ -> false

(* Determine if d1 is less than d2 : 
   all Toeholds are less than all Normal domains 
   Note: may be better to lexicographically sort based on the nucleotides *)
let compare (d1:t) (d2:t) =
  match d1,d2 with
  | Toe _,Normal _ -> -1
  | Normal _, Toe _ -> 1
  | Toe(v1,_,c1,_), Toe(v2,_,c2,_)
  | Normal(v1,c1,_), Normal(v2,c2,_) -> 
     let lex = compare (Value.to_string v1) (Value.to_string v2)
     if lex=0 then 
       if c1 && c2 then 0 
       else if c1 then 1 else -1 
     else
       lex

(*Compare two lists of domains *)
let compare_domains l1 l2 = 
  let len1, len2 = List.length l1, List.length l2 in
  if len1 < len2 then -1
  else if (len1 = len2) 
      then Lib.fold_right (fun (d1,d2) eq -> if eq=0 then compare d1 d2 else eq) (Lib.zip l1 l2) 0
      else 1

let display_doc = function
  | Value.Float (1.0,_) -> ""
  | Value.Int (1,_) -> ""
  | v -> "(" + Value.to_string v + ")"

let display_doc_picture = function
  | Value.Float (1.0,_) -> ""
  | Value.Int (1,_) -> ""
  | v -> Value.to_string v

(* Display a domain, ignoring tethering *)
let display_bare_domain (n:t) =
  match n with
  | Toe(v,r,cmp,_) ->  Value.to_string v + "^" + (display_doc r) + (if cmp then "*" else "")
  | Normal(v,cmp,_) -> Value.to_string v + (if cmp then "*" else "")

let display_tag = function
  | (s, 1.0) -> s
  | (s,f) -> "(" + s + "," + Lib.display_float f + ")"

let display_tether = function
  | None -> ""
  | Some (ts,_) -> "(" + String.concat ", " (List.map display_tag ts) + ")"

(* Display a domain for a picture (no details), showing a tether only when 'teth' is on. *)
let display_picture (n:t) (teth :bool) =
  match n with
  | Normal(Value.Domain ("tether",_,_,_,_),false,teth) -> (*"tether" +*) display_tether teth
  | Toe(v,r,cmp,mt) -> (if teth && Option.isSome mt then "!" else "") + Value.to_string v + (if cmp then "*" + (display_doc_picture r) else "")  (* N^c *)
  | Normal(v,cmp,mt) -> (if teth && Option.isSome mt then "!" else "") + Value.to_string v + (if cmp then "*" else "") (* N  *)

(* Returns the length in characters of the domain, as if it was complemented. *)
let display_length (n:t) =
  match n with
  | Normal(Value.Domain ("tether",_,_,_,_),false,teth) -> display_tether teth
  | Toe(v,r,cmp,mt) -> (if Option.isSome mt then "!" else "") + Value.to_string v + (display_doc_picture r)
  | Normal(v,cmp,mt) -> (if Option.isSome mt then "!" else "") + Value.to_string v
  |> String.length

let display_tether_debug = function
  | None -> ""
  | Some (ts,i) -> "!<" + string i + ">(" + Lib.string_of_list display_tag "," ts + ")"

(* String representation of a domain (with details). *)
let display (n:t) =
  match n with
  | Normal(Value.Variable ("tether",_),false,teth)
  | Normal(Value.Domain   ("tether",_,_,_,_),false,teth) -> "tether" + display_tether teth
  | Toe(v,r,cmp,teth) -> display_tether teth + Value.to_string v + "^" + (display_doc r)(* N^c *) + (if cmp then "*" else "") 
  | Normal(v,cmp,teth) -> display_tether teth + Value.to_string v + (if cmp then "*" else "")    (* N  *)

(* String representation of a sequence of domains. *)
let rec display_sequence (ns:t list) =
  match ns with
  | [] -> "_"
  | [n] -> display n
  | n::ns -> display n + " " + display_sequence ns

let eval_tag (env:value_env) (t:tag) =
  let n, f = t
  match Value.Variable (n, Types.emprange) |> Value.eval env with
  | Value.Domain (tag,_,_,_,_) -> tag, f
  | _ -> n, f

let eval_tether (env:value_env) (t:tether) =
  match t with
  | None -> None
  | Some (ts, i) -> (ts |> List.map (eval_tag env), i) |> Some

(* evaluate the values in a domain*)
let eval (env:value_env) (n:t) =
  match n with
  | Toe(v,r,cmp,teth) -> Toe ((Value.eval env v),(Value.eval env r),cmp,teth |> eval_tether env)
  | Normal(v,cmp,teth) -> Normal((Value.eval env v),cmp,teth |> eval_tether env)

(* evaluate the values in a sequence of domains *)
let eval_sequence (env:value_env) (ns:t list) = List.map (eval env) ns

(* Compute binding rate of a domain. *)
let bind_rate (n:t) =
  match n with
  | Toe(v,deg,_,_) -> Value.bind_rate v deg
  | Normal(v,_,_) -> Value.bind_rate v (Value.Float (1.0, None))

let localize_bind_rate (n:t) lc =
  match n with
  | Toe(Value.Domain(s,i,r,v,a),deg,b,c) -> Toe(Value.Domain(s,i,Prim.mul lc r,v,a),deg,b,c)
  | Toe(Value.DomainS(s,i,seq,r,v,a),deg,b,c) -> Toe(Value.DomainS(s,i,seq,Prim.mul lc r,v,a),deg,b,c)
  | Normal(Value.Domain(s,i,r,v,a),b,c) -> Normal(Value.Domain(s,i,Prim.mul lc r,v,a),b,c)
  | Normal(Value.DomainS(s,i,seq,r,v,a),b,c) -> Normal(Value.DomainS(s,i,seq,Prim.mul lc r,v,a),b,c)
  | _ -> failwith "expected domain value"

(* Compute unbinding rate of a domain. *)
let unbind_rate (n:t) =
  match n with
  | Toe(v,_,_,_) -> Value.unbind_rate v
  | Normal(v,_,_) -> Value.unbind_rate v

(* Compute free names of a domain. *)
let free_names (n:t) =
  match n with
  | Toe(v,r,_,_) -> (Value.free_names r) @ Value.free_names v
  | Normal(v,_,_) -> Value.free_names v

let inferType_doc env v =
  let t = Value.inferType env v in
  match !t with
  | Types.FloatT _ -> ()
  | Types.BottomT -> t:= Types.FloatT (Value.get_range v)
  | t -> let descr = "degree of complementarity " + (Value.to_string v) in
         Errors.type_error (Value.get_range v) (Errors.WrongType(t,[Types.FloatT(Types.emprange)],descr))

(* Infer/check type of a domain, including checking toe usage 
   May modify ref cells in the enviornment 
*)
let inferType in_origami (env:Types.type_env) (n:t) = 
  (match n with Toe(_,r,_,_) -> inferType_doc env r | _ -> ());
  let v = match n with Toe(v,_,_,_) | Normal(v,_,_) -> v in
      let not_in_origami = match in_origami with Some false -> true | _ -> false in
      if ((not_in_origami) && is_tethered n) 
      then Errors.type_error (Value.get_range v) (Errors.TetheredOutside (display n))
    else     
     let t = Value.inferType env v in
       match (!t,n) with
       | (Types.DomainT(Some(true),_),Normal _ ) | (Types.DomainT(Some(false),_),Toe _) -> () (*Checks pass*)
       | (Types.DomainT(None,_), Normal _) | Types.BottomT,Normal _ -> t:= Types.DomainT(Some(true),Value.get_range(v)) 
       | (Types.DomainT(None,_), Toe _) | Types.BottomT,Toe _ -> t:= Types.DomainT(Some(false),Value.get_range(v)) 
       | (Types.DomainT(Some(false),r), Normal _) -> Errors.type_error (Value.get_range(v)) (Errors.ToeInconsistent(r,false))
       | (Types.DomainT(Some(true),r), Toe _) -> Errors.type_error (Value.get_range(v)) (Errors.ToeInconsistent(r,true))
       | (t, Normal _) ->
           let descr = "domain " + (display n)
           Errors.type_error (Value.get_range v) (Errors.WrongType(t,[Types.DomainT(Some(true),Types.emprange)],descr))
       | (t, Toe _) ->
           let descr = "domain " + (display n)
           Errors.type_error (Value.get_range v) (Errors.WrongType(t,[Types.DomainT(Some(false),Types.emprange)],descr))

(* Get the position of this domain, for error messages *)
let getPosn = function | Toe(v,_,_,_) | Normal(v,_,_) -> Value.get_range v

(* Reset all positions (needed so positions are irrelevant in simulator etc). *)
let erasePosns = function
  | Toe(v,r,cmp,teth) -> Toe(Value.erasePosns v, Value.erasePosns r, cmp,teth)
  | Normal(v,cmp,teth) -> Normal(Value.erasePosns v, cmp,teth)

(* This allows us to track position data through module instantiations etc. *)
let replacePosns (mrs:Types.range Stringmap.t) = function
  | Toe(v,r,cmp,teth) -> Toe(Value.replacePosns mrs v, Value.replacePosns mrs r, cmp,teth)
  | Normal(v,cmp,teth) -> Normal(Value.replacePosns mrs v, cmp,teth)

(* Find out the length of a domain (from the supplied defaults). *)
let length (toehold_length:int) (specificity_length:int) = function
  | Toe(Value.DomainS(_,_,s,_,_,_),_,_,_) | Normal(Value.DomainS(_,_,s,_,_,_),_,_) -> String.length s
  | Toe _ -> toehold_length
  | Normal _ -> specificity_length

(* Does domain d' match the "pattern" d? *)
let matches d d' =
  match (d,d') with
  | (Normal(Value.Variable("_",_),_,_), _) -> true                                     (* Always match against an underscore wildcard. *)
  | (Toe(Value.Variable(x,_),_,cmp,_), Toe(Value.Domain(x',_,_,_,_),_,cmp',_))           (* Ignore the counters on domains and require that complements match. *)
  | (Toe(Value.Variable(x,_),_,cmp,_), Toe(Value.DomainS(x',_,_,_,_,_),_,cmp',_))        
  | (Normal(Value.Variable(x,_),cmp,_), Normal(Value.Domain(x',_,_,_,_),cmp',_)) 
  | (Normal(Value.Variable(x,_),cmp,_), Normal(Value.DomainS(x',_,_,_,_,_),cmp',_)) -> x=x' && cmp=cmp'
  | (Normal(Value.Domain(x,-1,_,_,_),cmp,_), Normal(Value.Domain(x',_,_,_,_),cmp',_)) | (* These cases occur when plotted species came from a module *)
    (Normal(Value.DomainS(x,-1,_,_,_,_),cmp,_), Normal(Value.DomainS(x',_,_,_,_,_),cmp',_)) |
    (Toe(Value.Domain(x,-1,_,_,_),_,cmp,_), Toe(Value.Domain(x',_,_,_,_),_,cmp',_)) |
    (Toe(Value.DomainS(x,-1,_,_,_,_),_,cmp,_), Toe(Value.DomainS(x',_,_,_,_,_),_,cmp',_)) -> x=x' && cmp=cmp'
  | (Normal(Value.Domain(x,i,_,_,_),cmp,_), Normal(Value.Domain(x',i',_,_,_),cmp',_)) | (* These cases occur when plotted species came from a module *)
    (Normal(Value.DomainS(x,i,_,_,_,_),cmp,_), Normal(Value.DomainS(x',i',_,_,_,_),cmp',_)) |
    (Toe(Value.Domain(x,i,_,_,_),_,cmp,_), Toe(Value.Domain(x',i',_,_,_),_,cmp',_)) |
    (Toe(Value.DomainS(x,i,_,_,_,_),_,cmp,_), Toe(Value.DomainS(x',i',_,_,_,_),_,cmp',_)) -> x=x' && i=i' && cmp=cmp'
  (* The "closing" code and int->variable conversion in the parser mean that all other possibilities shouldn't match. *)
  | (_,_) -> false

(* Does the list of domains ns' match the "pattern" ns? *)
let matches_list ns ns' = Lib.forall2 matches ns ns'

let matches_env env d d' =
  match d, d' with
  | (Toe(Value.Variable(x,_),_,cmp,_), Toe(Value.Domain(x',_,_,_,_),_,cmp',_))
  | (Toe(Value.Variable(x,_),_,cmp,_), Toe(Value.DomainS(x',_,_,_,_,_),_,cmp',_))        
  | (Normal(Value.Variable(x,_),cmp,_), Normal(Value.Domain(x',_,_,_,_),cmp',_)) 
  | (Normal(Value.Variable(x,_),cmp,_), Normal(Value.DomainS(x',_,_,_,_,_),cmp',_)) ->
    (match Stringmap.tryFind x env with
     | None -> Some (Stringmap.add x (x', cmp <> cmp') env)
     | Some (y,c) -> if y=x' && ((cmp <> c) = cmp') then Some env else None
    )
  | (Toe(Value.Variable(x,_),_,cmp,_), Toe(Value.Variable(x',_),_,cmp',_)) ->
    if x=x' && cmp=cmp' then Some env else None
  | _ -> if matches d d' then Some env else None
  
let rec matches_list_env env xs ys = Lib.option_and2 matches_env env xs ys

let is_wildcard = function
  | Normal(Value.Variable("_",_),_,_) -> true
  | _ -> false

(* Is a domain a toehold? *)
let is_toehold = function Toe _ -> true | _ -> false

(* Does a given domain list contain a toehold? *)
let contains_toehold (ds:t list) = List.exists is_toehold ds

(* Does a given domain list contain the given domain *)
let contains_domain (d : t) (ds: t list) = List.exists (fun d' -> d = d') ds

let tags_in_common m n = tags_in_common_list [m; n] <> Some []

(*
(* Find all complementary toehold matches between a pair of domain lists. Also return the surrounding contexts. *)
(* The returned domains are annotated with the intersection of their tags *)
let matching_toeholds (ns:t list) (ms:t list) : ((t list * (t * t list option) * t list) * (t list * (t * t list option) * t list)) list =
  let rec loop acc nsl nsr = match nsr with
    | [] -> acc
    | ((Normal(_,_,_)) as n)::nsr -> loop acc (nsl@[n]) nsr
    | ((Toe(_,_,_,_)) as n)::nsr ->
        let rec find acc' msl msr = match msr with
          | [] -> acc'
          | ((Normal(_,_,_)) as m)::msr -> find acc' (msl@[m]) msr
          | ((Toe(_,_,_,_)) as m)::msr ->
                if (are_complements m n) && tags_in_common m n
                then find (acc'@[msl, m, msr]) (msl@[m]) msr
                        else find acc' (msl@[m]) msr
        in
        let acc' =
          List.map
           (fun (msl,m,msr) ->
             let tags = tags_in_common_list [m; n] in
             (nsl,set_tags tags n,nsr),(msl,set_tags tags m,msr)
           )
           (find [] [] ms) in
        loop (acc@acc') (nsl@[n]) nsr
  in
  loop [] [] ns
*)

let get_name d =
  match get_value d with
  | Value.Domain (s, _, _, _, _)
  | Value.DomainS (s, _, _, _, _, _) -> s + (if is_complemented d then "*" else "")
  | v -> Value.to_string v

let to_string d =
  (match get_value d with
   | Value.Domain (s, _, _, _, _)
   | Value.DomainS (s, _, _, _, _, _) -> s
   | v -> (Value.to_string v))
  + (if is_complemented d then "*" else "")

let get_sequence d =
  match get_value d with
  | Value.Domain (_, _, _, _, _) -> ""
  | Value.DomainS (_, _, s, _, _, _) -> s
  | _ -> ""

(*
let sub_map = Hashtable.empty ()
let _ = Hashtable.add sub_map ["x"; "y"] (Toe (Value.Domain ("sup", 0, BMEValue.Float 42.003, BMEValue.Float 3.0042, Types.emprange),
                                          Value.Float (1.0, Types.emprange),
                                          false,
                                          None))
*)

let get_sublists_ctx l =
  let a = Array.ofList l in
  let n = Array.length a in
  let subs ln =
    let res = ref [] in
    for i = 0 to n-ln do
      res := (Array.sub a 0 i, Array.sub a i ln, Array.sub a (i+ln) (n-ln-i))::!res
    done;
    Lib.rev_map (fun (l,m,r) -> (Array.toList l, Array.toList m, Array.toList r)) !res in
  let res = ref [] in
  for ln = 1 to n do
    res := subs ln :: !res
  done;
  !res

let search_sublists_ctx f l =
  let rec loop = function
    | [] -> []
    | n::ns ->
      (match List.collect f n with
       | [] -> loop ns
       | r -> r) in
  loop (get_sublists_ctx l)

(* Find all complementary toehold matches between a pair of domain lists. Also return the surrounding contexts. *)
(* ns should be part of an upper strand and ms should be part of a lower strand. *)
let matching_toeholds sub_map (ns:t list) (ms:t list) : ((t list * (t * t list option) * t list) * (t list * (t * t list option) * t list)) list =
  let get_comp_prefix ns ms =
    let rec loop acc = function
    | [], ms -> [List.rev acc, ms]
    | _, [] -> []
    | (n::ns), (m::ms) ->
       if are_complements n m && tags_in_common m n
       then loop (m::acc) (ns, ms)
       else [] in
    loop [] (ns, ms) in
  let find (d: t) ns =
    let rec loop acc msl msr = (* quadratic for now *)
      match msr with
      | [] -> acc
      | ((Normal _) as m)::msr' -> loop acc (m::msl) msr'
      | ((Toe _) as m)::msr' ->
        let tls = get_comp_prefix ns msr in
        let acc' =
          List.map
           (fun (found,tl) ->
             let d_comp = match found with [m] -> m, None | _ -> complement d, Some found in
             (List.rev msl, d_comp, tl))
           tls in
        loop (acc@acc') (m::msl) msr' in
    loop [] [] ms in
  let rec loop acc record nsl nsr =
    let expand_finds pre post (d: t, dl: t list option) fs =
      List.map
        (fun (msl: t list,(m,ml): t*t list option,msr: t list) -> 
           let tags = tags_in_common_list [m; d]
           (nsl@pre,(set_tags tags d, dl),post@nsr),(msl,(set_tags tags m, ml),msr))
        fs in
    let search_record = function
      | (pre, [d], post) -> expand_finds pre post (d, None) (find d [d])
      | (pre, r, post) ->
        let r_names = List.map get_name r in
        let r_rot_names = List.map get_name (Lib.rev_map complement r) in
        (match Lib.option_or (List.map (Hashtable.tryFind sub_map) [r_names; r_rot_names]) with
        | None -> []
        | Some sup_d ->
          (match find sup_d r with
           | [] -> []
           | cs -> expand_finds pre post (sup_d, Some r) cs
          )
        ) in
    match nsr with
    | [] -> search_sublists_ctx search_record (List.rev record) @ acc
    | ((Toe _) as n)::nsr -> loop acc (n::record) nsl nsr
    | ((Normal _) as n)::nsr ->
        let toes = List.rev record in
        let acc = search_sublists_ctx search_record toes @ acc in
        loop acc [] (nsl@toes@[n]) nsr
  in
  loop [] [] [] ns

(* Find a pair of neighbouring toeholds in a domain list. *)
let neighbouring_toeholds (ns:t list) =
  let rec loop ns = 
    match ns with
    | [] -> None
    | [_] -> None
    | n1::n2::ns -> if (is_toehold n1) && (is_toehold n2) then Some(n1,n2) else loop (n2::ns)
  in
  loop ns

(* Finds complemented toe holds on the same strand, and divides the strand into pre toe middle toe* post, for each pair *)
let internal_complements (ns: t list) =
  let rec matchings (pre :t list) t acc ns =
    match ns with
    | [] -> []
    | (Toe _ as tc)::ns -> if (are_complements t tc) 
                              then (List.rev pre,t,List.rev acc,tc,ns)::(matchings pre t (tc::acc) ns)
                              else matchings pre t (tc::acc) ns
    | n::ns -> matchings pre t (n::acc) ns
  in
  let rec collect_toes ns pre_acc =
    match ns with
    | [] -> []
    | (Toe _ as t)::ns -> (pre_acc,t,ns)::(collect_toes ns (t::pre_acc))
    | n::ns -> collect_toes ns (n::pre_acc)
  in Lib.fold_left (fun l a -> a@l) [] 
     (List.map (fun (p,t,ns) -> matchings p t [] ns) (collect_toes ns []))

(******************************************************************************)
let universal_counters = function
  | Toe(v,r,cmp,teth) -> Toe(Value.universal_counters v, Value.universal_counters r, cmp,teth)
  | Normal(v,cmp,teth) -> Normal(Value.universal_counters v, cmp,teth)
