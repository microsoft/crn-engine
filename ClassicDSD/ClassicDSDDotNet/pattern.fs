// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Pattern
open Microsoft.Research.DNA

type value = Value.t

type t = 
  | Name of string
  | Pat of t list

(* Convert parameter list into a flattened string list, to remove the tuple information for substitution *)
let rec toStringList (l:t list) =
  match l with
  | [] -> []
  | Name(x):: ps -> x::toStringList ps
  | Pat(patlist) :: ps  -> List.append (["(" + (String.concat ", " (toStringList patlist)) + ")"]) (toStringList(ps))

let rec expandTuples (vs : value list) (ps : t list) =
  match (vs,ps) with
  ([],[]) -> []
  | (Value.Tuple(vs,_)::vals, Pat(ps)::parms) -> List.append (expandTuples vs ps) (expandTuples vals parms)
  | (v::vals,p::parms) -> v::expandTuples vals parms
  | (_,_) -> failwith "expandTuples called incorrectly"

let rec expandPatterns (vs : value list) (ps : t list) =
  match (vs,ps) with
  ([],[]) -> []
  | (Value.Tuple(vs,_)::vals, Pat(ps)::parms) -> List.append (expandPatterns vs ps) (expandPatterns vals parms)
  | (v::vals,p::parms) -> v::expandTuples vals parms
  | (_,_) -> failwith "expandTuples called incorrectly"

let rec toTypes = function
  | [] -> []
  | Name(x)::pats -> (ref Types.BottomT)::(toTypes pats)
  | Pat(ps)::pats -> (ref (Types.TupT(toTypes ps,Types.emprange)))::(toTypes pats)

let rec toTypeEnv ((typs: Types.t ref list),(names: t list)) =
  match (names,typs) with
  | [],[] -> []
  | Name(x)::pats,t::typs -> (x,t)::(toTypeEnv (typs,pats))
  | Pat(ps)::pats,t::typs -> 
    (match !t with Types.TupT(ts,r) -> (toTypeEnv (ts,ps))@(toTypeEnv (typs,pats))
                  | _ -> failwith "Typecheck failed" )
  | _,_  -> failwith "toTypeEnv not given lists of equal length"

let check_parameter_types (env : Types.type_env) x rng parms givens = 
   let rec type_check parm given =  
     if (Types.equal parm given) then ()
       else begin
       match (!parm,!given) with
       Types.BottomT,_ -> ()
       | p, Types.BottomT -> given := p
       | Types.TiedT(ts,b,rs),Types.TiedT(ts',b',rs') -> begin given:=Types.TiedT(ts',b,rs');
                                                               List.iter (fun t -> t:=!given) ts' end
       | Types.TiedT(ts,Some(b),rs), g -> if (b && Types.is_numeric(g)) || (Types.is_num_or_bool g) then ()
                                          else Errors.type_error rng (Errors.WrongType(!given,[!parm]," parameters "))
       | Types.TiedT(_,None,_),Types.TiedT(_,_,_) | Types.TiedT(_,None,_),Types.IntT _ | Types.TiedT(_,None,_),Types.FloatT _
       | Types.TiedT(_,None,_),Types.BoolT _ | Types.TiedT(_,None,_),Types.StringT _ -> ()
       | Types.DomainT(Some(b),_),Types.DomainT(None,_) -> given:=Types.DomainT(Some(b),rng)
       | Types.TupT(ps,_),Types.TupT(gs,_) -> if List.length ps = List.length gs
                                                 then List.iter2 type_check ps gs
                                                 else Errors.type_error rng (Errors.TupleArgs(x,List.length gs,List.length ps))
       | p,g -> Errors.type_error rng (Errors.WrongType(g,[p], " parameters "))
       end
   in
   let rec check = function
     | [],[] -> ()
     | p::parms, g::givens -> type_check p g; check(parms,givens)
     | _,_ -> failwith "length check failed"

   if (List.length parms)=(List.length givens) then
     check (parms,givens)
   else
     Errors.type_error rng (Errors.ModuleArgs(x,(List.length givens),(List.length parms)))