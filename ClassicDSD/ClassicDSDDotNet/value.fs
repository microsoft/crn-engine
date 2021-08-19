[<JavaScript>]
module Microsoft.Research.DNA.Value

open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine
open Errors
open Types
open Parser
type parser<'a> = Parser.t<'a>

module Prim = Microsoft.Research.CRNEngine.Expression
type range = Types.range

type name = string

(* The type of binary operators. *)
type op = 
  | Plus
  | Minus
  | Mul
  | Div
  | Power
  | Max
  | Modulo
(*  | Equal
  | Different
  | Lt
  | Gt
  | Ltequal
  | Gtequal *)

(* The type of unary functions. *)
type fn = 
  | Float2Int
  | Int2Float
  | Sqrt
  | Rate
   
(* Primitive values are either modelling engine values *)

type prim = Expression.t<string>

(* The type of DSD values/expressions. *)
type t =
  | String of string * range
  | Int of int * range
  | Domain of string * int * prim * prim * range
  | DomainS of string * int * string * prim * prim * range (* Domain with specified DNA sequence *)
  | Bool of bool * range
  | Char of char * range
  | Float of float * range
(*
  | Entalpy of float<kcal/mol> * range
  | Entropy of  float<cal/(mol*K)> * range
*)
  | Variable of name * range
  | Op of t * op * t * range
  | Neg of t * range
  | Show of t * range
  | Function of fn * t * range
  | Tuple of t list * range
  | Ceiling of t * range
  | Floor   of t * range

type t_bind =
  | Val of string * t
  | Sys of string * t
type t_env = t_bind list


(* Get the range information out of a value. *)
let rec get_range = function
  String(_,rng) | Int(_,rng) | Domain(_,_,_,_,rng) | DomainS(_,_,_,_,_,rng) | Bool(_,rng)
| Char(_,rng) | Float(_,rng) | Variable(_,rng) | Op(_,_,_,rng)
| Neg(_,rng) | Show(_,rng) | Function(_,_,rng) | Tuple(_,rng) -> rng

(* Erase the position information from a value (i.e. set them all to "empty") .*)
let rec erasePosns = function
  String(s,_) -> String(s,Types.emprange)
| Int(i,_) -> Int(i,Types.emprange)
| Domain(n,c,r,v,_) -> Domain(n,c,r,v,Types.emprange)
| DomainS(n,c,s,r,v,_) -> DomainS(n,c,s,r,v,Types.emprange)
| Bool(b,_) -> Bool(b,Types.emprange)
| Char(c,_) -> Char(c,Types.emprange)
| Float(f,_) -> Float(f,Types.emprange)
| Variable(n,_) -> Variable(n,Types.emprange)
| Op(v,op,v',_) -> Op((erasePosns v),op,(erasePosns v'),Types.emprange)
| Neg(v,_) -> Neg((erasePosns v),Types.emprange)
| Show(v,_) -> Show((erasePosns v),Types.emprange)
| Function(fn,v,_) -> Function(fn,(erasePosns v),Types.emprange)
| Tuple(vs,_) -> Tuple(List.map erasePosns vs, Types.emprange)
| Ceiling (v,_) -> Ceiling((erasePosns v),Types.emprange)
| Floor (v,_)   -> Floor((erasePosns v),Types.emprange)

(* Replace the position info of certain variables with the supplied information.
 * This allows us to track positions correctly across module instantiations. *)
let rec replacePosns (mrs:range Stringmap.t) = function
    String _ | Int _ | Bool _ | Char _ | Float _ as v -> v
    | Domain(n,c,r,v,rng) ->
        begin match (Stringmap.tryFind n mrs) with
          Some rng -> Domain(n,c,r,v,rng)
        | None -> Domain(n,c,r,v,rng)
        end
    | DomainS(n,c,s,r,v,rng) ->
        begin match (Stringmap.tryFind n mrs) with
          Some rng -> DomainS(n,c,s,r,v,rng)
        | None -> DomainS(n,c,s,r,v,rng)
        end
    | Variable(n,rng) ->
        begin match (Stringmap.tryFind n mrs) with
          Some rng -> Variable(n,rng)
        | None -> Variable(n,rng)
        end
    | Op(v,op,v',rng) -> Op((replacePosns mrs v),op,(replacePosns mrs v'),rng)
    | Neg(v,rng) -> Neg((replacePosns mrs v),rng) 
    | Floor(v,rng) -> Floor((replacePosns mrs v),rng) 
    | Ceiling(v,rng) -> Ceiling((replacePosns mrs v),rng) 
    | Show(v,rng) -> Show((replacePosns mrs v),rng) 
    | Function(fn,v,rng) -> Function(fn,(replacePosns mrs v),rng)
    | Tuple(vs,rng) -> Tuple((List.map (replacePosns mrs) vs),rng)

let rec set_range newRange (v:t) = 
  match v with
  | String (a, _) -> String (a, newRange)
  | Int (a, _)    -> Int (a, newRange)
  | Domain (s, i, p1, p2, _) -> Domain (s, i, p1, p2, newRange)
  | DomainS (s1, i, s2, p1, p2, _) -> DomainS (s1, i, s2, p1, p2, newRange)
  | Bool (a, _) -> Bool (a, newRange)
  | Char (a, _) -> Char (a, newRange)
  | Float (a, _) -> Float (a, newRange)
  | Variable (a, _) -> Variable (a, newRange)
  | Op (t1, op, t2, _) -> Op (set_range newRange t1, op, set_range newRange t2, newRange)
  | Neg (a, _) -> Neg (set_range newRange a, newRange)
  | Floor(a,_) -> Floor(set_range newRange a, newRange)
  | Ceiling(a,_) -> Ceiling(set_range newRange a, newRange)
  | Show (a, _) -> Show (set_range newRange a, newRange)
  | Function (fn, t, _) -> Function (fn, set_range newRange t, newRange)
  | Tuple (a, _) -> Tuple (List.map (set_range newRange) a, newRange)

(* Compare two values. *)
let compare (v:t) (v':t) = compare v v'

(* Display unary functions. *)
let display_function = function
  | Float2Int -> "int_of_float" 
  | Int2Float -> "float_of_int" 
  | Sqrt ->  "sqrt" 
  | Rate -> "rate"

(* Display binary operators. *)
let display_operator = function
  Plus -> "+" | Minus -> "-" | Mul -> "*" | Div -> "/ " | Power -> "**" | Max -> "max" | Modulo -> "%"
(*
    | Equal -> display_symbol " = "  
    | Different -> display_symbol " <> "  
    | Lt -> display_symbol " < "  
    | Gt -> display_symbol " > "  
    | Ltequal -> display_symbol " <= "  
    | Gtequal -> display_symbol " >= "  
*)

(* Produce string representations of values. *)
let display (html:bool) (v:t) =
  let display_symbol (s:string) = if html then "<font color=#990000>" + s + "</font>" else s in
  let display_string (s:string) = if html then "<font color=#009900>\"" + s + "\"</font>" else "\"" + s + "\"" in
  let display_char (s:string) = if html then "<font color=#009900>'" + s + "'</font>" else s in
  let operator (o:op) = display_symbol (" " + (display_operator o) + " ") in
  let rec display (v:t) =
    match v with
    | String(s,_) -> display_string s
    | Int(n,_) -> string n
    | Domain(s,i,r,v,_) | DomainS(s,i,_,r,v,_) -> s + (if i <= 0 then "" else "." + string i)      
    (*^(if r = 1.0 && v = 1.0 then "" else symbol "@" + Lib.display_float r + "," + Lib.display_float v)*)
    | Bool(b,_) -> display_symbol (string b)
(*#if JavaScript
    | Char(c,_) -> display_char (c.ToString())
#else*)
// FP: I am provisionally removing this conditional compilation statement, because I believe it to be unnecessary. I am not certain of the circumstances where this case is invoked. If you find that there is a good reason for the conditional compilation, please contact me.
    | Char(c,_) -> display_char (string (int c))
(*#endif*)
    | Float(f,_) -> Lib.display_float f
    | Variable(n,_) -> n
    | Op(v,o,v',_) -> display_symbol "(" + display v + operator o + display v' + display_symbol ")"
    | Neg(v,_) -> display_symbol "-" + display v
    | Ceiling(v,_) -> "ceiling(" + display v + ")"
    | Floor(v,_) -> "floor(" + display v + ")"
    | Show(v,_) -> display_symbol "show " + display v
    | Function(fn,v,_) -> display_symbol "(" + display_symbol(display_function fn) + " " + display v + display_symbol ")"
    | Tuple(vs,_) -> displays vs

  and displays (vs:t list) = 
    let rec f (vs:t list) =
      match vs with
      | [] -> ""
      | [v] -> display v
      | v::vs -> display v + display_symbol "," + f vs 
    in display_symbol "(" + f vs + display_symbol ")"
  in display v

(* Various ways to produce a string representation. *)
let displays (html:bool) (vs:t list) = display html (Tuple (vs, emprange))
let to_string (v:t) = display false v
let to_strings (vs:t list) = displays false vs
let to_html (v:t) = display true v 

let rec to_engineValue = function
  | Int (i, _) -> Prim.Float (float i)
  | Float (t, _) -> Prim.Float t
  (*
  | Entalpy (t, _) -> Prim.Entalpy t
  | Entropy (t, _) -> Prim.Entropy t
  *)
  | Variable (n, _) -> Prim.Key n
  | Op (v1, Plus, v2, _) -> Prim.add (to_engineValue v1) (to_engineValue v2)
  | Op (v1, Minus, v2, _) -> Prim.sub (to_engineValue v1) (to_engineValue v2)
  | Op (v1, Mul, v2, _) -> Prim.mul (to_engineValue v1) (to_engineValue v2)
  | Op (v1, Div, v2, _) -> Prim.div (to_engineValue v1) (to_engineValue v2)  
  | Op (v1, Power, v2, _) -> Prim.power (to_engineValue v1) (to_engineValue v2)  
  | Op (v1, Modulo, v2, _) -> Prim.modulo  (to_engineValue v1) (to_engineValue v2)
//  | Op (v1, Max, v2, _) -> Prim.max (to_engineValue v1) (to_engineValue v2)  TODO
//  | Function (Float2Int, v, _) -> Prim.norm_floor (to_engineValue v)         TODO
  | Floor (x,_ ) -> Prim.Floor (to_engineValue x)
  | Ceiling (x,_ ) -> Prim.Ceiling (to_engineValue x)
  | v -> failwith ("Value cannot be mapped to Term.Prim.t: " + (to_string v))

(* Return bind rate (of a domain). *)
let bind_rate (v:t) (deg:t) =
  let d = to_engineValue deg in
  match v with
  | Domain(_,_,r,_,_) -> Prim.mul r d
  | DomainS(_,_,_,r,_,_) -> Prim.mul r d
  | _ -> failwith "bind_rate - not a domain."

(* Return unbind rate (of a domain). *)
let unbind_rate (v:t) =
  match v with
  | Domain(_,_,_,v,_) -> v
  | DomainS(_,_,_,_,v,_) -> v
  | _ -> failwith "unbind_rate - not a domain."

(* ********************************************************************************************************* *)

(*Determine if two values match, with _ matching everything *)
let rec matching = function
 | (Variable("_",_),_) -> true
 | (_,Variable("_",_)) -> true
 | (Variable(x,_),Variable(y,_)) -> x=y
 | (Domain(s,i,_,_,_),Domain(s',i',_,_,_)) -> s=s' && i=i'
 | (DomainS(s,i,_,_,_,_),DomainS(s',i',_,_,_,_)) -> s=s' && i=i'
 | (String(s,_),String(s',_)) -> s=s'
 | (Int(i,_),Int(i',_)) -> i = i'
 | (Bool(c,_),Bool(c',_)) -> c = c'
 | (Char(c,_),Char(c',_)) -> c = c'
 | (Float(c,_),Float(c',_)) -> c = c'
 | (Op(x,op,y,_),Op(x',op',y',_)) -> (matching (x,x')) && (matching(y,y')) && op=op'
 | (Neg(x,_),Neg(x',_)) -> matching (x,x')
 | (Ceiling(x,_),Ceiling(x',_)) -> matching (x,x')
 | (Floor(x,_),Floor(x',_)) -> matching (x,x')
 | (Show(v,_),Show(v',_)) -> matching (v,v')
 | (Function(f,v,_),Function(f',v',_)) -> f=f' && matching(v,v')
 | (Tuple(vs,_),Tuple(vs',_)) -> List.fold (&&) true (List.map2 (fun v1 v2 -> matching (v1,v2)) vs vs')
 | _ -> false

let opError v t ts s op = type_error (get_range v) (WrongType(t,ts,(s + " argument of " + display_operator op)))

(* Deduce a type for a Value.t, given a particular environment. Modifies ref cells in environment to update inferred type*)
let inferType (env:Types.type_env) (v:t) =
  let updateType nt r cref = begin cref := Types.update_range nt r; ref nt end in
  let updateTiedTypes ts currnt nt r = 
     begin List.iter (fun t -> t:= Types.update_range nt r) ts ; updateType nt r currnt end in
  let op_accepts rng = function
   | Plus  -> ( (function
                 | IntT _ | FloatT _ | BoolT _ | StringT _
                 | TiedT _ | BottomT -> true
                 | _ -> false)
              , [IntT rng; FloatT rng; BoolT rng; StringT rng] )
   | Mul   -> ( (function
                 | IntT _ | FloatT _ | BoolT _
                 | TiedT _ | BottomT -> true
                 | _ -> false)
              , [IntT rng; FloatT rng; BoolT rng] )
   | Minus
   | Div
   | Power  // NB: previous implementation required exponent to be int, thisone accepts floats also
   | Max   -> ( (function
                   | IntT _ | FloatT _
                   | TiedT _ | BottomT -> true
                   | _ -> false)
                , [IntT rng; FloatT rng] )
    | Modulo -> ( (function
                 | IntT _ | FloatT _ | BoolT _ | StringT _
                 | TiedT _ | BottomT -> true
                 | _ -> false)
              , [IntT rng; FloatT rng; BoolT rng; StringT rng] )
  in
  let rec infer = function
    String(_,r) -> ref (StringT r) 
  | Int(_,r) -> ref (IntT r) 
  | Ceiling(_,r) -> ref (IntT r) 
  | Floor(_,r) -> ref (IntT r) 
  | Domain _ | DomainS _ -> failwith "domain exists at type check!"
  | Bool(_,r) -> ref (BoolT r) 
  | Char(_,r) -> ref (CharT r) 
  | Float(_,r) -> ref (FloatT r)
  | Tuple (vs,r) -> ref (TupT((List.map infer vs), r))
  | Variable(n,rng) ->
      begin match (Lib.try_assoc n env) with
        Some t -> t
      | None -> type_error rng (UnboundVariable n)
      end
  | Op(v1,o,v2,rng) ->
      let acceptable, acceptables = op_accepts rng o in
      let t1 = infer v1 in
      let t2 = infer v2 in
      if acceptable !t1 then
        if acceptable !t2 then
          if unify rng t1 t2 then t1
          else opError v2 !t2 (unify_possibilities rng acceptables !t1) "second" o
        else opError v2 !t2 acceptables "second" o
      else opError v1 !t1 acceptables "first" o
  | Neg(v,rng) ->
      begin 
      let t = infer v in
      match !t with
        FloatT _ | IntT _ | BoolT _ | TiedT(_,Some(_),_) -> t
      | BottomT -> begin t:= TiedT([],Some(false),[rng]); t end
      | TiedT(tys,None,_) -> updateTiedTypes tys t (TiedT(tys,Some(false),[rng])) rng
      | t -> let descr = "argument of negation operator" in
             type_error (get_range v) (WrongType(t,[IntT rng;FloatT rng;BoolT rng],descr))
      end
  | Show(v,rng) -> infer v |> ignore
                   ref (StringT rng)
  | Function(Float2Int,v,rng) ->
      begin 
        let t = infer v in
        match !t with
        FloatT _ -> ref (IntT rng)
      | BottomT -> begin t:= FloatT(rng); ref (IntT rng) end
      | TiedT(tys,_,_) -> begin ignore (updateTiedTypes tys t (FloatT(rng)) rng); ref (IntT rng) end
      | t -> let descr = "argument of " + (display_function Float2Int) in
             type_error (get_range v) (WrongType(t,[FloatT rng],descr))
      end
  | Function(Int2Float,v,rng) ->
      begin 
      let t = infer v in
      match !t with
        IntT _ -> ref (FloatT rng)
      | BottomT -> begin t:= IntT(rng); ref (FloatT rng) end
      | TiedT(tys,_,_) -> begin ignore (updateTiedTypes tys t (IntT(rng)) rng); ref (FloatT rng) end
      | t -> let descr = "argument of " + (display_function Int2Float) in
             type_error (get_range v) (WrongType(t,[IntT rng],descr))
      end
  | Function(Sqrt,v,rng) ->
      begin 
      let t = infer v in
      match !t with
      | FloatT _ -> t
      | BottomT -> begin t:= FloatT(rng); t end
      | TiedT(tys,_,_) -> updateTiedTypes tys t (FloatT(rng)) rng
      | t -> let descr = "argument of " + (display_function Sqrt) in
             type_error (get_range v) (WrongType(t,[FloatT rng],descr))
      end
  | Function(Rate,_,_) -> failwith "rate function not implemented"
  in
  infer v

(* ********************************************************************************************************* *)


(* Compute the free names in a value (i.e. all names). *)
let rec free_names (v:t) =
  match v with 
  | Variable(n,rng) -> [(n,rng)] 
  | Op(v,_,v',_) -> (free_names v) @ (free_names v')
  | Neg(v,_) -> free_names v
  | Ceiling(v,_) -> free_names v
  | Floor(v,_) -> free_names v
  | Show(v,_) -> free_names v
  | Function(_,v,_) -> free_names v
  | Tuple(vs,_) -> List.collect free_names vs
  | _ -> []

(* A predicate to determine if a variable is in the environment *)
let rec varInEnv (e:t_env) (n:string) =
  match e with
  | [] -> false
  | Val(n',_)::e' -> n' = n || varInEnv e' n
  | Sys(n',_)::e' -> n' = n || varInEnv e' n

(* lookup the variable in the environment. Raises an internal error if not present *)
let rec env_lookup (n:string) (e:t_env) =
  match e with
(*  | [] -> failwith ("free variable "^n^" persisted past type checking")*)
  | [] -> Variable (n, None)
  | Val(n',v)::e' -> if (n=n') then v else env_lookup n e'
  | Sys(n',v)::e' -> if (n=n') then v else env_lookup n e'

(* Evaluate a value down to normal form. NB always terminates! *)
(* Previous failure is now termination due to variables *)
let rec eval (env:t_env) (v:t) =
  match v with
  | Variable(n,_) -> env_lookup n env
  | Op(v,Plus,v',rng) ->
      match (eval env v,eval env v') with
      | Int(i,_),Int(i',_) -> Int(i + i', rng)
      | Float(f,_),Float(f',_) -> Float(f + f', rng)
      | Bool(b,_),Bool(b',_) -> Bool(b || b', rng)
      | String(s,_),String(s',_) -> String(s + s', rng)
      | v,v' -> Op(v, Plus, v', rng)
  | Op(v,Minus,v',rng) ->
      match (eval env v,eval env v') with
      | Int(i,_),Int(i',_) -> Int(i - i', rng)
      | Float(f,_),Float(f',_) -> Float(f - f', rng)
      | v,v' -> Op(v, Minus, v', rng)
  | Op(v,Mul,v',rng) ->
      match (eval env v,eval env v') with
      | Int(i,_),Int(i',_) -> Int(i * i', rng)
      | Float(f,_),Float(f',_) -> Float(f * f', rng)
      | Bool(b,_),Bool(b',_) -> Bool(b && b', rng)
      | v,v' -> Op(v, Mul, v', rng)
  | Op(v,Div,v',rng) ->
      match (eval env v,eval env v') with
      | Int(i,_),Int(i',_) -> Int(i / i', rng)
      | Float(f,_),Float(f',_) -> Float(f / f', rng)
      | v,v' -> Op(v, Div, v', rng)
  | Op(v,Power,v',rng) ->
      match (eval env v, eval env v') with
      |  Int(i,_),Int(i',_) -> Int(pown i i',rng)
      | Float(f,_),Int(i,_) -> Float(pown f i, rng)
      | v,v' -> Op(v, Power, v', rng)
  | Op(v,Max,v',rng) ->
      match (eval env v, eval env v') with
      | Int(i,_),Int(i',_) -> Int(max i i',rng)
      //| Float(f,_),Int(i,_) -> Float(max f ( i), rng)
      | Float(f,_),Float(i,_) -> Float(max f i, rng)
      | v,v' -> Op(v, Max, v', rng)
  | Op(v,Modulo,v',rng) ->
      match (eval env v, eval env v') with
      | Int(i,_),Int(i',_) -> 
        let m = Expression.moduloFloat (float i) (float i')
        Int(int32 m,rng)
      //| Float(f,_),Int(i,_) -> Float(max f ( i), rng)
      | Float(f,_),Float(i,_) -> 
        let m = Expression.moduloFloat f i
        Float(m, rng)
      | v,v' -> Op(v, Modulo, v', rng)
  | Neg(v,rng) ->
      match eval env v with
      | Bool(b,_) -> Bool(not b, rng)
      | Int(i,_) -> Int(-i, rng)
      | Float(i,_) -> Float(-i ,rng)
      | v -> Neg(v, rng)
  | Show(v,rng) -> String(to_string (eval env v), rng)
  | Function(fn,v,rng) ->
      match fn, (eval env v) with
      | Int2Float,Int(i,_) -> Float(float i, rng)
      | Float2Int,Float(f,_) -> Int(int f, rng)
      | Sqrt,Float(f,_) -> Float(sqrt f, rng)
      | _,v -> Function(fn,v,rng)
      (*| _,_ -> failwith ("incorrect evaluation of function " + display_function fn) *)
  | Tuple(l,rng) -> Tuple((List.map (eval env) l), rng)
  | Ceiling (v, rng) -> 
    match eval env v with
    | Int(i,_) -> Int (i, rng)
    | Float (i, _) -> Float (ceil i, rng)
    | v -> Ceiling (v, rng) 
  | Floor (v, rng) -> 
    match eval env v with
    | Int(i,_) -> Int (i, rng)
    | Float (i, _) -> Float (floor i, rng)
    | v -> Floor (v, rng)
  | v -> v

let universal_counters = function
  | Domain(n,_,r,v,p) -> Domain(n,-1,r,v,p)
  | DomainS(n,_,s,r,v,p) -> DomainS(n,-1,s,r,v,p)
  | x -> x


  (**** Value parser for Visual DSD(beta)'s old syntax, in parser.mly ****)
let parseWithPos p v flattener (initText, initPos) = 
  (p >>= fun x -> 
    (fun (finalText, finalPos) -> preturn (v (flattener (x, Types.mkRange(initPos, finalPos)))) (finalText, finalPos))
  ) (initText, initPos)

let punder      = Parser.kw "_"

let parseString = parseWithPos Parser.name                 t.String   id
let parseInt    = parseWithPos (Parser.pint32  .>> spaces) t.Int      id
let parseBool   = parseWithPos (Parser.pbool   .>> spaces) t.Bool     id 
let parseChar   = parseWithPos (Parser.anyChar .>> spaces) t.Char     id
let parseFloat  = parseWithPos (Parser.pfloat  .>> spaces) t.Float    id
let parseName   = parseWithPos (Parser.name    .>> spaces) t.Variable id
let parseUScore = parseWithPos (punder         .>> spaces) t.Variable id

let parseFun s o pval = parseWithPos 
                          (kw s >>. pval () >>= fun v -> preturn (o, v)) t.Function
                          (fun ((a,b),y) -> (a,b,y))
                        
let parseIntToFloat = parseFun "float_of_int" Int2Float
let parseFloatToInt = parseFun "int_of_float" Float2Int


let parsePair pval = parseWithPos (Parser.paren 
                                      (pval () >>= fun v1 -> 
                                        (kw "," >>. 
                                          (pval ()>>= fun v2 -> 
                                            preturn [v1; v2]))))
                                    t.Tuple 
                                    id

let parseTerm pval =
    fun st -> Parser.choice
                  [
                    parseString
                    parseFloat
                    parseInt 
                    parseBool
                    parseChar
                    parseUScore 
                    parseName
                    parsePair       pval
                    parseIntToFloat pval
                    parseFloatToInt pval 
                    Parser.paren    (pval ()) 
                  ] st

let parseOp s o pval = parseWithPos (parseTerm pval >>= fun v1 -> 
                                  kw s >>.
                                  pval () >>= fun v2 -> preturn (v1, o, v2)) t.Op
                                 (fun ((a,b,c),y) -> (a,b,c,y))
                                 
let parseAdd = parseOp "+"  Plus
let parseSub = parseOp "-"  Minus
let parseMul = parseOp "*"  Mul
let parseDiv = parseOp "/"  Div
let parsePow = parseOp "**" Power
                     
let parseParens pval = Parser.paren (pval ())

let rec do_parse _ : Parser.t<t> =
  Parser.choice
    [ 
      parseTerm do_parse
      parseAdd  do_parse
      parseSub  do_parse
      parseMul  do_parse
      parseDiv  do_parse
      parsePow  do_parse
    ]

let parse : Parser.t<t> = fun s -> do_parse () s