[<JavaScript>]
module Microsoft.Research.CRNEngine.Expression 
open WebSharper
//'k means 'key
type choice<'l,'r> = Lib.choice<'l,'r>
type lambda<'k> when 'k:equality = Lambda.t<'k> 

[<NamedUnionCases>]
type t<'k> when 'k:equality = 
  | Key of Key:'k
  | Float of Float:float
  | Times of Times:t<'k> list
  | Divide of Divide:divide<'k>
  | Power of Power:power<'k>
  | Plus of Plus:t<'k> list
  | Minus of Minus:minus<'k>
  | Absolute of Absolute:t<'k>
  | Log of Log:t<'k>
  | Modulo of Modulo : modulo<'k>
  | If of bexp<'k> * t<'k> * t<'k>
  //| Max   of Max:t<'k> list
    with 
      static member (*) : t<'k>*t<'k> -> t<'k> 
      static member (*) : float*t<'k> -> t<'k> 
      static member (*) : t<'k>*float -> t<'k> 
      static member (+) : t<'k>*t<'k> -> t<'k> 
      static member (+) : float*t<'k> -> t<'k> 
      static member (+) : t<'k>*float -> t<'k> 
      static member (-) : t<'k>*t<'k> -> t<'k>
      static member (-) : float*t<'k> -> t<'k> 
      static member (-) : t<'k>*float -> t<'k>  
      static member (/) : t<'k>*t<'k> -> t<'k> 
      static member (/) : float*t<'k> -> t<'k> 
      static member (/) : t<'k>*float -> t<'k>  
      static member  Pow  : t<'k>*t<'k> -> t<'k> 
      static member  Pow  : float*t<'k> -> t<'k> 
      static member  Pow  : t<'k>*float -> t<'k>  
      static member (%) : t<'k>*t<'k> -> t<'k> 
      static member (%) : float*t<'k> -> t<'k> 
      static member (%) : t<'k>*float -> t<'k> 
    end
and
  modulo<'k> when 'k:equality = {
    div     : t<'k>
    modulo  : t<'k>  
  }
and
  divide<'k> when 'k:equality = {
    div1:t<'k>
    div2:t<'k>
  }
and
  power<'k> when 'k:equality = {
    base_:t<'k>
    exponent:t<'k>
  }
and
  minus<'k> when 'k:equality = {
    sub1:t<'k>
    sub2:t<'k>
  }
and bexp<'k> when 'k:equality = BTrue
                              | BFalse
                              | BLT  of t<'k> * t<'k>
                              | BLeq of t<'k> * t<'k>
                              | BEq  of t<'k> * t<'k>
                              | BGeq of t<'k> * t<'k>
                              | BGT  of t<'k> * t<'k>
                              | BNot of bexp<'k>
                              | BAnd of bexp<'k> * bexp<'k>
                              | BOr  of bexp<'k> * bexp<'k>

val add : t<'k> -> t<'k> -> t<'k>
val sub : t<'k> -> t<'k> -> t<'k>
val mul : t<'k> -> t<'k> -> t<'k>
val div : t<'k> -> t<'k> -> t<'k>
val power : t<'k> -> t<'k> -> t<'k>
val modulo : t<'k> -> t<'k> -> t<'k>
//val max : t<'k> -> t<'k> -> t<'k>
val moduloFloat : float -> float -> float

val substitute    : env:Map<'k,t<'k>> -> e:t<'k> -> t<'k>
val map           : ('a -> 'b) -> t<'a> -> t<'b>
val collect       : ('a -> 'b list) -> t<'a> -> t<'b> list
val visit         : (t<'a> -> t<'a> option) -> t<'a> -> t<'a>
val expand        : ('a -> t<'b>) -> t<'a> -> t<'b>
val to_lambda : t<'k> -> lambda<'k >
val to_string     : ('k -> string) -> t<'k> -> string
val to_string_plot: ('k -> string) -> t<'k> -> string
val to_mathML     : ('k -> Sbml.sId) -> t<'k> -> Sbml.mathExpr
val to_matlab     : ('k -> string) -> t<'k> -> string
val convert       : ('k -> choice<float,'s>) -> t<'k> -> t<'s>
val mentions      : t<'a> -> 'a list
val eval          : ('k -> float) -> t<'k> -> float
val simplify      : t<'k> -> t<'k>
val is_not_empty  : x:t<'k> -> bool
val parse         : Parser.t<'k> -> Parser.t<t<'k>>
val parse_boolean : Parser.t<'k> -> Parser.t<bexp<'k>>
val from_string   : Parser.t<'k> -> string -> t<'k>
val inline_keys   : Environment.t                                  (* map from parameter names to floats *)
                     -> Map<string, t<Key2<'s>>>          (* map from rate names to inlined expressions *)
                     -> Key<'s>                                  (* original Key *)
                     -> t<Key2<'s>>                       (* inlined key *)
val inline_rates_env : Environment.t -> Map<string,t<Key<'s>>> -> Map<string,t<Key2<'s>>>