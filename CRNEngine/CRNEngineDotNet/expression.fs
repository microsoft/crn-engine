[<JavaScript>]
module Microsoft.Research.CRNEngine.Expression
//open System.Diagnostics
//'k means 'key
type choice<'l,'r> = Lib.choice<'l,'r>
//type lambda<'k> = Lambda<'k>  //when 'k:equality = Lambda<'k> 

[<WebSharper.NamedUnionCases>]
//[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type t<'k> when 'k:equality = 
  | Key      of Key:'k
  | Float    of Float:float
  | Times    of Times:t<'k> list
  | Divide   of Divide:divide<'k>
  | Power    of Power:power<'k>
  | Plus     of Plus    : t<'k> list
  | Minus    of Minus:minus<'k>
  | Absolute of Absolute:t<'k>
  | Log      of Log:t<'k>
  | Modulo   of Modulo : modulo<'k>
  | Ceiling  of Ceiling : t<'k>
  | Floor    of Floor   : t<'k>
  | Round    of Round   : t<'k>
  | If       of bexp<'k> * t<'k> * t<'k>
  //| Max   of Max:t<'k> list
    with 
      static member (*) (a : t<'k>, b: t<'k>) = Times [a;b]
      static member (*) (a : float, b: t<'k>) = Times [Float a;b]
      static member (*) (a : t<'k>, b: float) = Times [a;Float b]
      static member (+) (a : t<'k>, b: t<'k>) = Plus [a;b] 
      static member (+) (a : float, b: t<'k>) = Plus [Float a;b]
      static member (+) (a : t<'k>, b: float) = Plus [a;Float b]
      static member (-) (a : t<'k>, b: t<'k>) = Minus {sub1=a;sub2=b}
      static member (-) (a : float, b: t<'k>) = Minus {sub1=Float a;sub2=b}
      static member (-) (a : t<'k>, b: float) = Minus {sub1=a;sub2=Float b}
      static member (/) (a : t<'k>, b: t<'k>) = Divide {div1=a;div2=b}
      static member (/) (a : float, b: t<'k>) = Divide {div1=Float a;div2=b}
      static member (/) (a : t<'k>, b: float) = Divide {div1=a;div2=Float b}
      static member  Pow  (a : t<'k>, b: t<'k>) = Power {base_=a;exponent=b}
      static member  Pow  (a : float, b: t<'k>) = Power {base_=Float a;exponent=b}
      static member  Pow  (a : t<'k>, b: float) = Power {base_=a;exponent=Float b}
      static member ( % ) (a : t<'k>, b: t<'k>) = Modulo {div = a       ; modulo =b}
      static member ( % ) (a : float, b: t<'k>) = Modulo {div = Float a ; modulo=b}
      static member ( % ) (a : t<'k>, b: float) = Modulo {div = a       ; modulo=Float b}
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

let add a b =
  match (a, b) with
  | (Float 0.0, v) | (v, Float 0.0) -> v
  | (Float f1, Float f2) -> Float (f1 + f2)
  | (Float f1, Plus (Float f2 :: vs))
  | (Plus (Float f1 :: vs), Float f2) -> Plus (Float (f1 + f2) :: vs)
  | (Plus vs, v)
  | (v, Plus vs) -> Plus (v :: vs)
  | (Times [Float f; v1], v2)
  | (Times [v1; Float f], v2)
  | (v1, Times [Float f; v2])
  | (v1, Times [v2; Float f]) when v1 = v2  -> Times [Float (f + 1.0); v1]
  | (Times [Float f1; v1], Times [Float f2; v2])
#if JavaScript

#else
  | (Times [v1; Float f1], Times [Float f2; v2]) (* This line (or the line below) cause stackoverflow with WebSharper 3.6.12.235, temporarily skip them there as it crashes Visual Studio! *)
#endif
  | (Times [Float f1; v1], Times [v2; Float f2])
  | (Times [v1; Float f1], Times [v2; Float f2]) when v1 = v2  -> Times [Float (f1 + f2); v1]
  | (v1, v2) when v1 = v2 -> Times [Float 2.0; v1]
  | _ -> Plus [a; b]

let zero : t<'a> = Float 0.0
let one  : t<'a> = Float 1.0

let sub a b =
  match (a, b) with
  | (v, Float 0.0) -> v
  | (Float f1, Float f2) -> Float (f1 - f2)
  | _ -> Minus {sub1 = a; sub2 = b}

let mul a b =
  match (a, b) with
  | (Float 0.0, v) | (v, Float 0.0) -> Float 0.0
  | (Float 1.0, v) | (v, Float 1.0) -> v
  | (Float f1, Float f2) -> Float (f1 * f2)
  | (Float f1, Times (Float f2 :: v))
  | (Times (Float f1 :: v), Float f2) -> Times (Float (f1 * f2) :: v)
  | (Times vs, v)
  | (v, Times vs) -> Times (v :: vs)
  | _ -> Times [a; b]

let div a b =
  match (a, b) with
  | (v, Float 0.0) -> failwith "Division by 0"
  | (Float 0.0, v) -> Float 0.0
  | (v, Float 1.0) -> v
  | (Float f1, Float f2) -> Float (f1 / f2)
  | _ -> Divide {div1 = a; div2 =  b}

let power a b =
  match (a, b) with
  | (v, Float 0.0) -> Float 1.0
  | (Float 1.0, _) -> Float 1.0
  | (v, Float 1.0) -> v
  | (Float f1, Float f2) -> Float (f1 ** f2)
  | _ -> Power {base_ = a; exponent = b}


(* F#'s % operator does not deal with negative number as expected by a modulo operator, 
   because it is the remainder operator, not the modulo one. For example, -14 % 5 
   evaluates to -4, but (-14 mod 5) is 1. We define our own modulo operator that below.
*)
let moduloFloat x m = (x % m + m) % m

let modulo a b = 
  match (a, b) with
  | (_, Float 1.0) -> Float 0.0
  | (Float 0.0, _) -> Float 0.0
  | (Float f1, Float f2) -> Float (moduloFloat f1 f2)
  | _ -> Modulo {div = a; modulo = b}
  

(*
let max a b =
  match (a, b) with
  | (Float f1, Float f2) -> Float ( max f1 f2 )
  | _ -> Max (a,b)
*)


let rec substitute (env:Map<'k,t<'k>>) (e:t<'k>) =
  let f (x:'k) = 
    match Map.tryFind x env with
    | None -> Key x
    | Some(x') -> x'
  let g x = substitute env x
  in match e with
      | Key x                         -> f x
      | Float x                       -> Float x
      | Times l                       -> Times    (List.map g l)
      | Divide {div1=x; div2=y}       -> Divide   {div1=g x; div2=g y}
      | Power  {base_=x;exponent=y}   -> Power    {base_=g x;exponent=g y}
      | Plus l                        -> Plus     (List.map g l)
      | Minus  {sub1=x; sub2=y}       -> Minus    {sub1=g x; sub2=g y}
      | Absolute x                    -> Absolute (g x)
      | Log x                         -> Log      (g x)
      | Modulo {div = x; modulo = y}  -> Modulo   {div = g x; modulo = g y}
      | Ceiling x                     -> Ceiling  (g x)
      | Floor   x                     -> Floor    (g x)
      | Round   x                     -> Round    (g x)
      | If (b, e1, e2)                -> If       (substituteBool env b, g e1, g e2)
and substituteBool (env:Map<'k,t<'k>>) (b:bexp<'k>) =
  let f x = substituteBool env x
  let g x = substitute env x
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> BLT  (g e1, g e2) 
  | BLeq (e1, e2) -> BLeq (g e1, g e2)
  | BEq  (e1, e2) -> BEq  (g e1, g e2) 
  | BGeq (e1, e2) -> BGeq (g e1, g e2)
  | BGT  (e1, e2) -> BGT  (g e1, g e2)
  | BNot b1       -> BNot (f b1)
  | BAnd (b1, b2) -> BAnd (f b1, f b2)
  | BOr  (b1, b2) -> BOr  (f b1, f b2)

let rec map f = function
  | Key x                         -> Key (f x)
  | Float f                       -> Float f
  | Times l                       -> Times    (List.map (map f) l)
  | Divide {div1=x; div2=y}       -> Divide   {div1=map f x; div2=map f y}
  | Power {base_=x;exponent=y}    -> Power    {base_=map f x;exponent=map f y}
  | Plus l                        -> Plus     (List.map (map f) l)
  | Minus {sub1=x; sub2=y}        -> Minus    {sub1=map f x; sub2=map f y}
  | Absolute x                    -> Absolute (map f x)
  | Log x                         -> Log      (map f x)
  | Modulo {div = x; modulo = y}  -> Modulo   {div = map f x; modulo = map f y}
  | Ceiling x                     -> Ceiling  (map f x)
  | Floor   x                     -> Floor    (map f x)
  | Round   x                     -> Round    (map f x)
  | If (b, e1, e2)                -> If       (mapBool f b, map f e1, map f e2)
and mapBool f (b:bexp<'k>) =
  let mb x = mapBool f x
  let me x = map f x
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> BLT  (me e1, me e2) 
  | BLeq (e1, e2) -> BLeq (me e1, me e2)
  | BEq  (e1, e2) -> BEq  (me e1, me e2) 
  | BGeq (e1, e2) -> BGeq (me e1, me e2)
  | BGT  (e1, e2) -> BGT  (me e1, me e2)
  | BNot b1       -> BNot (mb b1)
  | BAnd (b1, b2) -> BAnd (mb b1, mb b2)
  | BOr  (b1, b2) -> BOr  (mb b1, mb b2)

let cartesianProd l1 l2 = 
  Seq.toList <|
    seq { for el1 in l1 do
            for el2 in l2 do
              yield el1, el2 };; 

// lift a function f from species to list of species, 
// to a function (collect f) from expressions to list of expressions
let rec collect (f : ('a -> 'b list)) = 
  // collect f on each operand in a list, then take their cartesian product
  let foldCartesian (i:t<'b> list) (acc:t<'b> list list) =
    match acc with
    | [] -> [i]
    | _  -> cartesianProd i acc
            |> List.map (fun (a, b) -> a :: b)
  let listCartesian operands = 
      let operands' = 
        operands 
        |> List.map (collect f)
        |> List.filter (List.isEmpty >> not)
      List.foldBack foldCartesian operands' []
  function
  | Key x -> f x |> List.map  Key
  | Float f -> [Float f]
  | Times multiplicands -> 
      listCartesian multiplicands
      |> List.map Times
  | Plus addends -> 
      listCartesian addends
      |> List.map Plus    
  | Divide {div1 = dividendSp; div2 = divisorSp } -> 
      let xs = (collect f) dividendSp
      let ys = (collect f) divisorSp 
      let crossProd = cartesianProd xs ys
      List.map (fun (x,y) -> Divide {div1 = x; div2 = y}) crossProd
    
  | Power  { base_ = b; exponent = e} ->
      let xs        = (collect f) b
      let ys        = (collect f) e
      let crossProd = cartesianProd xs ys
      List.map (fun (x,y) -> Power { base_ = x; exponent = y}) crossProd
    
  | Minus  {sub1 = minuend; sub2 = subtrahend} -> 
      let xs        = (collect f) minuend
      let ys        = (collect f) subtrahend
      let crossProd = cartesianProd xs ys
      List.map (fun (x,y) -> Minus { sub1 = x; sub2 = y }) crossProd
    
  | Absolute plot -> 
      let xs = (collect f) plot
      List.map Absolute xs
  | Log plot -> 
      let xs = (collect f) plot
      List.map Log xs
  | Modulo {div = x; modulo = y}  -> 
      let xs = (collect f) x
      let ys = (collect f) y
      let crossProd = cartesianProd xs ys
      List.map (fun (x,y) -> Modulo   {div = x; modulo = y}) crossProd
  | Ceiling plot -> List.map Ceiling ((collect f) plot)
  | Floor   plot -> List.map Floor   ((collect f) plot)
  | Round   plot -> List.map Round   ((collect f) plot)
  | If (b, e1, e2)                -> 
      let xs = collectBool f b
      let ys = collect f e1
      let zs = collect f e2
      let crossProd = cartesianProd xs (cartesianProd ys zs)
      
      List.map (fun (b', (e1', e2')) -> If (b',e1', e2')) crossProd
and collectBool f b =
  let mkProd e1 e2 x  : bexp<'b> list = cartesianProd (collect f e1) (collect f e2) |> List.map x
  let mkBProd b1 b2 x : bexp<'b> list = cartesianProd (collectBool f b1) (collectBool f b2) |> List.map x
  match b with 
  | BTrue         -> [BTrue]
  | BFalse        -> [BFalse]
  | BLT  (e1, e2) -> mkProd e1 e2 BLT
  | BLeq (e1, e2) -> mkProd e1 e2 BLeq
  | BEq  (e1, e2) -> mkProd e1 e2 BEq
  | BGeq (e1, e2) -> mkProd e1 e2 BGeq
  | BGT  (e1, e2) -> mkProd e1 e2 BGT
  | BNot b1       -> collectBool f b1 |> List.map BNot
  | BAnd (b1, b2) -> mkBProd b1 b2 BAnd
  | BOr  (b1, b2) -> mkBProd b1 b2 BOr

let rec visit f e =
  let g x = visit f x
  match f e with
  | Some ve -> ve
  | None ->
    match e with
    | Key x                         -> Key x
    | Float f                       -> Float f
    | Times l                       -> Times (List.map g l)
    | Divide {div1=x; div2=y}       -> Divide {div1=g x; div2=g y}
    | Power {base_=x;exponent=y}    -> Power {base_=g x; exponent=g y}
    | Plus l                        -> Plus (List.map g l)
    | Minus {sub1=x; sub2=y}        -> Minus {sub1=g x; sub2=g y}
    | Absolute x                    -> Absolute (g x)
    | Log x                         -> Log (g x)
    | Modulo {div = x; modulo = y}  -> Modulo   {div = g x; modulo = g y}
    | Ceiling x                     -> Ceiling (g x)
    | Floor   x                     -> Floor   (g x)
    | Round   x                     -> Round   (g x)
    | If (b, e1, e2)                -> If       (visitBool f b, g e1, g e2)
and visitBool f b =
  let vb x = visitBool f x
  let ve x = visit f x
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> BLT  (ve e1, ve e2) 
  | BLeq (e1, e2) -> BLeq (ve e1, ve e2)
  | BEq  (e1, e2) -> BEq  (ve e1, ve e2) 
  | BGeq (e1, e2) -> BGeq (ve e1, ve e2)
  | BGT  (e1, e2) -> BGT  (ve e1, ve e2)
  | BNot b1       -> BNot (vb b1)
  | BAnd (b1, b2) -> BAnd (vb b1, vb b2)
  | BOr  (b1, b2) -> BOr  (vb b1, vb b2)

let rec expand f e = 
  let g x = expand f x
  match e with
  | Key x                         -> f x
  | Float f                       -> Float f
  | Times l                       -> Times (List.map g l)
  | Divide {div1=x; div2=y}       -> Divide {div1=g x; div2=g y}
  | Power {base_=x;exponent=y}    -> Power {base_=g x; exponent=g y}
  | Plus l                        -> Plus (List.map (g) l)
  | Minus {sub1=x; sub2=y}        -> Minus {sub1=g x; sub2=g y}
  | Absolute x                    -> Absolute (g x)
  | Log x                         -> Log (g x)
  | Modulo {div = x; modulo = y}  -> Modulo   {div = g x; modulo = g y}
  | Ceiling x                     -> Ceiling (g x)
  | Floor   x                     -> Floor   (g x)
  | Round   x                     -> Round   (g x)
  | If (b, e1, e2)                -> If       (expandBool f b, g e1, g e2)
and expandBool f b =
  let eb x = expandBool f x
  let ee x = expand f x
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> BLT  (ee e1, ee e2) 
  | BLeq (e1, e2) -> BLeq (ee e1, ee e2)
  | BEq  (e1, e2) -> BEq  (ee e1, ee e2) 
  | BGeq (e1, e2) -> BGeq (ee e1, ee e2)
  | BGT  (e1, e2) -> BGT  (ee e1, ee e2)
  | BNot b1       -> BNot (eb b1)
  | BAnd (b1, b2) -> BAnd (eb b1, eb b2)
  | BOr  (b1, b2) -> BOr  (eb b1, eb b2)

let rec to_lambda_inner (x:t<'k>) =
  match x with 
  | Key(k)                        -> fun (env:'k -> float) -> env k
  | Float(c)                      -> fun (_:'k -> float) -> c
  | Times [l; r]                  ->
    let a_ = to_lambda_inner l
    let b_ = to_lambda_inner r
    fun (env:'k -> float) -> (a_ env) * (b_ env)
  | Times l                       ->
    let g_ = l |> List.map to_lambda_inner 
    fun (env:'k -> float) ->
      let mutable result = 1.0
      for op in g_ do
        let x = op env
        result <- result * x
      result
  | Divide {div1=l; div2=r}       ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> (l_ env) / (r_ env)
  | Power {base_=l;exponent=r}    ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> (l_ env) ** (r_ env) 
  | Plus [a; b]                   ->
    let a_ = to_lambda_inner a
    let b_ = to_lambda_inner b
    fun (env:'k -> float) -> (a_ env) + (b_ env)
  | Plus l                        ->
    let g_ = l |> List.map to_lambda_inner
    fun (env:'k -> float) ->
      let mutable result = 0.0
      for op in g_ do
        let x = op env
        result <- result + x
      result
  | Minus {sub1=l;sub2=r}         ->
    let a_ = to_lambda_inner l
    let b_ = to_lambda_inner r
    fun (env:'k -> float) -> (a_ env) - (b_ env)
  | Absolute x                    ->
    let x_ = to_lambda_inner x
    fun (env:'k -> float) -> abs (x_ env)
  | Log x                         ->
    let x_ = to_lambda_inner x
    fun (env:'k -> float) -> log (x_ env)
  | Modulo {div = l; modulo = r}  ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> moduloFloat (l_ env) (r_ env)
  | Ceiling x                     -> 
    let x_ = to_lambda_inner x
    fun (env:'k -> float) -> ceil (x_ env)
  | Floor   x                     -> 
    let x_ = to_lambda_inner x
    fun (env:'k -> float) -> floor (x_ env)
  | Round   x                     -> 
    let x_ = to_lambda_inner x
    fun (env:'k -> float) -> round (x_ env)
  | If (b, e1, e2)                ->
    //We speculatively as for current use it's easily worthwhile
    let bool_ = to_lambda_bool b
    let l_ = to_lambda_inner e1
    let r_ = to_lambda_inner e2
    fun (env:'k -> float) -> if (bool_ env) then l_ env else r_ env
and to_lambda_bool b =
  match b with 
  | BTrue         -> fun (_:'k -> float) -> true
  | BFalse        -> fun (_:'k -> float) -> false
  | BLT  (l, r) ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> l_ env < r_ env
  | BLeq (l, r) ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> l_ env <= r_ env
  | BEq (l, r) ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> l_ env = r_ env
  | BGeq (l, r) ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> l_ env >= r_ env
  | BGT  (l, r) ->
    let l_ = to_lambda_inner l
    let r_ = to_lambda_inner r
    fun (env:'k -> float) -> l_ env > r_ env
  | BNot b1 ->
    let l_ = to_lambda_bool b1
    fun (env:'k -> float) -> not (l_ env)
  | BAnd (b1, b2) ->
    let l_ = to_lambda_bool b1
    let r_ = to_lambda_bool b2
    fun (env:'k -> float) -> l_ env && r_ env
  | BOr  (b1, b2) ->
    let l_ = to_lambda_bool b1
    let r_ = to_lambda_bool b2
    fun (env:'k -> float) -> l_ env || r_ env

let rec to_lambda (x:t<'k>) = Lambda.Lambda (to_lambda_inner x)

let bracket = function "" -> "" | s -> "(" + s + ")"

let rec to_string (f:'k -> string) (x:t<'k>) =
  let print x = to_string f x
  match x with
  | Key(k)                        -> f k
  | Float(n)                      -> n.ToString()
  | Divide {div1=x; div2=y}       -> "(" + print x + " / " + print y + ")"
  | Power {base_=x;exponent=n}    -> "(" + print x + " ^ " + print n + ")"
  | Times l                       -> if l.Length  = 1
                                      then "prod(" + print l.Head + ")"
                                      else l |> List.map print |> List.reduce (fun x y -> x + " * " + y) |> bracket
  | Plus l                        -> if l.Length = 1 
                                      then "sum(" + print l.Head + ")"
                                      else l |> List.map print |> List.reduce (fun x y -> x + " + " + y) |> bracket
  | Minus {sub1=x;sub2=y}         -> "(" + print x + " - " + print y + ")"
  | Absolute x                    -> "|" + print x + "|"
  | Log x                         -> "log(" + print x + ")"
  | Modulo {div = x; modulo = y}  -> print x + " % " + print y
  | Ceiling x                     -> "ceiling(" + print x + ")"
  | Floor x                       -> "floor(" + print x + ")"
  | Round x                       -> "round(" + print x + ")"
  | If (b, e1, e2)                -> "if " + to_string_bool f b + " then " + print e1 + " else " + print e2
and to_string_bool f b =
  let print x     = to_string f x
  let printBool x = to_string_bool f x
  match b with 
  | BTrue         -> "true"
  | BFalse        -> "false"
  | BLT  (e1, e2) -> print e1 + " < "  + print e2
  | BLeq (e1, e2) -> print e1 + " <= " + print e2
  | BEq  (e1, e2) -> print e1 + " = "  + print e2
  | BGeq (e1, e2) -> print e1 + " >= " + print e2
  | BGT  (e1, e2) -> print e1 + " >= " + print e2
  | BNot b1       -> "not " + printBool b1
  | BAnd (b1, b2) -> printBool b1 + " && " + printBool b2
  | BOr  (b1, b2) -> printBool b1 + " || " + printBool b2

let to_string_plot f x = 
  match x with
  | Key s -> f s
  | _     -> to_string (fun z -> "[" + f z + "]") x

let rec to_mathML (f:'k -> Sbml.sId) (x:t<'k>) =
  match x with
  | Key(k)                        -> Sbml.Identifier( f k )
  | Float(n)                      -> Sbml.Const(Sbml.Float(n), None)
  | Times l                       -> Sbml.Nary(Sbml.Times, List.map (fun x -> to_mathML f x) l)
  | Divide {div1=x; div2=y}       -> Sbml.Binary(Sbml.Divide, to_mathML f x, to_mathML f y ) 
  | Power {base_=x;exponent=n}    -> Sbml.Binary(Sbml.Power, to_mathML f x, to_mathML f n)
  | Plus l                        -> Sbml.Nary(Sbml.Plus, List.map (fun x -> to_mathML f x) l)
  | Minus {sub1=x;sub2=y}         -> Sbml.Binary(Sbml.Subtract, to_mathML f x, to_mathML f y)
  | Absolute x                    -> Sbml.Unary(Sbml.Abs,to_mathML f x)
  | Log x                         -> Sbml.Unary(Sbml.Log None,to_mathML f x)
  | Modulo {div = x; modulo = y}  -> Sbml.Binary(Sbml.Mod, to_mathML f x, to_mathML f y)
  | Ceiling x                     -> Sbml.Unary(Sbml.Ceiling, to_mathML f x)
  | Floor x                       -> Sbml.Unary(Sbml.Floor, to_mathML f x)
  | Round x                       -> failwith "Conversion of rounding expression to MathML not supported."
  | If (b, e1, e2)                -> failwith "Conversion of conditional expressions to MathML not supported."

let to_matlab (f:'k -> string) (x:t<'k>) = to_string f x

let rec convert (f:'k -> choice<float,'s>) = function
  | Key k -> (
              match f k with 
              | Lib.Left(f) -> Float f 
              | Lib.Right(s) -> Key s
              )
  | Times l                       -> Times (List.map (convert f) l)
  | Divide {div1=l; div2=r}       -> Divide {div1=convert f l; div2=convert f r}
  | Power {base_=l;exponent=r}    -> Power {base_=convert f l;exponent= convert f r}
  | Plus l                        -> Plus (List.map (convert f) l)
  | Minus {sub1=l; sub2=r}        -> Minus {sub1=convert f l;sub2=convert f r}
  | Absolute  x                   -> Absolute (convert f x)
  | Log x                         -> Log (convert f x)
  | Float f                       -> Float f 
  | Modulo {div = x; modulo = y}  -> Modulo   {div = convert f x; modulo = convert f y}
  | Ceiling x                     -> Ceiling (convert f x)
  | Floor x                       -> Floor   (convert f x)  
  | Round x                       -> Round   (convert f x)  
  | If (b, e1, e2)                -> If (convertBool f b, convert f e1, convert f e2)
and convertBool f b =
  let ce x = convert f x
  let cb x = convertBool f x
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> BLT  (ce e1, ce e2)
  | BLeq (e1, e2) -> BLeq (ce e1, ce e2)
  | BEq  (e1, e2) -> BEq  (ce e1, ce e2)
  | BGeq (e1, e2) -> BGeq (ce e1, ce e2)
  | BGT  (e1, e2) -> BGT  (ce e1, ce e2)
  | BNot b1       -> BNot (cb b1)
  | BAnd (b1, b2) -> BAnd (cb b1, cb b2)
  | BOr  (b1, b2) -> BOr  (cb b1, cb b2)

let mentions (e: 'a t) =
  let rec m (acc: 'a list) = function
    | Key x                         -> x::acc
    | Float f                       -> acc
    | Times l                       -> List.fold m acc l
    | Divide {div1=x; div2=y}       -> m (m acc x) y
    | Power {base_=x;exponent=y}    -> m (m acc x) y
    | Plus l                        -> List.fold m acc l
    | Minus {sub1=x; sub2=y}        -> m (m acc x) y
    | Absolute x | Log x            -> m acc x 
    | Modulo {div = x; modulo = y}  -> m (m acc x) y
    | Ceiling x                     
    | Floor x                       
    | Round x                       -> m acc x 
    | If (b, e1, e2)                -> m (m (mBool acc b) e1) e2
  and mBool acc b =
    match b with 
    | BTrue         -> acc
    | BFalse        -> acc
    | BLT  (e1, e2) -> m (m acc e1) e2
    | BLeq (e1, e2) -> m (m acc e1) e2
    | BEq  (e1, e2) -> m (m acc e1) e2
    | BGeq (e1, e2) -> m (m acc e1) e2
    | BGT  (e1, e2) -> m (m acc e1) e2
    | BNot b1       -> mBool acc b1
    | BAnd (b1, b2) -> mBool (mBool acc b1) b2
    | BOr  (b1, b2) -> mBool (mBool acc b1) b2

  in
    List.rev (m [] e)

let rec eval (env:'key -> float) (e:'key t) =
  let f x = eval env x
  match e with
  | Key k                         -> env k
  | Float f                       -> f
  | Times l                       -> List.fold (fun v x -> v * f x) 1.0 l
  | Divide {div1=e1; div2=e2}     -> (f e1) / (f e2)
  | Power{base_=e1;exponent=e2}   -> (f e1) ** (f e2)
  | Plus l                        -> List.fold (fun v x -> v + (f x)) 0.0 l
  | Minus {sub1=e1;sub2=e2}       -> (f e1) - (f e2)
  | Absolute e                    -> abs (f e)
  | Log e                         -> log (f e)
  | Modulo {div = x; modulo = y}  -> moduloFloat (f x) (f y)
  | Ceiling x                     -> ceil (f x)
  | Floor x                       -> floor (f x)
  | Round x                       -> round (f x)
  | If (b, e1, e2)                -> if evalBool env b then f e1 else f e2
and evalBool env b =
  let ee x = eval env x
  let eb x = evalBool env x
  match b with 
  | BTrue         -> true
  | BFalse        -> false
  | BLT  (e1, e2) -> ee e1 <  ee e2
  | BLeq (e1, e2) -> ee e1 <= ee e2
  | BEq  (e1, e2) -> ee e1 =  ee e2
  | BGeq (e1, e2) -> ee e1 >= ee e2
  | BGT  (e1, e2) -> ee e1 >  ee e2
  | BNot b1       -> not (eb b1)
  | BAnd (b1, b2) -> eb b1 && eb b2
  | BOr  (b1, b2) -> eb b1 || eb b2

let split_floats (l:t<'a> list) =
  let rec loop acc_fs (acc_es:t<'a> list) =
    function
    | [] -> List.rev acc_fs, List.rev acc_es // RLP: consider not reversing here
    | Float x :: tail -> loop (x::acc_fs) acc_es tail
    | e :: tail -> loop acc_fs (e::acc_es) tail
  loop [] [] l

let isFactor e = match e with
                 | Key _                   -> true
                 | Times [Float _ ; Key _] -> true
                 | Times [Key _; Float _]  -> true
                 | _                       -> false
                 
let splitFactor x = match x with
                    | Key k                   -> 1.0, k
                    | Times [Float v ; Key k] 
                    | Times [Key k; Float v]  -> v, k
                    | _                       -> failwith "unexpected factor"

let addFactors x y = 
  let f1, k  = splitFactor x
  let f2, _  = splitFactor y
  Times [Float (f1+f2); Key k ]

let rec simpleFactoring (es : t<'z> list) : t<'z> list = 
  match es with 
  | []      -> []
  | x :: xs -> if isFactor x 
                then 
                  let _, key = splitFactor x
                  let factors, xs' = List.partition (fun e -> isFactor e &&
                                                              let _, y = splitFactor e
                                                              in y = key ) xs
                  let x' = List.fold addFactors x factors
                  x' :: simpleFactoring xs'
                else x :: simpleFactoring xs

let rec simplify (x:t<'k>) =
  match x with
  | Key _ -> x
  | Float _ -> x
  | Times l -> 
    match l with
    | [x] -> simplify x
    | _ ->
        if List.exists (fun x -> x = Float 0.0) l
          then Float 0.0
          else let noUnits   = List.filter ((<>) (Float 1.0)) l
               // Times [xs; Times ys] => Times [ xs; ys ]
               let flattened = noUnits |> List.collect (fun x -> match x with 
                                                                 | Times x -> x
                                                                 | _ -> [x])
               // multiply all Float factors
               match flattened |> List.map simplify |> split_floats with
               | [], [] -> Float 1.0
               | fs, [] -> fs |> List.fold (*) 1.0 |> Float
               | fs, es ->
                  let f = fs |> List.fold (*) 1.0 |> Float in
                  if f = Float 0.0 then f
                  elif f = Float 1.0 then Times es
                  else Times (f :: es)

  | Divide {div1=e1; div2=e2} as d -> 
      let e1,e2 = simplify e1, simplify e2 in
        if e1 = Float 0.0 then e1 else div e1 e2
  | Power{base_=e1;exponent=e2} -> 
      let e1,e2 = simplify e1, simplify e2 in
        if e1 = Float 0.0 then e1 else power e1 e2
  | Plus [x] -> simplify x
  // CS: the following simplifications are from the old ModellingEngineDotNet project
  (* CS: writing too many patterns causes the F# compiler to throw an OutOfMemory exception,
         on my machine at least (see: http://stackoverflow.com/questions/10617364/f-compiler-throws-outofmemoryexception).
         So I'm refactoring this part to use fewer patterns.
  *)
  | Plus l ->
      match l |> List.map simplify |> split_floats with
      | [], [] -> Float 0.0
      | fs, [] -> fs |> List.sum |> Float
      | fs, es ->
        let f = fs |> List.sum |> Float in
        let flattened = es |> List.collect (fun x -> match x with 
                                                                 | Plus x -> x
                                                                 | _ -> [x])
        let factored = (simpleFactoring flattened)
        if f = Float 0.0 then 
          if factored.Length = 1
            then factored.Head
            else Plus factored 
        else Plus (f :: factored)

  | Minus{sub1=e1; sub2=e2} -> 
      let e1,e2 = simplify e1, simplify e2 in
        if e1=e2 then Float 0.0 else sub e1 e2
  | Absolute e ->
      match simplify e with
      | Float 0.0 -> Float 0.0
      | Minus { sub1 = Float 0.0; sub2 = e_neg} -> Absolute e_neg
      | e_simpl -> Absolute e_simpl
  | Log e -> Log (simplify e)
  | Modulo {div = x; modulo = y}  -> 
    let x' = simplify x
    let y' = simplify y
    if y' = Float 1.0 || x' = Float 0.0
      then Float 0.0
      else Modulo {div = x'; modulo = y'}
  | Ceiling x   -> Ceiling (simplify x)                  
  | Floor x     -> Floor   (simplify x)                  
  | Round x     -> Round   (simplify x)
  | If (b, e1, e2) -> 
      match simplifyBool b with
        | BTrue  -> simplify e1
        | BFalse -> simplify e2
        | b'     -> If (b', simplify e1, simplify e2)
and simplifyBool (b:bexp<'k>) =
  let b2bool x : bexp<'k> = match x with 
                            | true -> BTrue 
                            | false -> BFalse
  let nop e1 e2 op bop = match simplify e1, simplify e2 with
                         | Float f1, Float f2 -> b2bool (op f1 f2)
                         | e1', e2'           -> bop (e1', e2')
  let bop b1 b2 op bop = match simplifyBool b1, simplifyBool b2 with
                         | BTrue,  BTrue  -> b2bool (op true  true)
                         | BTrue,  BFalse -> b2bool (op true  false)
                         | BFalse, BTrue  -> b2bool (op false true)
                         | BFalse, BFalse -> b2bool (op false false)
                         | b1', b2'       -> bop (b1', b2')
  match b with 
  | BTrue         -> BTrue
  | BFalse        -> BFalse
  | BLT  (e1, e2) -> nop e1 e2 (<)  BLT
  | BLeq (e1, e2) -> nop e1 e2 (<=) BLeq
  | BEq  (e1, e2) -> nop e1 e2 (=)  BEq
  | BGeq (e1, e2) -> nop e1 e2 (>=) BGeq
  | BGT  (e1, e2) -> nop e1 e2 (>)  BGT
  | BNot b1       -> match simplifyBool b1 with 
                      | BTrue   -> BFalse
                      | BFalse  -> BTrue
                      | b1'     -> BNot b1'
  | BAnd (b1, b2) -> bop b1 b2 (&&) BAnd
  | BOr  (b1, b2) -> bop b1 b2 (||) BOr

let is_not_empty (x:t<'k>) =
  (simplify x) <> Float 0.0

(*************** Expression parsing **********************************)
let ( |>> ) a b c = Parser.(|>>) a b c
let ( >>% ) a b c = Parser.(>>%) a b c
let ( .>> ) a b c = Parser.(.>>) a b c
let ( >>. ) a b c = Parser.(>>.) a b c
let ( .>>. ) a b c = Parser.(.>>.) a b c
let ( <|> ) a b = Parser.(<|>) a b
let ( >>= ) a b = Parser.(>>=) a b

let rec exp sp =
  let op = Parser.choice [ 
             Parser.skw "-" >>% (fun a b -> Minus {sub1=a;sub2=b})
             Parser.skw "+" >>% (fun a b -> Plus [a;b])
           ] 
  Parser.chainl1Try (Parser.choice [ Parser.pTry(Parser.kw "-" >>. term sp >>= fun x -> Parser.preturn (Minus {sub1=Float 0.0; sub2=x}))
                                     Parser.pTry(term sp)]) op
and term sp =
  let op = Parser.choice [
             Parser.skw "*" >>% (fun a b -> Times [a;b])
             Parser.skw "%" >>% (fun a b -> Modulo {div = a; modulo = b})
           ] 
  in Parser.chainl1Try (factor sp) op
and factor sp =
  let op = Parser.choice [ 
             Parser.skw "/" >>% (fun a b -> Divide {div1=a;div2=b})
             Parser.skw "^" >>% (fun a b -> Power {base_=a;exponent=b})
             Parser.skw "**" >>% (fun a b -> Power {base_=a;exponent=b})
           ] 
  in Parser.chainl1Try (irr sp) op
  //https://github.com/intellifactory/websharper/issues/680
and e sp s = s |> Parser.paren (exp sp) // inlining this causes stack overflow (in WebSharper first, then further inlining in F#)
and es sp s = s |> Parser.paren (Parser.sepBy (exp sp) (Parser.kw ";"))
and irr sp =
  //let e  s = s |> Parser.paren (exp sp) in // inlining this causes stack overflow
  //let es s = s |> Parser.paren (Parser.sepBy (exp sp) (Parser.kw ";")) in // inlining this causes stack overflow
  let t st =  Parser.choice [
                Parser.pfloat .>> Parser.spaces |>> Float
                Parser.pint32 .>> Parser.spaces |>> (float >> Float)
                Parser.kw "sum"  >>. (es sp) |>> Plus
                Parser.kw "prod" >>. (es sp) |>> Times
                Parser.kw "log" >>. (e sp) |>> Log
                Parser.kw "ceiling"  >>. (e sp) |>> Ceiling
                Parser.kw "floor"  >>. (e sp)   |>> Floor
                Parser.kw "round"  >>. (e sp)   |>> Round
                Parser.kw "if" >>. bexp sp 
                  .>> Parser.kw "then" .>>. exp sp
                  .>> Parser.kw "else" .>>. exp sp
                  >>= fun ((b, e1), e2) -> Parser.preturn (If (b, e1, e2))
                sp
                (e sp)
              ] st 
  in Parser.pTry t
and bterm sp st = 
  Parser.choice [ 
    Parser.kw "true"  >>. Parser.preturn BTrue 
    Parser.kw "false" >>. Parser.preturn BFalse
    exp sp >>= fun e1 -> Parser.choice [
                            Parser.kw "<=" >>. exp sp >>= fun e2 -> Parser.preturn (BLeq (e1, e2))
                            Parser.kw "<"  >>. exp sp >>= fun e2 -> Parser.preturn (BLT  (e1, e2))
                            Parser.kw "="  >>. exp sp >>= fun e2 -> Parser.preturn (BEq  (e1, e2))
                            Parser.kw ">=" >>. exp sp >>= fun e2 -> Parser.preturn (BGeq (e1, e2))
                            Parser.kw ">"  >>. exp sp >>= fun e2 -> Parser.preturn (BGT  (e1, e2))
                         ]
  ] st
and bexp sp st = 
    Parser.choice [
      Parser.kw "not" >>. (bexp sp) >>= (BNot >> Parser.preturn)
      bterm sp >>= fun b1 ->  Parser.choice [ 
                                Parser.kw "&&" >>. bexp sp >>= fun b2 -> Parser.preturn (BAnd (b1, b2))
                                Parser.kw "||" >>. bexp sp >>= fun b2 -> Parser.preturn (BOr (b1, b2))
                                Parser.preturn b1
                              ]
      Parser.paren (bexp sp)
    ] st 
let parse (species:Parser.t<'s>) = exp (species |>> Key)
let parse_boolean (species:Parser.t<'s>) = bexp (species |>> Key)
let from_string (fs:Parser.t<'s>) (s:string) = Parser.from_string (parse fs) s