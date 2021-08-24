// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.GEC.Gecreaction

open Microsoft.Research.GEC
open Parser

//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine

open System.Diagnostics

(* A type for "database reactions", which records some of the structure of complexes. *)
type normalReacData = { catalysts: string list list;
                        reactants: string list list;
                        products: string list list;
                        rate: float }
type transportReacData = { reactant: string list;
                           product: string list;
                           rate: float;
                           compartment: string;
                           direction: Ast.direction }
type t = Normal of normalReacData
       | Transport of transportReacData

(* Functions to create GEC reaction datatypes. *)
let makeNormal (catalysts:string list list) (reactants:string list list) (products:string list list) (rate:float) =
  Normal {catalysts=catalysts; reactants=reactants; products=products; rate=rate}
let makeTransport (reactant:string list) (product:string list) (rate:float) (compartment:string) (direction:Ast.direction) =
  Transport {reactant=reactant; product=product; rate=rate; compartment=compartment; direction=direction}

(* Decide whether a reaction is a normal or a transport reaction... *)
let isNormal (r:t) = match r with Normal r -> Some (r.catalysts, r.reactants, r.products, r.rate) | _ -> None
let isTransport (r:t) = match r with Transport r -> Some (r.reactant, r.product, r.rate, r.compartment, r.direction) | _ -> None

(* Produce a string representation of a GEC reaction. *)
let display (r:t) : string = 
  let mkStr xss = Lib.string_of_list Ast.complexString " + " xss in
  match r with
    | Normal r -> let prefix = match r.catalysts with [] -> "" | _ -> (mkStr r.catalysts) + " ~ " in
                  prefix + (mkStr r.reactants) + " ->{" + (Lib.display_float r.rate) + "} " + (mkStr r.products)
    | Transport r -> let reactantStr,productStr =
                        match r.direction with
                        | Ast.In -> Ast.complexString r.reactant, Ast.compartmentString r.compartment (Ast.complexString r.product)
                        | Ast.Out -> Ast.compartmentString r.compartment (Ast.complexString r.reactant), Ast.complexString r.product
                     in
                     reactantStr + " ->{" + (Lib.display_float r.rate) + "} " + productStr

(* Are two reactions equal? Must consider reordering of reactions/products/catalysts and the ordering of species within complexes themselves... *)
let equal (r1:t) (r2:t) =
  match r1,r2 with
  | Normal r1, Normal r2 -> (Lib.is_permutation Ast.complexesEqual r1.catalysts r2.catalysts) &&
                            (Lib.is_permutation Ast.complexesEqual r1.reactants r2.reactants) &&
                            (Lib.is_permutation Ast.complexesEqual r1.products r2.products) &&
                            (r1.rate = r2.rate)
  | Transport r1, Transport r2 -> (Ast.complexesEqual r1.reactant r2.reactant) &&
                                  (Ast.complexesEqual r1.product r2.product) &&
                                  (r1.rate = r2.rate) &&
                                  (r1.compartment = r2.compartment) &&
                                  (r1.direction = r2.direction)
  | _,_ -> false

(* Get all species names from a GEC reaction. *)
let species (r:t) : string list list = 
  let allRawSpecies =
    match r with
    | Normal r -> r.catalysts @ r.reactants @ r.products
    | Transport r -> [r.reactant; r.product]
  in
  Lib.remove_duplicates Ast.complexesEqual allRawSpecies

(* Apply a substitution to a GEC reaction. *)
let applySubst (theta:Subst.t) (r:t) : t =
  match r with
  | Normal r -> Normal { r with catalysts = List.map (Subst.applyToComplex theta) r.catalysts;
                                reactants = List.map (Subst.applyToComplex theta) r.reactants;
                                 products = List.map (Subst.applyToComplex theta) r.products }
  | Transport r -> Transport { r with reactant = Subst.applyToComplex theta r.reactant;
                                       product = Subst.applyToComplex theta r.product }


let lookaheadLinebreak = Parser.pTry (Parser.linebreak >>. Parser.failParser "" <|> Parser.satisfy Parser.isWhiteSpace >>. preturn ())
let lookaheadDashSeparator = Parser.pTry(Parser.pstring "->" >>. failParser "" <|> Parser.pstring "-")
 // Parser.satisfy (fun c -> Parser.isWhiteSpace c && c <> '\n')
//let whiteSpacenlb : t<unit> = skipChar isWhiteSpace <?> "a white space"

let spacesnlb :t<string>  = fun st -> 
  match many  (commentLine <|> commentMultiline () <|> lookaheadLinebreak) <| st with
  | OkEmpty    (_, st') -> OkEmpty ("", st')
  | OkConsumed (_, st') -> OkEmpty ("", st')
  | FailEmpty _         -> OkEmpty ("", st)
  | FailConsumed (e, p) -> FailConsumed (e, p)

let kwnlb s = pstring s .>> spacesnlb

let gecName = (Parser.sepBy Parser.name (Parser.pstring "::")) .>> spacesnlb
let bracketnlb l r = Parser.between (Parser.pstring l) (Parser.spaces >>. kwnlb r)
let parennlb a      = bracketnlb "(" ")" a 
let sqBracketnlb a = bracketnlb "[" "]" a
let bracesnlb a = bracketnlb "{" "}" a
let compartmentParser = (sqBracketnlb gecName)
let rateParser = bracesnlb Parser.pfloat
let chainNames = Parser.sepBy gecName (Parser.kw "+")

let parseReaction = chainNames >>= fun n ->  
    Parser.choice[
        compartmentParser .>> Parser.kw "->" .>>. rateParser .>>. gecName |>> fun(((reactant:string list),rate:float),(product:string list)) -> Transport {reactant = reactant; product=product;rate = rate;compartment = n.Head.Head; direction = Ast.direction.Out}
        Parser.skw "~" >>. 
            Parser.choice[
                Parser.kw "->" >>. rateParser .>>. chainNames |>> fun((rate:float),(products:string list list)) -> Normal {catalysts=n;reactants = [];rate=rate;products=products}
                chainNames .>> Parser.kw "->" .>>. rateParser .>>. chainNames |>> fun(((reactants:string list list),rate:float),(products:string list list)) -> Normal {catalysts=n;reactants = reactants;rate=rate;products=products}
            ]
        Parser.kw "->" >>. rateParser .>>. chainNames >>= fun (r,x) -> 
            Parser.choice[
                compartmentParser |>> fun(product) -> Transport {reactant = n.Head;product=product;rate=r;compartment=x.Head.Head;direction=Ast.direction.In}
                Parser.preturn (Normal {catalysts=[];reactants = n;products=x;rate=r})
            ]
    ]



//let parseTransportOut = compartmentParser .>> Parser.kw "->" .>>. rateParser .>>. gecName |>> fun ( ((compartment:string, (reactant:string list)), rate:float), (product: string list)) -> Transport {reactant=reactant; product=product; rate=rate; compartment=compartment; direction=Ast.direction.Out}
//let parseTransportIn = gecName .>> Parser.kw "->" .>>. rateParser .>>. compartmentParser |>> fun(((reactant:string list),rate:float),(compartment:string,(product:string list))) -> Transport {reactant=reactant; product=product; rate=rate; compartment=compartment; direction=Ast.direction.In}


