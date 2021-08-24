// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.Errors
open Microsoft.Research.DNA
open Microsoft.Research.CRNEngine

(* Types for the different kinds of type error we might report. *)
type typeErrorKind =
  | UnboundVariable of string
  | WrongType of Types.t * (Types.t list) * string
  | ModuleArgs of string * int * int
  | TupleArgs of string * int * int
  | ToeInconsistent of Types.range * bool
  | UnTetheredInOrigami of string
  | TetheredOutside of string
  | RepeatedSpec of string * string
  | NoSeq of string
  | UnknownSpec of string * string
  | UnknownModule of string * string

let string_of_typeErrorKind = function
    UnboundVariable n -> "unbound variable " + n
  | WrongType(t,ts,s) ->
      "found " + (Types.type_to_string t) + " but expected " +
        (Types.types_to_string ts) + "\n(" + s + ")"
  | ModuleArgs(x,i,j) ->
      "module " + x + " applied to " + (string i) +
      " arguments, expected " + (string j)
  | ToeInconsistent(r,b) -> 
      "domain is inconsistently used as a " + (if b then " toe hold " else " normal strand ") + 
      " but at " + (Types.format_range (Types.rangeBegin(r)) (Types.rangeEnd(r))) + " was used as the other"   
  | UnTetheredInOrigami(s) ->
      "species " + s + " within origami should contain one tethered domain"
  | TetheredOutside(s) -> "tethered domain " + s + " should not be found outside of an origami"
  | TupleArgs(x,i,j) ->
      "module " + x + " applied to a tuple value of length " +
      (string i) + ", with a pattern expecting " + (string j) + " values" 
  | RepeatedSpec(x,s) ->
      "declaration " + x + " contains more than one " + s + " specification"
  | NoSeq(x) -> "new " + x + " contains an unexpected seq specification"
  | UnknownSpec(x,s) -> "declaration " + x + " contains an unknown specification " + s
  | UnknownModule (s, vs) -> "undefined module " + s + vs
 
(* Types for ALL the different kinds of error we might report. *)
type errorKind = SyntaxError of string | CommentError | CharError | TypeError of typeErrorKind
               | DNAIllegalCharError of char | DNADuplicateError of string
               | DNALongToeholdError of string | DNAShortSpecificityError of string
               | LeakRatesError of float * float | DomainLengthsError of int * int
               | InteractingNonToeholdError of string * string
               | IllegalPolymerisationError of string * string
               | SecondaryStructureError of string * string
               | NeighbouringToeholdsError of string * string * string

(* Produce error messages. *)
let string_of_errorKind = function
    SyntaxError m -> "Syntax error: " + m
  | CommentError -> "Comment not terminated"
  | CharError -> "Illegal character"
  | TypeError k -> "Type error: " + (string_of_typeErrorKind k)
  | DNAIllegalCharError c -> "Illegal character \'" + (Lib.string_of_char c) + "\' in DNA sequence"
  | DNADuplicateError s -> "Duplicate DNA sequence " + s
  | DNALongToeholdError s -> "Toehold sequence " + s + " is longer than 9 nucleotides"
  | DNAShortSpecificityError s -> "Specificity sequence " + s + " is shorter than 10 nucleotides"
  | LeakRatesError (l,w) -> "Leak rate w " + Lib.paren(Lib.display_float w) + " must be smaller than l " + Lib.paren(Lib.display_float l)
  | DomainLengthsError (th,sp) ->
      "Default toehold domain length " + Lib.paren(string th) +
      " must be shorter than default specificity domain lenghth " + Lib.paren(string sp)
  | InteractingNonToeholdError(s1,s2) -> "Non-toehold domains " + s1 + " and " + s2 + " are both exposed and could interact"
  | IllegalPolymerisationError(s1,s2) -> "Species " + s1 + " and " + s2 + " could interact to produce a polymer"
  | SecondaryStructureError (s1,s2) -> "Species " + s1 + " and " + s2 + " could interact to produce non-transient secondary structure"
  | NeighbouringToeholdsError (s,t1,t2) -> "Species " + s + " contains neightbouring exposed toeholds " + t1 + " and " + t2


let makeMessage (r:Types.range) (k:errorKind) =
    let basestring = (string_of_errorKind k) + "."
    match r with
    | Some r_ -> (r_ ||> Types.format_range) + ": " + basestring
    | _ -> basestring

type DSDException (r:Types.range, k:errorKind) =
    inherit Parser.Exception(makeMessage r k,match r with None -> [||] | Some ((r,c),_) -> [|{row = r;column=c;text=string_of_errorKind k}|])
    member x.BeginPos = match r with None -> None | _ -> Some(Types.rangeBegin r)
    member x.EndPos = match r with None -> None | _ -> Some(Types.rangeEnd r)

(* Raise errors. *)
let type_error r k = 
  match k with
  | WrongType (_, expecteds, _) -> if expecteds.IsEmpty
                                           then raise(new DSDException(r, TypeError k))
                                           else raise(new DSDException(Types.getBeginPos expecteds.Head, TypeError k))
  | _                                  -> raise(new DSDException(r, TypeError k))
let char_error p p'                     = raise(new DSDException(Some(p,p'),CharError))
let comment_error p p'                  = raise(new DSDException(Some(p,p'),CommentError))
let syntax_error p p' m                 = raise(new DSDException(Some(p,p'),SyntaxError m))
let dna_char_error c                    = raise(new DSDException(None, DNAIllegalCharError c))
let dna_dup_error s                     = raise(new DSDException(None, DNADuplicateError s))
let dna_toe_error s                     = raise(new DSDException(None, DNALongToeholdError s))
let dna_spec_error s                    = raise(new DSDException(None, DNAShortSpecificityError s))
let leak_rates_error l w                = raise(new DSDException(None, LeakRatesError(l,w)))
let domain_lengths_error th sp          = raise(new DSDException(None, DomainLengthsError(th,sp)))
let interacting_nontoehold_error d d'   = raise(new DSDException(None, InteractingNonToeholdError(d,d')))
let illegal_polymerisation_error m m'   = raise(new DSDException(None, IllegalPolymerisationError(m,m')))
let secondary_structure_error m m'      = raise(new DSDException(None, SecondaryStructureError(m,m')))
let neighbouring_toeholds_error m t1 t2 = raise(new DSDException(None, NeighbouringToeholdsError(m,t1,t2)))
