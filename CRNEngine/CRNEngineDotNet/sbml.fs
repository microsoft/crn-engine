// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Sbml

(* sbml.ml - types representing elements of SBML documents, with to_string functions. *)

(* Possible future work:
 * =====================
 * - Use classes, accessors and mutators etc. to make the code less verbose???
 * - Implement SBase, i.e. MetaID and SBOTerm?
 * - Factor out some functionality like the construction of multi-element XML data? *)

(* *************************** *)
(*      HELPER FUNCTIONS.      *)
(* *************************** *)

(* Fail functions. *)
let failimpl () = failwith "Not implemented yet!"

(* Some commonly-used strings. *)
#if JavaScript
let newline = "\r\n"
#else
let newline = System.Environment.NewLine
#endif
let tab = "\t"
let quotemark = "\""
let space = " "
let empty = ""
let equals = "="
let indent_string = space + space

(* Indent a string the given amount. *)
let indent i s =
  let rec inner = function
    0 -> s
  | n -> indent_string + (inner (n-1))
  in
  if i<0 then failwith "Sbml.indent: negative indentation" else inner i

(* More functions for constructing strings. *)
let quote s = quotemark + s + quotemark
(*let glue (xs:string list) = Lib.fold_left (^) "" xs *)
let string_of_bool (b:bool) = match b with true -> "true" | false -> "false"

(* An (almost) map over strings is useful! *)
let stringMap f str =
  let acc = ref "" in
  String.iter (fun c -> acc := !acc + (f c)) str;
  !acc

(* The "iteri" function over strings is also missing from the OCaml standard library. *)
let stringIteri f str =
  for i = 0 to ((String.length str) - 1) do
    f i str.[i]
  done
  
(* A map over options is also useful. *)
let oMap f xo = match xo with None -> None | Some x -> Some (f x)

(* Convert parameters of a tag to a string. *)
let string_of_params parameters =
  match parameters with
  | [] -> empty
  | parameters ->
      let string_of_param (p,v) = match v with None -> empty | Some v -> space + p + equals + (quote v) in
      parameters |> List.map string_of_param |> List.reduce (fun a b -> a + empty + b)

(* Construct an opening tag. *)
let tagopen i s parameters = indent i ("<" + s + (string_of_params parameters) + ">")

(* Construct a closing tag. *)
let tagclose i s = indent i ("</" + s + ">")

(* Construct a "single" tag. *)
let tagsingle i s parameters = indent i ("<" + s + (string_of_params parameters) + "/>")
let tagsimple i s = tagsingle i s []

(* Construct the initial XML tag *)
let xmltag parameters = "<?xml" + (string_of_params parameters) + "?>"

(* The standard pre- and postamble for SBML documents. *)
let preamble = 
  (xmltag [("version", Some "1.0");("encoding", Some "UTF-8")]) + newline +
  (tagopen 0 "sbml" [("xmlns", Some "http://www.sbml.org/sbml/level2/version4");
                     ("level", Some "2"); ("version", Some "4")]) + newline
let postamble = tagclose 0 "sbml"

(* Turn a "list of elements" into a string. *)
let xml_of_elmtlist sb (i:int) (f:'a -> unit) (title:string) (elmts:'a list) =
  match elmts with
  | [] -> ()
  | elmts ->
        Stringbuilder.append sb (tagopen i title []);
        Stringbuilder.append sb (newline);
        List.iter (fun x -> (f x; Stringbuilder.append sb (newline))) elmts;
        Stringbuilder.append sb (tagclose i title);
        Stringbuilder.append sb (newline)

(* ***************** *)
(* SBML IDENTIFIERS. *)
(* ***************** *)

(* Strings can't contain any instances of opening brackets! They get mistaken for XML tags. *)
type sbmlString = STR of string
let openAngle = "&lt;"
let closeAngle = ">"
let mkString s = STR (stringMap (function '<' -> openAngle | '>' -> closeAngle | c -> string c) s)
let string_of_SBMLString (STR s) = s

(* The "testId" function tests to see whether a given string
 * adheres to the definition of a SId (or UnitSId),
 * and throws an exception if it doesn't. *)
exception NotId of string
let isLetter c = (('a' <= c) && (c <= 'z')) || (('A' <= c) && (c <= 'Z'))
let isUnderscore c = (c = '_')
let isDigit c = ('0' <= c) && (c <= '9')
let testId (s:string) =
  let valid i c =
    match i with
    | 0 -> if not ((isLetter c) || (isUnderscore c)) then raise (NotId s)
    | i -> if not ((isLetter c) || (isUnderscore c) || (isDigit c)) then raise (NotId s)
  in
  stringIteri valid s

(* SId *)
type sId = SID of string
let mkSId (s:string) = begin testId s; SID s end
let string_of_SId (SID s) = s

(* UnitSId *)
type unitSId = UNITSID of string
let mkUnitSId (s:string) = begin testId s; UNITSID s end
let string_of_UnitSId (UNITSID s) = s

(* SBOTerm *)
type sboTerm = SBOTERM of string
exception NotSBOTerm of string
let testSBOTerm (s:string) =
  let len = String.length s in
  if (len = 11) then
    let initial,final = s.Substring(0,4),s.Substring(4,7) in
    if (initial = "SBO:") then
      if String.forall isDigit final then () else raise (NotSBOTerm s)
    else raise (NotSBOTerm s)
  else raise (NotSBOTerm s)
let mkSBOTerm (s:string) = begin testSBOTerm s; SBOTERM s end
let string_of_SBOTerm (SBOTERM s) = s

(* ************************************************************* *)
(* CODE FOR (SOME OF) THE SUBSET OF MATHML THAT IS USED IN SBML. *)
(* ************************************************************* *)

(* Unary operators. *)
type uOp = Not     | Neg     | Abs     | Exp     | Ln      | Floor
         | Sin     | Cos     | Tan     | Sec     | Csc     | Cot    
         | Arcsin  | Arccos  | Arctan  | Arcsec  | Arccsc  | Arccot
         | Sinh    | Cosh    | Tanh    | Sech    | Csch    | Coth
         | Arcsinh | Arccosh | Arctanh | Arcsech | Arccsch | Arccoth
         | Root of mathExpr option (* degree   *)
         | Log  of mathExpr option (* log base *)
         | Ceiling | Factorial
(* Binary operators. *)
and bOp = Neq | Divide | Power | Subtract | Max | Mod
(* n-ary operators. *)
and nOp = And | Or | Xor | Plus | Times | Eq | Leq | Lt | Geq | Gt
(* Constants. *)
and constant = Int of int | Float of float | Bool of bool | Notanumber | Pi | Infinity | Exponentiale
(* Qualifiers. *)
and qualifier = Degree of mathExpr | Logbase of mathExpr (* | BVar of ??? *)
(* Expression that can get turned into MathML. *)
and mathExpr = Unary of uOp * mathExpr
             | Binary of bOp * mathExpr * mathExpr
             | Nary of nOp * (mathExpr list)
             | Const of constant * unitSId option (* The unit is not standard, but recognized by Copasi *)
             | Identifier of sId

(* Convert MathML operators etc into strings, for tags. *)
let string_of_uOp =
  function
    Not -> "not" | Neg -> "minus" | Abs -> "abs" | Exp -> "exp" | Ln -> "ln"
  | Floor -> "floor" | Ceiling -> "ceiling" | Factorial -> "factorial" | Root _ -> "root"
  | Log _ -> "log" | Sin -> "sin" | Cos -> "cos" | Tan -> "tan" | Sec -> "sec" | Csc -> "csc"
  | Cot -> "cot" | Arcsin -> "arcsin" | Arccos -> "arccos" | Arctan -> "arctan" | Arcsec -> "arcsec"
  | Arccsc -> "arccsc" | Arccot -> "arccot" | Sinh -> "sinh" | Cosh -> "cosh" | Tanh -> "tanh"
  | Sech -> "sech" | Csch -> "csch" | Coth -> "coth" | Arcsinh -> "arcsinh" | Arccosh -> "arccosh"
  | Arctanh -> "arctanh" | Arcsech -> "arcsech" | Arccsch -> "arccsch" | Arccoth -> "arccoth"
let string_of_bOp = function Neq -> "neq" | Divide -> "divide" | Power -> "power" | Subtract -> "minus" | Max -> "max" | Mod -> "mod"
let string_of_nOp = 
  function 
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Plus -> "plus"
  | Times -> "times"
  | Eq -> "eq"
  | Leq -> "leq"
  | Lt -> "lt"
  | Geq -> "geq"
  | Gt -> "gt"
let string_of_qualifier = function Degree _ -> "degree" | Logbase _ -> "logbase"

(* Produce an XML representation for an expression from MathML. *)
let rec xml_of_mathExpr sb i = function
    Unary (op,m) ->
      let rec doOpening = function
        | Root None -> doOpening (Root(Some(Const(Int 2, None))))
        | Log  None -> doOpening (Log(Some(Const(Int 10, None))))
        | Root (Some x) -> Stringbuilder.append sb (tagopen i (string_of_uOp op) []); Stringbuilder.append sb newline; xml_of_qualifier sb (i+1) (Degree x)
        | Log  (Some x) -> Stringbuilder.append sb (tagopen i (string_of_uOp op) []); Stringbuilder.append sb newline; xml_of_qualifier sb (i+1) (Logbase x)
        |  op -> Stringbuilder.append sb (tagopen i (string_of_uOp op) [])
      in
      doOpening op; Stringbuilder.append sb newline; (xml_of_mathExpr sb (i+1) m);
      Stringbuilder.append sb (tagclose i (string_of_uOp op)); Stringbuilder.append sb newline
  | Binary (op,m,m') -> Stringbuilder.append sb (tagopen i "apply" []); Stringbuilder.append sb newline;
                        Stringbuilder.append sb (tagsimple (i+1) (string_of_bOp op)); Stringbuilder.append sb newline;
                        (xml_of_mathExpr sb (i+1) m); Stringbuilder.append sb newline;
                        (xml_of_mathExpr sb (i+1) m'); Stringbuilder.append sb newline;
                        Stringbuilder.append sb (tagclose i "apply")
  | Nary (op,ms)     -> Stringbuilder.append sb (tagopen i "apply" []); Stringbuilder.append sb newline;
                        Stringbuilder.append sb (tagsimple (i+1) (string_of_nOp op)); Stringbuilder.append sb newline;
                        (List.iter (fun m -> xml_of_mathExpr sb (i+1) m; Stringbuilder.append sb newline) ms);
                        Stringbuilder.append sb (tagclose i "apply")
  | Const (c, u_opt) ->
      let unit_decl = match u_opt with None -> [] | Some uid -> [("units", Some (string_of_UnitSId uid))] in
      begin
        match c with
            Int n        -> Stringbuilder.append sb (tagopen i "cn" (("type", Some "integer")::unit_decl)); Stringbuilder.append sb (string n); Stringbuilder.append sb (tagclose 0 "cn")
          | Float f      -> Stringbuilder.append sb (tagopen i "cn" (("type", Some "real")::unit_decl)); Stringbuilder.append sb (string f); Stringbuilder.append sb (tagclose 0 "cn")
          | Bool b       -> Stringbuilder.append sb (tagsimple i (string_of_bool b))
          | Notanumber   -> Stringbuilder.append sb (tagsimple i "notanumber")
          | Pi           -> Stringbuilder.append sb (tagsimple i "pi")
          | Infinity     -> Stringbuilder.append sb (tagsimple i "infinity")
          | Exponentiale -> Stringbuilder.append sb (tagsimple i "exponentiale")
      end
  | Identifier x -> Stringbuilder.append sb (tagopen i "ci" []); Stringbuilder.append sb (string_of_SId x); Stringbuilder.append sb (tagclose 0 "ci")
and xml_of_qualifier sb i = function
  | Degree x ->
      let str = "degree" in
      Stringbuilder.append sb (tagopen i str []); Stringbuilder.append sb newline;
      (xml_of_mathExpr sb (i+1) x); Stringbuilder.append sb newline; Stringbuilder.append sb (tagclose i str)
  | Logbase x ->
      let str = "logbase" in
      Stringbuilder.append sb (tagopen i str []); Stringbuilder.append sb newline;
      (xml_of_mathExpr sb (i+1) x);  Stringbuilder.append sb newline; Stringbuilder.append sb (tagclose i str)

(* Produce an overall XML string for a MathML element. *)
let xml_of_MathML sb i m =
  Stringbuilder.append sb (tagopen i "math" [("xmlns", Some "http://www.w3.org/1998/Math/MathML")]);
  Stringbuilder.append sb newline; (xml_of_mathExpr sb (i+1) m); Stringbuilder.append sb newline; Stringbuilder.append sb (tagclose i "math")

(* Extra bits mentioned in the MathML section of the SBML spec that aren't dealt with above:
type token = CSymbol | Sep
type general = Piecewise | Piece | Otherwise | Lambda (* takes BVar *)
type annotation = Semantics | Annotation | AnnotationXml
*)

(* ********************************************** *)
(*      TYPES FOR A SUBSET OF SBML.               *)
(*      ALSO, FUNCTIONS FOR ADDING DATA           *)
(*      AND PRODUCING STRING REPRESENTATIONS.     *)
(* ********************************************** *)

(* **************** *)
(* UNIT DEFINITION. *)
(* **************** *)
type unitKind = Ampere | Becqerel | Candela (*| Celsius*) | Coulomb | Dimensionless | Farad | Gram | Gray | Henry | Hertz | Item | Joule | Katal | Kelvin | Kilogram | Litre | Lumen | Lux | Metre | Mole | Newton | Ohm | Pascal | Radian | Second | Siemens | Sievert | Steradians | Tesla | Volt | Watt | Weber
let string_of_unitKind = function
  | Ampere -> "ampere"
  | Becqerel -> "becqerel"
  | Candela -> "candela"
(*  | Celsius -> "Celsius" *)
  | Coulomb -> "coulomb"
  | Dimensionless -> "dimensionless"
  | Farad -> "farad"
  | Gram -> "gram"
  | Gray -> "gray"
  | Henry -> "henry"
  | Hertz -> "hertz"
  | Item -> "item"
  | Joule -> "joule"
  | Katal -> "katal"
  | Kelvin -> "kelvin"
  | Kilogram -> "kilogram"
  | Litre -> "litre"
  | Lumen -> "lumen"
  | Lux -> "lux"
  | Metre -> "metre"
  | Mole -> "mole"
  | Newton -> "newton"
  | Ohm -> "ohm"
  | Pascal -> "pascal"
  | Radian -> "radian"
  | Second -> "second"
  | Siemens -> "siemens"
  | Sievert -> "sievert"
  | Steradians -> "steradians"
  | Tesla -> "tesla"
  | Volt -> "volt"
  | Watt -> "watt"
  | Weber-> "weber"

type sbmlUnit = { kind : unitKind;
                  exponent : int option;
                  scale : int option;
                  multiplier : float option;
                  offset : float option
                }

let create_sbmlUnit x = { kind       = x;
                          exponent   = None;
                          scale      = None;
                          multiplier = None;
                          offset     = None
                        }

(* Add data to an sbmlUnit. *)
let u_set_kind       x (u:sbmlUnit) = { u with kind = x }
let u_set_exponent   x (u:sbmlUnit) = { u with exponent = Some x }
let u_set_scale      x (u:sbmlUnit) = { u with scale = Some x }
let u_set_multiplier x (u:sbmlUnit) = { u with multiplier = Some x }
let u_set_offset     x (u:sbmlUnit) = { u with offset = Some x }

let xml_of_sbmlUnit sb i u = Stringbuilder.append sb (tagsingle
                                                            i
                                                            "unit"
                                                            [("kind",       Some (string_of_unitKind u.kind))
                                                             ("exponent",   oMap string u.exponent)
                                                             ("scale",      oMap string u.scale)
                                                             ("multiplier", oMap string u.multiplier)
                                                             ("offset",     oMap string u.offset)])

type unitDefinition = { id   : unitSId;
                        name : sbmlString option;
                        listOfUnits : sbmlUnit list
                      }

let create_unitDefinition x l = { id          = x;
                                  name        = None;
                                  listOfUnits = l
                                }

(* Add data to a unitDefinition. *)
let ud_set_id          x (ud:unitDefinition) = { ud with id = x }
let ud_set_name        x (ud:unitDefinition) = { ud with name = Some x }
let ud_set_listOfUnits x (ud:unitDefinition) = { ud with listOfUnits = x }

let xml_of_unitDefinition sb i ud =
  Stringbuilder.append sb (tagopen i "unitDefinition" [("id", Some (string_of_UnitSId ud.id));("name", oMap string_of_SBMLString ud.name)]);
  Stringbuilder.append sb newline;
   (xml_of_elmtlist sb (i+1) (xml_of_sbmlUnit sb (i+2))     "listOfUnits"     ud.listOfUnits);
  Stringbuilder.append sb (tagclose i "unitDefinition")


(* ************ *)
(* COMPARTMENT. *)
(* ************ *)
type compartment = { id                : sId;
                     name              : sbmlString option;
                     compartmentType   : sId option;
                     spatialDimensions : int option;
                     size              : float option; (* Make this mandatory to avoid SBML validation warning? *)
                     units             : unitSId option;
                     outside           : sId option;
                     constant          : bool option
                   }

let create_compartment x = { id                = x;
                             name              = None;
                             compartmentType   = None;
                             spatialDimensions = None;
                             size              = None;
                             units             = None;
                             outside           = None;
                             constant          = None
                           }

(* Add data to a compartment. *)
let cmt_set_id                x (c:compartment) = { c with id = x }
let cmt_set_name              x (c:compartment) = { c with name = Some x }
let cmt_set_compartmentType   x (c:compartment) = { c with compartmentType = Some x }
let cmt_set_spatialDimensions x (c:compartment) = { c with spatialDimensions = Some x }
let cmt_set_size              x (c:compartment) = { c with size = Some x }
let cmt_set_units             x (c:compartment) = { c with units = Some x }
let cmt_set_outside           x (c:compartment) = { c with outside = Some x }
let cmt_set_constant          x (c:compartment) = { c with constant = Some x }

(* Produce an XML string for a compartment. *)
let xml_of_compartment sb i c = Stringbuilder.append sb (tagsingle
                                                                i
                                                                "compartment"
                                                                [("id",                Some (string_of_SId c.id));
                                                                 ("name",              oMap string_of_SBMLString c.name);
                                                                 ("compartmentType",   oMap string_of_SId c.compartmentType);
                                                                 ("spatialDimensions", oMap string c.spatialDimensions);
                                                                 ("size",              oMap string c.size);
                                                                 ("units",             oMap string_of_UnitSId c.units);
                                                                 ("outside",           oMap string_of_SId c.outside);
                                                                 ("constant",          oMap string_of_bool c.constant)])

(* ******** *)
(* SPECIES. *)
(* ******** *)
type species = { id                    : sId;
                 name                  : sbmlString option;
                 speciesType           : sId option;
                 compartment           : sId;
                 initialAmount         : float option;
                 initialConcentration  : float option;
                 substanceUnits        : unitSId option;
                 hasOnlySubstanceUnits : bool option;
                 boundaryCondition     : bool option;
                 charge                : int option; (* deprecated *)
                 constant              : bool option
               }

let create_species idx cmtx = { id                    = idx;
                                name                  = None;
                                speciesType           = None;
                                compartment           = cmtx;
                                initialAmount         = None;
                                initialConcentration  = None;
                                substanceUnits        = None;
                                hasOnlySubstanceUnits = None;
                                boundaryCondition     = None;
                                charge                = None;
                                constant              = None
                              }

(* Add data to a species. *)
let sp_set_id                    x (s:species) = { s with id = x }
let sp_set_name                  x (s:species) = { s with name = Some x }
let sp_set_speciesType           x (s:species) = { s with speciesType = Some x }
let sp_set_compartment           x (s:species) = { s with compartment = x }
let sp_set_initialAmount         x (s:species) = { s with initialAmount = Some x }
let sp_set_initialConcentration  x (s:species) = { s with initialConcentration = Some x }
let sp_set_substanceUnits        x (s:species) = { s with substanceUnits = Some x }
let sp_set_hasOnlySubstanceUnits x (s:species) = { s with hasOnlySubstanceUnits = Some x }
let sp_set_boundaryCondition     x (s:species) = { s with boundaryCondition = Some x }
let sp_set_charge                x (s:species) = { s with charge = Some x }
let sp_set_constant              x (s:species) = { s with constant = Some x }

(* Produce an XML string for a species. *)
let xml_of_species sb i s = Stringbuilder.append sb (tagsingle
                                                            i
                                                            "species"
                                                            [("id",                    Some (string_of_SId s.id))
                                                             ("name",                  oMap string_of_SBMLString s.name)
                                                             ("speciesType",           oMap string_of_SId s.speciesType)
                                                             ("compartment",           Some (string_of_SId s.compartment))
                                                             ("initialAmount",         oMap string s.initialAmount)
                                                             ("initialConcentration",  oMap string s.initialConcentration)
                                                             ("substanceUnits",        oMap string_of_UnitSId s.substanceUnits)
                                                             ("hasOnlySubstanceUnits", oMap string_of_bool s.hasOnlySubstanceUnits)
                                                             ("boundaryCondition",     oMap string_of_bool s.boundaryCondition)
                                                             ("charge",                oMap string s.charge)
                                                             ("constant",              oMap string_of_bool s.constant)])

(* SPECIES REFERENCE. *)
type speciesReference = { id            : sId option;
                          name          : sbmlString option;
                          species       : sId;
                          stoichiometry : float option;
                        }

(* Create a speciesReference for a particular species. *)
let create_speciesReference speciesx = { id            = None;
                                         name          = None;
                                         species       = speciesx;
                                         stoichiometry = None;
                                       }

(* Add data to a speciesReference. *)
let sr_set_id      x (sr:speciesReference) = { sr with id = Some x }
let sr_set_name    x (sr:speciesReference) = { sr with name = Some x }
let sr_set_species x (sr:speciesReference) = { sr with species = x }
let sr_set_stoichiometry  x (sr:speciesReference) = { sr with stoichiometry = Some x }

(* Produce an XML string for a speciesReference. *)
let xml_of_speciesReference sb i sr = Stringbuilder.append sb (tagsingle
                                                                    i
                                                                    "speciesReference"
                                                                    [("id",      oMap string_of_SId sr.id)
                                                                     ("name",    oMap string_of_SBMLString sr.name)
                                                                     ("species", Some (string_of_SId sr.species))
                                                                     ("stoichiometry", oMap string sr.stoichiometry)])

(* ********** *)
(* PARAMETER. *)
(* ********** *)
type parameter = { id       : sId;
                   name     : sbmlString option;
                   value    : float option; (*Should this change to expression?*)
                   units    : unitSId option;
                   constant : bool option
                 }

(* Create a parameter. *)
let create_parameter idx = { id       = idx;
                             name     = None;
                             value    = None;
                             units    = None;
                             constant = None
                           }

(* Add data to a parameter. *)
let p_set_id       x (p:parameter) = { p with id = x }
let p_set_name     x (p:parameter) = { p with name = Some x }
let p_set_value    x (p:parameter) = { p with value = Some x }
let p_set_units    x (p:parameter) = { p with units = Some x }
let p_set_constant x (p:parameter) = { p with constant = Some x }

(* Produce an XML string for a parameter. *)
let xml_of_parameter sb i p = Stringbuilder.append sb (tagsingle
                                                            i
                                                            "parameter"
                                                            [("id",       Some (string_of_SId p.id))
                                                             ("name",     oMap string_of_SBMLString p.name)
                                                             ("value",    oMap string p.value)
                                                             ("units",    oMap string_of_UnitSId p.units)
                                                             ("constant", oMap string_of_bool p.constant)])

(* ******************* *)
(* INITIAL ASSIGNMENT. *)
(* ******************* *)
type initialAssignment = { symbol : sId;
                           math   : mathExpr
                         }

(* Create an initial assignment *)
let create_initialAssignment s m = { symbol = s;
                                     math =  m
                                   }

let xml_of_initialAssignment sb i ia = 
   Stringbuilder.append sb (tagopen i "initialAssignment" ["symbol", Some (string_of_SId ia.symbol)]);
   Stringbuilder.append sb newline; (xml_of_MathML sb (i+1) ia.math); Stringbuilder.append sb newline;
   Stringbuilder.append sb (tagclose i "initialAssignment")

(* ************ *)
(* KINETIC LAW. *)
(* ************ *)
type kineticLaw = { math             : mathExpr;
                    listOfParameters : parameter list
                  }

(* Create a kinetic law. *)
let create_kineticLaw m = { math             = m;
                            listOfParameters = []
                          }

(* Add data to a kinetic law. *)
let kl_set_math       x (kl:kineticLaw) = { kl with math = x }
let kl_set_parameters x (kl:kineticLaw) = { kl with listOfParameters = x }
let kl_add_parameter  x (kl:kineticLaw) = { kl with listOfParameters = (kl.listOfParameters @ [x]) }

(* Produce an XML string from a kinetic law. *)
let xml_of_kineticLaw sb i kl =
   Stringbuilder.append sb (tagopen i "kineticLaw" []);
   Stringbuilder.append sb newline; (xml_of_MathML sb (i+1) kl.math); Stringbuilder.append sb newline;
   (xml_of_elmtlist sb (i+1) (xml_of_parameter sb (i+2)) "listOfParameters" kl.listOfParameters);
   Stringbuilder.append sb (tagclose i "kineticLaw")

(* ********* *)
(* REACTION. *)
(* ********* *)
type reaction = { id              : sId;
                  name            : sbmlString option;
                  reversible      : bool option;
                  fast            : bool option;
                  listOfReactants : speciesReference list;
                  listOfProducts  : speciesReference list;
                  (* listOfModifiers : ??? *)
                  kineticLaw      : kineticLaw option
                }

(* Create a reaction initially. *)
let create_reaction idx =
    { id              = idx;
      name            = None;
      reversible      = None;
      fast            = None;
      listOfReactants = [];
      listOfProducts  = [];
      kineticLaw      = None
    }

(* Add data to a reaction. *)
let r_set_id         x (r:reaction) = { r with id = x }
let r_set_name       x (r:reaction) = { r with name = Some x }
let r_set_reversible x (r:reaction) = { r with reversible = Some x }
let r_set_fast       x (r:reaction) = { r with fast = Some x }
let r_set_reactants  x (r:reaction) = { r with listOfReactants = x }
let r_add_reactant   x (r:reaction) = { r with listOfReactants = r.listOfReactants @ [x] }
let r_set_products   x (r:reaction) = { r with listOfProducts = x }
let r_add_product    x (r:reaction) = { r with listOfProducts = r.listOfProducts @ [x] }
let r_set_kineticLaw x (r:reaction) = { r with kineticLaw = Some x }

(* Produce an XML string for a reaction. *)
let xml_of_reaction sb i r =
  Stringbuilder.append sb (tagopen i "reaction"
    [("id",         Some (string_of_SId r.id));
     ("name",       oMap string_of_SBMLString r.name);
     ("reversible", oMap string_of_bool r.reversible);
     ("fast",       oMap string_of_bool r.fast)]);
  Stringbuilder.append sb newline;
  xml_of_elmtlist sb (i+1) (xml_of_speciesReference sb (i+2)) "listOfReactants" r.listOfReactants;
  xml_of_elmtlist sb (i+1) (xml_of_speciesReference sb (i+2)) "listOfProducts"  r.listOfProducts;
  (match r.kineticLaw with None -> () | Some kl -> (xml_of_kineticLaw sb (i+1) kl; Stringbuilder.append sb newline));
  Stringbuilder.append sb (tagclose i "reaction")

(* ****** *)
(* MODEL. *)
(* ****** *)
type model = { id                        : sId option;
               name                      : sbmlString option;
(*               listOfFunctionDefinitions : functionDefinition list; *)
               listOfUnitDefinitions     : unitDefinition list;
(*               listOfCompartmentTypes    : compartmentType list;    *)
(*               listOfSpeciesTypes        : speciesType list;        *)
               listOfCompartments        : compartment list;
               listOfSpecies             : species list;
               listOfParameters          : parameter list;
               listOfInitialAssignments  : initialAssignment list;
(*               listOfRules               : rule list;               *)
(*               listOfConstraints         : constraint list;         *)
               listOfReactions           : reaction list;
(*               listOfEvents              : event list               *)
             }
type t = model
(* The initial model is all empty! *)
let empty_model = { id = None;
                    name = None;
(*                    listOfFunctionDefinitions = []; *)
                    listOfUnitDefinitions = [];
(*                    listOfCompartmentTypes = [];    *)
(*                    listOfSpeciesTypes = [];        *)
                    listOfCompartments = [];
                    listOfSpecies = [];
                    listOfParameters = [];
                    listOfInitialAssignments = [];
(*                    listOfRules = [];               *)
(*                    listOfConstraints = [];         *)
                    listOfReactions = [];
(*                    listOfEvents = [];              *)
                  }

(* Add data to a model. *)
let model_set_id                 x (m:model) = { m with id = Some x }
let model_set_name               x (m:model) = { m with name = Some x }
(* let model_set_functionDefinition x (m:model) = { m with listOfFunctionDefinitions = m.listOfFunctionDefinitions @ [x] } *)
let model_add_unitDefinition     x (m:model) = { m with listOfUnitDefinitions = m.listOfUnitDefinitions @ [x] }
(* let model_set_compartmentType    x (m:model) = { m with listOfCompartmentTypes = m.listOfCompartmentTypes @ [x] }       *)
(* let model_set_speciesType        x (m:model) = { m with listOfSpeciesTypes = m.listOfSpeciesTypes @ [x] }               *)
let model_set_compartments       x (m:model) = { m with listOfCompartments = x }
let model_add_compartment        x (m:model) = { m with listOfCompartments = m.listOfCompartments @ [x] }
let model_set_species            x (m:model) = { m with listOfSpecies = x }
let model_add_species            x (m:model) = { m with listOfSpecies = m.listOfSpecies @ [x] }
let model_set_parameters         x (m:model) = { m with listOfParameters = x }
let model_add_parameter          x (m:model) = { m with listOfParameters = m.listOfParameters @ [x] }
let model_add_initialAssignment  x (m:model) = { m with listOfInitialAssignments = m.listOfInitialAssignments @ [x] }
(* let model_add_rule               x (m:model) = { m with listOfRules = m.listOfRules @ [x] }                             *)
(* let model_add_constraint         x (m:model) = { m with listOfConstraints = m.listOfConstraints @ [x] }                 *)
let model_set_reactions          x (m:model) = { m with listOfReactions = x }
let model_add_reaction           x (m:model) = { m with listOfReactions = m.listOfReactions @ [x] }
(* let model_add_event              x (m:model) = { m with listOfEvents = m.listOfEvents @ [x] }                           *)

(* Produce an XML string from a model. *)
let xml_of_model sb i m = 
  Stringbuilder.append sb (tagopen i "model" [("id", oMap string_of_SId m.id);("name", oMap string_of_SBMLString m.name)]);
  Stringbuilder.append sb newline;
(*   (xml_of_elmtlist sb (i+1) (xml_of_functionDefinition sb (i+2)) "listOfFunctionDefinitions" m.listOfFunctionDefinitions); *)
   (xml_of_elmtlist sb (i+1) (xml_of_unitDefinition sb (i+2))     "listOfUnitDefinitions"     m.listOfUnitDefinitions);
(*   (xml_of_elmtlist sb (i+1) (xml_of_compartmentType sb (i+2))    "listOfCompartmentTypes"    m.listOfCompartmentTypes);    *)
(*   (xml_of_elmtlist sb (i+1) (xml_of_speciesType sb (i+2))        "listOfSpeciesTypes"        m.listOfSpeciesTypes);        *)
  (xml_of_elmtlist sb (i+1) (xml_of_compartment sb (i+2))        "listOfCompartments"        m.listOfCompartments);
  (xml_of_elmtlist sb (i+1) (xml_of_species sb (i+2))            "listOfSpecies"             m.listOfSpecies);
  (xml_of_elmtlist sb (i+1) (xml_of_parameter sb (i+2))          "listOfParameters"          m.listOfParameters);
  (xml_of_elmtlist sb (i+1) (xml_of_initialAssignment sb (i+2))  "listOfInitialAssignments"  m.listOfInitialAssignments);
(*   (xml_of_elmtlist sb (i+1) (xml_of_rule sb (i+2))               "listOfRules"               m.listOfRules);               *)
(*   (xml_of_elmtlist sb (i+1) (xml_of_constraint sb (i+2))         "listOfConstraints"         m.listOfConstraints);         *)
  (xml_of_elmtlist sb (i+1) (xml_of_reaction sb (i+2))           "listOfReactions"           m.listOfReactions);
(*   (xml_of_elmtlist sb (i+1) (xml_of_event sb (i+2))              "listOfEvents"              m.listOfEvents);              *)
  Stringbuilder.append sb (tagclose i "model");
  Stringbuilder.append sb newline

(* ******************************************* *)
(*      THE OVERALL "TO_XML" FUNCTION.         *)
(* ******************************************* *)

let to_xml (m:model) =
  let sb = Stringbuilder.init preamble in
  xml_of_model sb 1 m;
  Stringbuilder.append sb postamble;
  Stringbuilder.value sb
