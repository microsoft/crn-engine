// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Sbml
(* sbml.ml - types representing SBML documents, with to_string functions.
 * Currently working towards Level 2, Version 4, Release 1 of SBML. *)

(* ********************************************** *)
(*      TYPES FOR A SUBSET OF SBML.               *)
(*      ALSO, FUNCTIONS FOR ADDING DATA           *)
(*      AND PRODUCING STRING REPRESENTATIONS.     *)
(* ********************************************** *)

(* ***************** *)
(* SBML IDENTIFIERS. *)
(* ***************** *)

(* String. *)
type sbmlString
val mkString : string -> sbmlString
val string_of_SBMLString : sbmlString -> string

(* SId *)
type sId
val mkSId : string -> sId
val string_of_SId : sId -> string

(* UnitSId *)
type unitSId
val mkUnitSId : string -> unitSId
val string_of_UnitSId : unitSId -> string

(* SBOTerm *)
type sboTerm
val mkSBOTerm : string -> sboTerm
val string_of_SBOTerm : sboTerm -> string

(* **************** *)
(* UNIT DEFINITION. *)
(* **************** *)
type unitKind = Ampere | Becqerel | Candela (*| Celsius*) | Coulomb | Dimensionless | Farad | Gram | Gray | Henry | Hertz | Item | Joule | Katal | Kelvin | Kilogram | Litre | Lumen | Lux | Metre | Mole | Newton | Ohm | Pascal | Radian | Second | Siemens | Sievert | Steradians | Tesla | Volt | Watt | Weber

type sbmlUnit
val create_sbmlUnit  : unitKind -> sbmlUnit
val u_set_kind       : unitKind -> sbmlUnit -> sbmlUnit
val u_set_exponent   : int -> sbmlUnit -> sbmlUnit
val u_set_scale      : int -> sbmlUnit -> sbmlUnit
val u_set_multiplier : float -> sbmlUnit -> sbmlUnit
val u_set_offset     : float -> sbmlUnit -> sbmlUnit

type unitDefinition
val create_unitDefinition : unitSId -> sbmlUnit list -> unitDefinition
val ud_set_id             : unitSId -> unitDefinition -> unitDefinition
val ud_set_name           : sbmlString -> unitDefinition -> unitDefinition
val ud_set_listOfUnits    : sbmlUnit list -> unitDefinition -> unitDefinition

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

(* ************ *)
(* COMPARTMENT. *)
(* ************ *)
type compartment
val create_compartment        : sId -> compartment
val cmt_set_id                : sId -> compartment -> compartment
val cmt_set_name              : sbmlString -> compartment -> compartment
val cmt_set_compartmentType   : sId -> compartment -> compartment
val cmt_set_spatialDimensions : int -> compartment -> compartment
val cmt_set_size              : float -> compartment -> compartment
val cmt_set_units             : unitSId -> compartment -> compartment
val cmt_set_outside           : sId -> compartment -> compartment
val cmt_set_constant          : bool -> compartment -> compartment

(* ******** *)
(* SPECIES. *)
(* ******** *)
type species
val create_species               : sId -> sId -> species
val sp_set_id                    : sId -> species -> species
val sp_set_name                  : sbmlString -> species -> species
val sp_set_speciesType           : sId -> species -> species
val sp_set_compartment           : sId -> species -> species
val sp_set_initialAmount         : float -> species -> species
val sp_set_initialConcentration  : float -> species -> species
val sp_set_substanceUnits        : unitSId -> species -> species
val sp_set_hasOnlySubstanceUnits : bool -> species -> species
val sp_set_boundaryCondition     : bool -> species -> species
val sp_set_charge                : int -> species -> species
val sp_set_constant              : bool -> species -> species

(* ****************** *)
(* SPECIES REFERENCE. *)
(* ****************** *)
type speciesReference
val create_speciesReference : sId -> speciesReference
val sr_set_id            : sId -> speciesReference -> speciesReference
val sr_set_name          : sbmlString -> speciesReference -> speciesReference
val sr_set_species       : sId -> speciesReference -> speciesReference
val sr_set_stoichiometry : float -> speciesReference -> speciesReference

(* ********** *)
(* PARAMETER. *)
(* ********** *)
type parameter
val create_parameter : sId -> parameter
val p_set_id       : sId -> parameter -> parameter
val p_set_name     : sbmlString -> parameter -> parameter
val p_set_value    : float -> parameter -> parameter
val p_set_units    : unitSId -> parameter -> parameter
val p_set_constant : bool -> parameter -> parameter

(* ******************* *)
(* INITIAL ASSIGNMENT. *)
(* ******************* *)
type initialAssignment
val create_initialAssignment : sId -> mathExpr -> initialAssignment

(* ************ *)
(* KINETIC LAW. *)
(* ************ *)
type kineticLaw
val create_kineticLaw : mathExpr -> kineticLaw
val kl_set_math       : mathExpr -> kineticLaw -> kineticLaw
val kl_set_parameters : parameter list -> kineticLaw -> kineticLaw
val kl_add_parameter  : parameter -> kineticLaw -> kineticLaw

(* ********* *)
(* REACTION. *)
(* ********* *)
type reaction
val create_reaction  : sId -> reaction
val r_set_id         : sId -> reaction -> reaction
val r_set_name       : sbmlString -> reaction -> reaction
val r_set_reversible : bool -> reaction -> reaction
val r_set_fast       : bool -> reaction -> reaction
val r_set_reactants  : speciesReference list -> reaction -> reaction
val r_add_reactant   : speciesReference -> reaction -> reaction
val r_set_products   : speciesReference list -> reaction -> reaction
val r_add_product    : speciesReference -> reaction -> reaction
val r_set_kineticLaw : kineticLaw -> reaction -> reaction

(* ****** *)
(* MODEL. *)
(* ****** *)
type model
type t = model
val empty_model : model
val model_set_id                : sId -> model -> model
val model_set_name              : sbmlString -> model -> model
val model_add_unitDefinition    : unitDefinition -> model -> model
val model_set_compartments      : compartment list -> model -> model
val model_add_compartment       : compartment -> model -> model
val model_set_species           : species list -> model -> model
val model_add_species           : species -> model -> model
val model_set_parameters        : parameter list -> model -> model
val model_add_parameter         : parameter -> model -> model
val model_add_initialAssignment : initialAssignment -> model -> model
val model_set_reactions         : reaction list -> model -> model
val model_add_reaction          : reaction -> model -> model

(* ******************************************* *)
(*      THE OVERALL "TO_XML" FUNCTION.         *)
(* ******************************************* *)

val to_xml : model -> string
