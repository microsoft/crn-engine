// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.GEC.Database

open Microsoft.Research.GEC
open FSBOL.ComponentDefinition
open FSBOL.ModuleDefinition
open FSBOL.SBOLDocument
//open Microsoft.Research.ModellingEngine
open Microsoft.Research.CRNEngine
open Parser


(* ************************************************************************************************* *)

(* Devices. *)
type device = string * string list

(* The different kinds of property that part types can have. *)
type pcrProperty = CODES of string list * float
type promProperty = POS of string list * float * float * float
                  | NEG of string list * float * float * float
                  | CON of float
                  | FRATE of Ast.aexp
type rbsProperty = RATE of float


(* An encoding of part types, along with their properties. *)
type partType = PCR of pcrProperty
              | PROM of promProperty list
              | RBS of rbsProperty
              | TER

(* Compute FS(Q^t). *)
val speciesInPartType : partType -> string list list

(* ************************************************************************************************* *)

(* A "database" consists of a "parts database" and a "reaction database".
   A "parts database" is just a mapping of part identifiers (i.e. strings) to their part types.
   A "reaction database" is just a list of reactions. *)
type 'a entry = { value:'a; enabled:bool; comments:string }
type t = { parts:partType entry Stringmap.t; devices: device list; reactions:Gecreaction.t entry list }

val partTypeToSBOL: string -> (partType entry) -> ComponentDefinition
val createModuleDefinitions: (ComponentDefinition list) -> ((string*(partType entry))list) -> (ModuleDefinition list)
val convertTableToSBOLDocument: t -> SBOLDocument
val createProteinCDs: (string * (partType entry)) list -> ComponentDefinition list
(* The "empty" database. *)
val empty : t

(* Produce a string representation of a database. *)
val display : t -> string

(* Add a part to the database (part id must be unique). *)
//val addPart : t -> bool -> string -> string -> string -> string -> t

(* Add a reaction to the database. *)
//val addReaction : t -> bool -> string -> string -> t

type parser = Parser.t<t>
val parse : parser

type dnacomponent = 
    | Part of string * partType
    | Device of device

val partParser: Parser.t<dnacomponent>

