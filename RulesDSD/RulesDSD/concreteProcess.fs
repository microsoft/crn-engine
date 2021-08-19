[<JavaScript>]
module RulesDSD.ConcreteProcess

open RulesDSD.Syntax

(*
   The main purpose of this module is to provide an intermediate data structure 'ProcessC' that strips a Syntax.Process
   of Term-related components, thus providing a simpler data structure to render in the tool.
*)

// the suffix 'C' is for 'concrete', and it helps to separate this module's namespace from Syntax's namespace
