[<JavaScript>]
module Microsoft.Research.CRNEngine.LegacyParser

// parses a CRN written in the deprecated Silver Light version of the CRN tool, and updates it to the new syntax
val parse_legacy_SL : Parser.t<Species> -> Parser.t<Crn>