[<JavaScript>]
module Microsoft.Research.CRNEngine.Operators

let (=>) a b = a,b

let ( |>> ) a b c = Parser.( |>> ) a b c
let ( >>= ) a b c = Parser.( >>= ) a b c
let ( .>>. ) a b c = Parser.( .>>. ) a b c
let ( .>> ) a b c = Parser.( .>> ) a b c
let ( >>. ) a b c = Parser.( >>. ) a b c
let ( <|> ) a b c = Parser.( <|> ) a b c
let ( +>>+ ) a b c = Parser.( +>>+ ) a b c
let ( >>+ ) a b c = Parser.( >>+ ) a b c
let ( >>% ) a b c = Parser.( >>% ) a b c
let ( <?> ) a b c = Parser.( <?> ) a b c
let ( +>> ) a b c = Parser.( +>> ) a b c
let ( |~> ) a b = Parser.( |~> ) a b
