// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Parser

type pos = int * int
type parse_state = string * pos
type Result<'a> = OkEmpty      of 'a * parse_state
                | OkConsumed   of 'a * parse_state
                | FailEmpty    of string * pos
                | FailConsumed of string * pos

type t<'a> = parse_state -> Result<'a> 

type ParserResult<'a> =
  | Success of 'a * string * pos
  | Failure of string * string * string

type error = { row: int; column: int; text: string}
[<Class>]
type Exception =
  new : msg:string * errors:error[] -> Exception
  inherit System.Exception
  member Errors: error[]

val start_pos : int * int
// val mk_error : pos -> string -> Result<'a>
val mk_failEmpty    : pos -> string -> Result<'a>
val mk_failConsumed : pos -> string -> Result<'a>
val add_cols : int -> pos -> pos
val add_rows : int -> pos -> pos
val add_row : (pos -> pos)
val pTry : t<'a> -> t<'a>
val isLetter : char -> bool
val isDigit : char -> bool
val isWhiteSpace : char -> bool
val move : string -> pos -> pos
val linebreak : t<unit>
val newline : t<char>
val skipNewline : t<unit>
val ( |>> ) : t<'a>  -> ('a -> 'b) -> t<'b>
val ( >>% ) : t<'a>  -> 'b -> t<'b>
val ( |~> ) : t<'a>  -> (pos -> 'a -> 'b) -> t<'b>
val ( >>= ) : t<'a>  -> ('a -> t<'b>) -> t<'b>
val preturn    : 'a     -> t<'a>  (* the return operation in the Parsec monad *)
val ( .>>. ) : t<'a> -> t<'b>  -> t<'a * 'b>
val ( .>> )  : t<'a> -> t<'b>  -> t<'a>
val ( >>. )  : t<'a> -> t<'b>  -> t<'b>
val ( +>>+ ) : t<'a> -> t<'b>  -> t<'a * 'b>
val ( +>> )  : t<'a> -> t<'b>  -> t<'a>
val ( >>+ )  : t<'a> -> t<'b>  -> t<'b>
val ( <|> )  : t<'a> -> t<'a>  -> t<'a>
val ( <?> )  : t<'a> -> string -> t<'a>
val restOfLine : skipNewline:bool -> t<string>
val failParser : string -> t<'a>
val pstring : string -> t<string>
val skipString : string -> t<unit>
#if JavaScript

#else
val skipCharsTillString : str:string -> skipString:bool -> maxCount:int -> s:string * p:pos -> Result<string>
#endif
val opt : t<'a> -> t<'a option>
val pipe3 : t<'a> -> t<'b> -> t<'c> -> ('a -> 'b -> 'c -> 'd) -> t<'d>
//val applies : parse_state -> t<'a> -> ('a * parse_state) option
val choice : seq<t<'a>> -> t<'a>
val pfix    : ('a -> t<'a>) -> 'a -> t<'a>
val between : t<'a> -> t<'b> -> t<'c> -> t<'c>
val many : t<'a> -> t<'a list>
val many1 : t<'a> -> t<'a list>
val manyTill: t<'a> -> t<'b> -> t<'a list>
val eof : t<unit>
val manyChars: t<char> -> t<string>
val satisfy: (char -> bool) -> t<char>
val manySatisfy : (char -> bool) -> t<string>
val many1Satisfy : (char -> bool) -> t<string>
val sepBy : t<'a> -> t<'b> -> t<'a list>
val sepBy1 : t<'a> -> t<'b> -> t<'a list>
val sepBy2 : t<'a> -> t<'b> -> t<'a list>
val sepEndBy1 : t<'a> -> t<'b> -> t<'a list>
val sepEndBy : t<'a> -> t<'b> -> t<'a list>
val chainl1 : t<'a> -> t<('a -> 'a -> 'a)> -> t<'a>
val chainl1Try : t<'a> -> t<('a -> 'a -> 'a)> -> t<'a>
val commentLine: t<unit>
val commentMultiline: unit -> t<unit>
val spaces : t<string>
val spaces1 : t<string>
val digits : t<string>
val digits1 : t<string>
val sign : t<string>
val forced_sign : t<string>
val guarded : (string -> 'a) -> t<'a0> -> t<'a>
val such_that : ('a -> bool) -> t<'a> -> t<'a>
val anyChar : t<char>
val pchar  : char -> t<char>
val pint32 : t<int32>
val pfloat : t<float>
val pbool : t<bool>
val pcolour : t<string>
val attempt : t<'a> -> t<'a>
val run : t<'a> -> string -> ParserResult<'a>
val run_result : t<'a> -> string -> Result<'a>
val lineSeparators : string []
val lookAhead : string -> t<unit>
val plookAhead : t<'a> -> t<unit>
// tries to run parser t<'a> and undoes any input consumption
val plookAheadWith : t<'a> -> t<'a>


//Copied definitions from parser.ml
val name : t<string>
val name_kw: (string list) -> t<string>
val kw : string -> t<string>
val skw : string -> t<string>
val bracket : string -> string -> (t<'a> -> t<'a>)
val paren         : t<'a> -> t<'a>
val sqBrackets    : t<'a> -> t<'a>
val angleBrackets : t<'a> -> t<'a>
val braces        : t<'a> -> t<'a>
val list_of : t<'a> -> t<'a list>
val list_of1 : t<'a> -> t<'a list> // non-empty list
val tuple_of : t<'a> -> t<'a list>
val record : 'a -> (string * t<'a->'a>) list -> t<'a>
val from_string : t<'a> -> string -> 'a
val from_string_first_error : t<'a> -> string -> (int * int) option
val from_string_find_errors : t<'a> -> string -> 'a option * error []
val from_string_first_error_with_msg : t<'a> -> string -> Choice<'a, error>

[<Class>]
type ParserBuilder =
    member Bind : t<'a> * ('a -> t<'b>) -> t<'b>
    member Return : 'a -> t<'a>
    member ReturnFrom : t<'a> -> t<'a>
    member Combine : t<'a> * t<'b> -> t<'a * 'b>
    member Delay : (unit -> t<'a>) -> t<'a>
    member Zero : unit -> t<'a>

val parse : ParserBuilder