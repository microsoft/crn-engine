module CrnLanguage
open Fable.Core
open ReactCodePad

// This is the F# equivalent of the TypeScript CRN language definition from CrnLang.ts. The references for understanding what's going on here are:
// https://microsoft.github.io/monaco-editor/monarch.html
// biocomputing/HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/CrnLang
// biocomputing/ReactComponents/FableElmishSample/Shared/SharedCodePad

let keywords = [|"CNil";"END";"Nil";"SNil";"agent";"and";"as";"binding";"bool";"comp";"compilation";"conc";"copy";"data";"deterministic";"directive";"edge";"event";"ff";"float";"force";"gen";"if";"inference";"init";"inside";"int";"kappa";"module";"moments";"mutation";"new";"node";"not";"or";"parameters";"phos";"plot";"plot_settings";"predicate";"query";"rate";"rates";"scale";"script";"simulation";"simulations";"simulator";"sundials";"spatial";"specout";"spec";"stochastic";"string";"sum";"sweeps";"synthesis";"system";"task";"tt";"units";"vol"|]
let attributes = [|"doublecircle";"circle";"diamond";"box";"point";"ellipse";"record";"inv";"invdot";"dot";"dashed";"dotted";"filled";"back";"forward"|]
let symbols = JS.Constructors.RegExp.Create "[=><!~?:&|+\-*\/\^%]+"

let root = [|
    // Identifiers and keywords.
    Short { Reg = JS.Constructors.RegExp.Create "[a-zA-Z_\\x80-\\xFF][\w\\x80-\\xFF]*" |> RegExp
            Action = { EmptyExpanded with Cases = Some [|("@keywords","keyword");("@attributes","constructor");("@default","identifier")|] } |> ExpandedAct
            Next = None }
    // Whitespace
    Expanded { Reg = None ; Action = None ; Include = Some "@whitespace" }
    // Delimiters and operators.
    Short { Reg = JS.Constructors.RegExp.Create "[{}()\[\]]" |> RegExp ; Action = ShortAct "@brackets" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "@symbols" |> RegExp
            Action = { EmptyExpanded with Cases = Some [|("@keywords","keyword");("@default","operator")|] } |> ExpandedAct
            Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "[;|]" |> RegExp ; Action = ShortAct "delimiter" ; Next = None }
    // Numbers.
    Short { Reg = JS.Constructors.RegExp.Create "\d*\.\d+([eE][\-+]?\d+)?" |> RegExp ; Action = ShortAct "number.float" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "0[xX][0-9a-fA-F]+" |> RegExp ; Action = ShortAct "number.hex" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "\d+" |> RegExp ; Action = ShortAct "number" ; Next = None }
    // Strings
    Short { Reg = JS.Constructors.RegExp.Create "\"([^\"\\\\]|\\\\.)*$" |> RegExp ; Action = ShortAct "string.invalid" ; Next = None } // Non-terminated string.
    Short { Reg = JS.Constructors.RegExp.Create "\"" |> RegExp
            Action = { EmptyExpanded with Token = Some "string.quote" ; Bracket = Some "@open" ; Next = Some "@string" } |> ExpandedAct
            Next = None }
|]

let comment = [|
    Short { Reg = JS.Constructors.RegExp.Create "[^\/*]+" |> RegExp ; Action = ShortAct "comment" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "\(\*" |> RegExp ; Action = ShortAct "comment" ; Next = Some "@push" } // Nested comment.
    Short { Reg = String "\\*\\)" ; Action = ShortAct "comment" ; Next = Some "@pop" }
    Short { Reg = JS.Constructors.RegExp.Create "[\/*]" |> RegExp ; Action = ShortAct "comment" ; Next = None }
|]

let whitespace = [|
    Short { Reg = JS.Constructors.RegExp.Create "[ \\t\r\n]+" |> RegExp ; Action = ShortAct "white" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "\(\*" |> RegExp ; Action = ShortAct "comment" ; Next = Some "@comment" }
    Short { Reg = JS.Constructors.RegExp.Create "\/\/.*$" |> RegExp ; Action = ShortAct "comment" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "#.$" |> RegExp ; Action = ShortAct "comment" ; Next = None }
|]

let string = [|
    Short { Reg = JS.Constructors.RegExp.Create "[^\\\\\"&]+" |> RegExp ; Action = ShortAct "string" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "\\\\\"" |> RegExp ; Action = ShortAct "string.escape" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "&\w+;" |> RegExp ; Action = ShortAct "string.escape" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "[\\\\&]" |> RegExp ; Action = ShortAct "string" ; Next = None }
    Short { Reg = JS.Constructors.RegExp.Create "\"" |> RegExp ; Action = { EmptyExpanded with Token = Some "string.quote" ; Bracket = Some "@close" ; Next = Some "@pop" } |> ExpandedAct ; Next = None }
|]

let language : IMonarchLanguage = {
    Extras = [|("keywords",EStrings keywords);("attributes",EStrings attributes);("symbols",ERegExp symbols)|]
    Tokenizer = [|("root",root);("comment",comment);("string",string);("whitespace",whitespace)|]
    IgnoreCase = None
    Unicode = None
    DefaultToken = None
    Brackets = None
    Start = None
    TokenPostfix = None
}