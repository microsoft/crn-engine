// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module ReactCodePad
open System.Text.RegularExpressions
open FSharp.Collections
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions

(*
This is the Fable wrapper for reactcodepad. The correct way to use this is to create a CodePadProps object, and pass it to monaco.
The F# declaration for ReactCodePad is:

let inline codePad (props : CodePadProps list) (elems : ReactElement list) : ReactElement =
    ofImport "CodePad" "reactcodepad" (keyValueList CaseRules.LowerFirst props) elems

This is not declared in this file, in order to avoid acquiring a dependency on Fable.Import.React. It should be declared in the client, but this file can also be used in a server.

Also, note that you have to add the Monaco plugin to Webpack configuration:

var MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

Then, add this to the plugins: 

new MonacoWebpackPlugin()
*)

type CodePadProps =
| Width of obj
| Height of obj
| Language of string
| Text of string
| Theme of string
| OnTextChange of (string -> unit)
| DefaultFileName of string

// What follows are the types required to create a Monarch definition for syntax highlighting. This closely matches the types used in Monaco. It may be possible to simplify this, with some work.
// See https://microsoft.github.io/monaco-editor/monarch.html as well as the sample in FableElmishSample/Client/CrnLanguage, or the language declarations in the CRN tools projects.

// Note: Monarch language definitions are TS objects that make heavy use of JavaScript's ability to have objects with more fields than the ones declared in their types. This is not easily possible in F#. Instead, in this interface I am adding an "Extras" field that can be used to add any additional objects (strings or regexes) that would normally be referenced by Monarch as extra fields.

// Note: pay attention to escaping. It's subtly different in .NET compared to JS. For example, in F# you always need to escape double quotes, and you'll need to double-escape backslashes (i.e. \\\\) in strings so that the regular expression gets an escaped backslash (i.e. \\).

// Note: do not use the standard .NET Regex constructor. Use JS.Constructors.RegExp.Create instead. This is required because Fable will add the g (global) flag by default to any .NET Regex object, and this will prevent syntax highlighting from working.

// Note: as an alternative to all of this, you can define the language in TS/JS following the Monarch documentation in the link above, then import it into Fable as native JS, and then call RegisterLanguageJS on it. An example of how this is done is in biocomputing/ReactComponents/FableElmishSample/src/Client/App.fs.

type IShortMonarchLanguageAction = string

type IExpandedMonarchLanguageAction = {
    Group: IMonarchLanguageAction[] option
    Cases: (string*string)[] option
    Token: string option
    Next: string option
    SwitchTo: string option
    GoBack: int option
    Bracket: string option
    NextEmbedded: string option
    Log: string option
}
and IMonarchLanguageAction =
| ShortAct of IShortMonarchLanguageAction
| ExpandedAct of IExpandedMonarchLanguageAction
| ShortActs of IShortMonarchLanguageAction[]
| ExpandedActs of IExpandedMonarchLanguageAction[]

let EmptyExpanded = { Group = None ; Cases = None; Token = None; Next = None; SwitchTo = None; GoBack = None; Bracket = None; NextEmbedded = None; Log = None }

type Reg = String of string | RegExp of Regex

type IShortMonarchLanguageRule = {
    Reg: Reg
    Action: IMonarchLanguageAction
    Next: string option
}
type IExpandedMonarchLanguageRule = {
    Reg: Reg option
    Action: IMonarchLanguageAction option
    Include: string option
}
type IMonarchLanguageRule =
| Short of IShortMonarchLanguageRule
| Expanded of IExpandedMonarchLanguageRule

type IMonarchLanguageBracket = {
    Open: string
    Close: string
    Token: string
}

type Extra = EStrings of string[] | ERegExp of Regex

// This is the object you should create to define syntax highlighting.
type IMonarchLanguage = {
    Extras: (string*Extra)[]
    Tokenizer: (string*IMonarchLanguageRule[])[]
    IgnoreCase: bool option
    Unicode: bool option
    DefaultToken: string option
    Brackets: IMonarchLanguageBracket[] option
    Start: string option
    TokenPostfix: string option
}

/// Registers a Monarch definition for syntax highlighting. After calling this, any Monaco editor instanced that specifies the given id as its language will use the provided syntax highlighting. 
[<Import("RegisterLanguage", from="reactcodepad")>]
let RegisterLanguageJS (id:string, language:obj) : unit = jsNative

/// Converts an IMonarchLanguage instance to a Monarch definition for syntax highlighting and registers it. After calling this, any Monaco editor instanced that specifies the given id as its language will use the provided syntax highlighting.
let RegisterLanguage (id:string, language:IMonarchLanguage) : unit =
    let makePojo (map:(string*string)[]) : obj =
        let ret = obj()
        map |> Array.iter (fun (k,v) -> ret.[k] <- v)
        ret
    let makePojoReg (reg:Reg) : obj =
        match reg with
        | String str -> str :> obj
        | RegExp regExp -> regExp :> obj
    let rec makePojoAction (action:IMonarchLanguageAction) : obj =
        match action with
        | ShortActs shorts ->
            shorts :> obj
        | ExpandedActs expandeds ->
            let ret = expandeds |> Array.map (ExpandedAct >> makePojoAction)
            ret :> obj
        | ShortAct short ->
            short :> obj
        | ExpandedAct expanded ->
            let ret = obj()
            if expanded.Group.IsSome then ret.["group"] <- expanded.Group.Value |> Array.map makePojoAction
            if expanded.Cases.IsSome then ret.["cases"] <- expanded.Cases.Value |> makePojo
            if expanded.Token.IsSome then ret.["token"] <- expanded.Token.Value
            if expanded.Next.IsSome then ret.["next"] <- expanded.Next.Value
            if expanded.SwitchTo.IsSome then ret.["switchTo"] <- expanded.SwitchTo.Value
            if expanded.GoBack.IsSome then ret.["goBack"] <- expanded.GoBack.Value
            if expanded.Bracket.IsSome then ret.["bracket"] <- expanded.Bracket.Value
            if expanded.NextEmbedded.IsSome then ret.["nextEmbedded"] <- expanded.NextEmbedded.Value
            if expanded.Log.IsSome then ret.["log"] <- expanded.Log.Value
            ret
    let makePojoLanguageRule (rule:IMonarchLanguageRule) : obj =
        match rule with
        | Short short ->
            if short.Next.IsSome then
                [|makePojoReg short.Reg;makePojoAction short.Action;short.Next.Value:>obj|] :> obj
            else
                [|makePojoReg short.Reg;makePojoAction short.Action|] :> obj
        | Expanded expanded ->
            let ret = obj()
            if expanded.Include.IsSome then ret.["include"] <- expanded.Include.Value
            if expanded.Action.IsSome then ret.["action"] <- makePojoAction expanded.Action.Value
            if expanded.Reg.IsSome then ret.["reg"] <- makePojoReg expanded.Reg.Value
            ret
    let makePojoTokenizer (tokenizer:(string*IMonarchLanguageRule[])[]) : obj =
        let ret = obj()
        Array.iter (fun (k,v) -> ret.[k] <- v |> Array.map makePojoLanguageRule) tokenizer
        ret
    let makePojoLanguageBracket (bracket:IMonarchLanguageBracket) : obj =
        let ret = obj()
        ret.["open"] <- bracket.Open
        ret.["close"] <- bracket.Close
        ret.["token"]  <- bracket.Token
        ret
    let pojo =
        let ret = obj()
        language.Extras |> Array.iter (fun (k,v) -> ret.[k] <- match v with EStrings strings -> strings:>obj | ERegExp regExp -> regExp:>obj)
        ret.["tokenizer"] <- makePojoTokenizer language.Tokenizer
        if language.IgnoreCase.IsSome then ret.["ignoreCase"] <- language.IgnoreCase.Value
        if language.Unicode.IsSome then ret.["unicode"] <- language.Unicode.Value
        if language.DefaultToken.IsSome then ret.["defaultToken"] <- language.DefaultToken.Value
        if language.Brackets.IsSome then ret.["brackets"] <- language.Brackets.Value |> Array.map makePojoLanguageBracket
        if language.TokenPostfix.IsSome then ret.["tokenPostfix"] <- language.TokenPostfix.Value
        ret
    RegisterLanguageJS (id, pojo)
