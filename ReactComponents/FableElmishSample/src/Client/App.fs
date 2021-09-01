// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module App

open FSharp.Collections
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open ReactIDD
open ReactMSAGL
open ReactCodePad
open Fable.Remoting.Client

// This is a ReactElement that corresponds to ReactIDD.
let inline iddChart (props: IDDProps list) (elems: ReactElement list): ReactElement =
    ofImport "Chart" "reactidd" (keyValueList CaseRules.LowerFirst props) elems

// This is a ReactElement that corresponds to ReactMSAGL.
let inline msaglGraph (props: MSAGLProps list) (elems: ReactElement list): ReactElement =
    ofImport "Graph" "reactmsagl" (keyValueList CaseRules.LowerFirst props) elems

// This is a ReactElement that corresponds to ReactCodePad. You also need to make changes to webpack.config.js (see SharedMonaco.fs).
let inline codePad (props : CodePadProps list) (elems : ReactElement list) : ReactElement =
    ofImport "CodePad" "reactcodepad" (keyValueList CaseRules.LowerFirst props) elems

// Now, I'm going to register the crn language with Monaco, for syntax highlighting. I have two ways of doing this.

// The first is to import a language definition from JavaScript or TypeScript. The disadvantage is that I have to create the definition in TypeScript, not in F#. The advantage is that this way of defining a language is well documented at https://microsoft.github.io/monaco-editor/monarch.html and we can use the languages from our CRN tools directly. This is how it's done:

// First, I'm using Fable's capability to import JavaScript objects to get the JS language definition. Note that here I'm accessing the same language definition file that's actually used in the CRN tools.
[<Import("default", from="../../../../HTML5SharedGUI/HTML5CRN_Lib/Scripts/Components/CrnLang")>]
let private crnLanguage : IMonarchLanguage = jsNative
// Next, I'm calling ReactCodePad.RegisterLanguageJS to register the language. That's it.
RegisterLanguageJS("crn", crnLanguage)

// The alternative way of registering a language is to create an instance of ReactCodePad.IMonarchLanguage. This is an F# equivalent of the Monarch definition format. The advantage is that you can define syntax highlighting without leaving F#. The disadvantage is that this way is not as well documented, although by comparing the example definition in CrnLanguage.fs with the original definition in CrnLang.ts you should be able to figure it out.
RegisterLanguage("crn", CrnLanguage.language)

// Note that I have now registered the same language twice. This is fine, but it's not something you would normally do.

let parsePlotApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder (fun t m -> sprintf "/api/%s/%s" t m)
    |> Remoting.buildProxy<IPlotParseApi>

/// The overall data model driving the view.
type Model =
    { TextData: string
      CrnText: string
      Plots: Plot []
      Axes: Axis []
      Error: string option
      Graph: Graph }

/// The different types of messages in the system.
type Msg =
    | TextDataChanged of string
    | ParseTextData
    | GotPlot of Plot
    | DelPlot of string
    | ErrorMsg of exn

let graph =
    { nodes =
          [| Node.OfIDLabel("Node1", "Node 1")
             Node.OfIDLabel("Node2", "Node 2")
             Node.OfIDLabel("Node3", "Node 3") |]
      edges =
          [| Edge.OfIDSourceTarget("12", "Node1", "Node2")
             Edge.OfIDSourceTarget("13", "Node1", "Node3") |] }

let initialData = "id:myPlot\r\nthickness:2\r\n0 0\r\n1 1"
let crnText = "directive simulation {}\r\nX + Y ->{0.1} Z"

/// The init function is called to start the message pump with an initial view.
let init () =
    { TextData = initialData
      CrnText = crnText
      Plots = [||]
      Axes =
          [| { position = Bottom; kind = Numeric }
             { position = Left; kind = Numeric }
             { position = Top; kind = Numeric }
             { position = Right; kind = Numeric } |]
      Error = None
      Graph = graph },
    Cmd.ofMsg ParseTextData

let parsePlot plotData =
    async {
        let! res = parsePlotApi.parse plotData
        return res
    }

/// The update function knows how to update the model given a message.
let update msg model =
    match model, msg with
    | _, TextDataChanged str -> { model with TextData = str }, Cmd.none
    | { TextData = textData }, ParseTextData ->
        let getcmd res =
            match res with
            | Delete id -> DelPlot id
            | Plot plot -> GotPlot plot

        { model with Error = None }, Cmd.OfAsync.either parsePlot textData getcmd ErrorMsg
    | _, GotPlot response ->
        let plots =
            match FSharp.Collections.Array.tryFindIndex (fun (p: Plot) -> (Helpers.getid p) = (Helpers.getid response))
                      model.Plots with
            | Some idx ->
                FSharp.Collections.Array.mapi (fun i v -> if i = idx then response else model.Plots.[i]) model.Plots
            | _ -> FSharp.Collections.Array.append model.Plots [| response |]

        { model with Plots = plots }, Cmd.none
    | _, DelPlot id ->
        let plots =
            FSharp.Collections.Array.where (fun (p: Plot) -> (Helpers.getid p) <> id) model.Plots

        { model with Plots = plots }, Cmd.none
    | _, ErrorMsg e ->
        { model with
              Plots = [||]
              Error = Some e.Message },
        Cmd.none

/// The view function knows how to render the UI given a model, as well as to dispatch new messages based on user actions.
let view model dispatch =
    let iddcontent =
        match model with
        | { Error = Some error } -> div [] [ str error ]
        | { Plots = plots; Axes = axes } ->
            div [] [
                // This is where I create the IDD chart.
                iddChart [ IDDProps.Width 600.
                           IDDProps.Height 500.
                           Axes(Some(FSharp.Collections.Array.map Helpers.convertAxis axes))
                           Plots(FSharp.Collections.Array.map Helpers.convertPlot plots) ] []
            ]

    let msaglcontent =
        div [ Style [ Border "1px solid black"
                      CSSProp.Width 600
                      CSSProp.Height 300 ] ] [
            msaglGraph [ MSAGLProps.Width 600.
                         MSAGLProps.Height 300.
                         Graph graph ] []
        ]

    let codePadContent =
        div [ Style [ Border "1px solid black"
                      CSSProp.Width 600
                      CSSProp.Height "100%" ] ] [
            codePad [ CodePadProps.Width 600.
                      CodePadProps.Height "100%"
                      CodePadProps.Text model.TextData
                      CodePadProps.OnTextChange(TextDataChanged >> dispatch) ] []
        ]

    let crnCodeContent =
        div [ Style [ Border "1px solid black"
                      CSSProp.Width 600
                      CSSProp.Height 300 ] ] [
            codePad [ CodePadProps.Width 600.
                      CodePadProps.Height "100%"
                      CodePadProps.Text model.CrnText
                      CodePadProps.Language "crn" ] []
        ]

    div [ Style [ Display DisplayOptions.Flex
                  FlexDirection "column" ] ] [
        h1 [ Style [ TextAlign TextAlignOptions.Center ] ] [
            str "Fable components testing area"
        ]

        div [ Style [ Border "1px solid black"
                      Padding 10
                      Margin 10 ] ] [
            h2 [] [ str "MSAGL graph" ]

            msaglcontent
        ]

        div [ Style [ Border "1px solid black"
                      Padding 10
                      Margin 10 ] ] [
            h2 [] [ str "Monaco editor + IDD plot" ]

            div [ Style [ Display DisplayOptions.Flex ] ] [
                div [ Style [ Display DisplayOptions.Flex
                              FlexDirection "column" ] ] [
                    label [] [
                        str "Enter new plots here (enter just id:plotId to delete a plot):"
                    ]

                    div [ Style [ FlexGrow "1"
                                  MarginTop 5
                                  MarginBottom 5 ] ] [
                        codePadContent
                    ]

                    div [] [
                        str "Click"
                        button [ Style [ MarginLeft 5; MarginRight 5 ]
                                 OnClick(fun _ -> dispatch ParseTextData) ] [
                            str "Submit"
                        ]
                        str "to parse the data and add the plot to the IDD view."
                    ]
                ]

                div [ Style [ MarginLeft 10 ] ] [
                    iddcontent
                ]
            ]
        ]

        div [ Style [ Border "1px solid black"
                      Padding 10
                      Margin 10 ] ] [
            h2 [] [ str "Monaco editor + syntax highlighting" ]

            crnCodeContent
        ]
    ]
