// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Tests

open Expecto
open canopy
open canopy.classic
open OpenQA.Selenium
open Microsoft.Research.CRNIntegrationTestLib.Tests

let testCaseExample address = testCaseExample address "#sg-editor"

let tests groups mode address =
    let groups = if groups = [||] then [|"default"|] else groups
    printfn "Testing: %s" (System.String.Join(", ",groups))
    testList "UI" [
        if Array.contains "all_examples" groups then yield testList "all_examples" [
            load address
            let options = (element "#dsdCode").FindElement(By.CssSelector ".c-examples").FindElements(By.CssSelector "option") |> Seq.map (fun (opt:IWebElement) -> opt.Text) |> Seq.skip 1
            for ex in options do
                // This kludge is to avoid failing tests for Consensus Parameterized, that doesn't provide the necessary data files. Longer term, we need to decide what to do with this example.
                // I'm also excluding Localized HCR, which runs into a stack overflow in release mode. That particular issue sounds extremely hard to debug.
                if ex <> "Consensus Parameterized" && ex <> "Localized HCR" then
                    yield testCaseExample address ex
        ]
        if Array.contains "default" groups then yield testList "default" [
            //Direct URL request
            (*yield testCase "Smoke" <| fun _ ->
                let body =
                    Request.createUrl Get Microsoft.Research.ClassicDSDServer.Program.url
                    |> Request.responseAsString
                    |> run

                Expect.stringContains body "<title>Classic DSD Tool</title>" "It really should"*)
            //Canopy browser automation
            yield testCase "Canopy Smoke" <| fun _ ->
                load address
                ()

            yield testCase "Inference Smoke" <| fun _ ->
                load address

                selectExample "#sg-editor" "Join - Inference"

                infer()

                ()
        ]
    ]