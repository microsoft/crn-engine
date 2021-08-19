module Tests

open Expecto
open canopy
open canopy.classic
open canopy.types
open OpenQA.Selenium
open Microsoft.Research.CRNIntegrationTestLib.Tests

let testCaseExample address = testCaseExample address "#crnCode"

let amSundials = """directive simulation { final=0.15; points=300 }
directive parameters [r = 1.0]
directive simulator sundials
directive deterministic { reltolerance=1e-5 }

| X + Y ->{r} X + B
| Y + X ->{r} Y + B
| X + B ->{r} X + X
| Y + B ->{r} Y + Y

| 30 X
| 20 Y"""

let inferQuick = """directive inference  { burnin = 10; samples = 100; thin = 2 }
directive simulation { final=1.0; points=1000; plots=[B; Y; X] }
directive simulator deterministic
directive deterministic { reltolerance=1e-5 }
directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]
directive data [AM_obs_noised]

| X + Y ->{r} X + B
| Y + X ->{r} Y + B
| X + B ->{r} X + X
| Y + B ->{r} Y + Y

| 30 X
| 20 Y
| 0 B"""

let tests groups mode address =
    let groups = if groups = [||] then [|"default"|] else groups
    printfn "Testing: %s" (System.String.Join(", ",groups))
    testList "UI" [
        if Array.contains "all_examples" groups then yield testList "all_examples" [
            load address
            let options = (element ".c-examples").FindElements(By.CssSelector("option")) |> Seq.map (fun (opt:IWebElement) -> opt.Text) |> Seq.skip 1
            for ex in options do yield testCaseExample address ex
        ]
        if Array.contains "default" groups then yield testList "default" [
            //Direct URL request
            (*yield testCase "Smoke" <| fun _ ->
                let body =
                    Request.createUrl Get Microsoft.Research.CRNEngineWebServer.Program.url
                    |> Request.responseAsString
                    |> run

                Expect.stringContains body "<title>CRN Tool</title>" "It really should"*)
            //Canopy browser automation
            yield testCase "Canopy Smoke" <| fun _ ->
                load address
                ()
            yield testCaseExample address "AM - Stochastic"
            yield testCaseExample address "AM - ODE"
            yield testCaseExample address "AM - CME"
            yield testCaseExample address "AM - LNA"
            yield testCaseExample address "AM - PDE (1d)"
            if mode = Localhost then yield testCaseExample address "AM - Moment closure"
            yield testCase "Sundials" <| fun _ ->
                load address
                let codeEditor = (element ".c-codepad__editor textarea")
                codeEditor.Clear()

                //WINDOWS specific
                codeEditor.SendKeys(OpenQA.Selenium.Keys.Control + "a")
                codeEditor.SendKeys(OpenQA.Selenium.Keys.Delete)

                sleep 1 //Racey

                typeTextInEditor codeEditor amSundials
                //codeEditor.SendKeys(OpenQA.Selenium.Keys.Escape)
            
                sleep 1 //Racey

                simulate false

            yield testCase "Basic inference" <| fun _ ->
                load address
                let codeEditor = (element ".c-codepad__editor textarea")
                codeEditor.Clear()
            
                //WINDOWS specific
                codeEditor.SendKeys(OpenQA.Selenium.Keys.Control + "a")
                codeEditor.SendKeys(OpenQA.Selenium.Keys.Delete)

                sleep 1 //Racey

                typeTextInEditor codeEditor inferQuick
                //codeEditor.SendKeys(OpenQA.Selenium.Keys.Escape)
            
                sleep 1 //Racey

                infer ()
        ]
    ]