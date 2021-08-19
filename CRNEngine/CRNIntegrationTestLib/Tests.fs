module Microsoft.Research.CRNIntegrationTestLib.Tests

open System
open Expecto
open canopy
open canopy.classic
open canopy.types
open OpenQA.Selenium

// FP: there is a problem with Monaco where if you type a row and hit enter really, really fast, the enter may get caught by an autocompletion widget that appeared at some point while you were typing, and should have been discarded by the time you got to the end of the row, but in reality was still around. When this happens, the autocompletion will trigger and cause an extra token to get inserted at the point where autocompletion was started. This can be avoided by having a small pause before hitting enter, enough to let Monaco dispose the autocompletion widget. Also, note that I'm adding an extra space at the end of each line; this way, I'll get rid of any autocomplete box that was opened by the last token.
let typeTextInEditor (codeEditor:OpenQA.Selenium.IWebElement) (text:string) = text.Split('\r','\n') |> Array.where (fun row -> row <> "") |> Array.map (fun row -> "\r\n" + row + " ") |> Array.iter (fun row -> codeEditor.SendKeys row; sleep 1)

type Mode = Worker | Localhost

let path = @"."

let ready () =
    let cover = someElement "#app-cover"
    match cover with
    | None -> false
    | Some cover -> not cover.Displayed

let isSuccessfulCompletion (str:System.String) =
    // Being unable to run something because the current execution mode prevents it still counts as a success.
    str.Contains("completed successfully") || str.Contains("cannot be invoked this way") || str.Contains("this model is designed for parsing only")

let simulateDone () =
    let simulateButton = element "#simulateButton"
    let status = element "#execution-status" 
    (isSuccessfulCompletion status.Text) && simulateButton.Enabled

let hasProbabilities () =
    let options = (element ".t-probabilities_species").FindElements(By.CssSelector("option")) |> Seq.length
    options > 0

let inferDone () =
    let inferButton = element "#inferButton"
    let status = element "#execution-status" 
    (isSuccessfulCompletion status.Text) && inferButton.Enabled

let UIErrors () =
    let errorList = element ".c-codepad__error-list"
    if errorList.Text = "" then None else Some errorList.Text

let load address =
    sleep 1 //Racey

    url address

    sleep 1 //Racey

    waitFor ready

let selectExample codeEditor example =
    sleep 1 //Racey

    (codeEditor + " .c-examples") << example

let simulate probabilities =
    sleep 1 //Racey

    click "#simulateButton"

    sleep 1 //Racey

    waitFor simulateDone

    sleep 1 //Racey

    if probabilities then waitFor hasProbabilities

    sleep 1 //Racey

    match UIErrors() with
    | None -> ()
    | Some text -> failwith text

let infer () =
    sleep 1 //Racey

    click "#inferButton"

    sleep 1 //Racey

    waitFor inferDone

    sleep 1 //Racey

    match UIErrors() with
    | None -> ()
    | Some text -> failwith text

let takeScreenshot name =
    sleep 1 //Racey

    screenshot path name |> ignore

    System.IO.File.Delete(path + "/" + name + ".png")
    System.IO.File.Move(path + "/" + name + ".jpg", path + "/" + name + ".png") //Weirdly it adds a .jpg

let testCaseExample address codeEditor name =
    testCase (sprintf "Example '%s' runs to completion" name) <| fun _ ->
        load address
        selectExample codeEditor name
        let code = element codeEditor
        let content = code.Text
        let probabilities = content.Contains "simulator cme"
        let inference = content.Contains "directive inference"
        if inference then infer() else simulate probabilities
