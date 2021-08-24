// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNIntegrationTestLib.BrowserSetup

open System
open canopy
open Expecto
open canopy.types
open canopy.classic

let driverDirectory =
    //https://github.com/Microsoft/azure-pipelines-image-generation/blob/master/images/win/Vs2019-Server2019-Readme.md
    let possibleChromewebddriverPath = System.Environment.GetEnvironmentVariable("CHROMEWEBDRIVER")
    if String.IsNullOrEmpty possibleChromewebddriverPath then
        System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    else
        possibleChromewebddriverPath

//https://sites.google.com/a/chromium.org/chromedriver/help/chrome-doesn-t-start
let configureCanopy timeout =
    printfn "Configuring canopy with %f secs timeout..." timeout
    canopy.configuration.edgeDir <- driverDirectory
    canopy.configuration.chromeDir <- driverDirectory
    canopy.configuration.chromiumDir <- driverDirectory

    canopy.configuration.compareTimeout <- timeout
    canopy.configuration.elementTimeout <- timeout
    canopy.configuration.pageTimeout <- timeout
    
    //start BrowserStartMode.FirefoxHeadless //Firefox headless threw exceptions under the VSTS runner, to be investigated
    //start BrowserStartMode.EdgeBETA //https://wpdev.uservoice.com/forums/257854-microsoft-edge-developer/suggestions/6545168-headless-browser-for-testing

    //Have to install Chrome on the target machine
    //https://www.google.com/chrome/browser/desktop/index.html?standalone=1
    let options = new OpenQA.Selenium.Chrome.ChromeOptions()
#if !DEBUG
    options.AddArgument("--headless");
#endif
    start (ChromeWithOptionsAndTimeSpan (options,TimeSpan.FromMinutes(2.)))