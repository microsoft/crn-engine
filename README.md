[![Windows-CLI](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml)
[![Windows-Server](https://github.com/microsoft/CRN/actions/workflows/build-localhost.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-localhost.yml)
[![Windows-HTML](https://github.com/microsoft/CRN/actions/workflows/build-html.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-html.yml)

# Project

Chemical Reaction Networks Engine (CRN-Engine) is an open source repository that enables programming and analysis of (bio)chemical systems. The implementation for several domain-specific programming languages (DSLs) are included, and have been published previously in web tools (Visual DSD, Visual GEC, Visual CRN) and the scientific literature. The languages compile to chemical reaction networks, which is a mathematical object that defines parameterised chemical systems. The included analysis methods on CRNs include stochastic simulation, approximate simulation of stochastic dynamics using moment closure techniques, integration of the chemical master equation, as well as satisfiability analysis for stable systems. Finally, the parameters of CRNs can be inferred from observation data, using Markov chain Monte Carlo (MCMC).

## Getting Started

The main tools are located in the folders \CRNEngine, \ClassicGEC and \ClassicDSD. For each tool, there are solutions for a command-line interface (CLI), or with a visual front-end, either deployed as server-client (run-on-localhost) or fully HTML. To build these solutions, you will need to configure Visual Studio with the necessary add-ins, and also install a few more dependencies.

The instructions for building on Windows are as follows:

### 1. Install Visual Studio 2019

The community edition provides access to all of the necessary build components. After opening the Visual Studio installer, you should ensure that the following **Workloads** are selected:
- .NET desktop development
- Desktop development with C++
- Azure development

Additionally, you should ensure that the following **Individual Components** are selected:
- .NET Framework 4.7.2 SDK
- .NET Framework 4.7.2 targeting pack
- MSVC v142 tools (latest version), and the corresponding libs for Spectre mitigation 
- F# desktop language support
- F# language support
- F# language support for web projects
- Windows 10 SDK (10.0.17763.0)

### 2. Install .NET SDKs

The .NET Core SDKs required are [2.1](https://dotnet.microsoft.com/download/dotnet/2.1) and [3.1](https://dotnet.microsoft.com/download/dotnet/3.1). Currently, the build pipeline is working with 2.1.818 and 3.1.413.

### 3. Install dotnet tools

dotnet tools can be installed/restored by navigating to your repository directory and executing:

`dotnet tool restore`

This will install `paket` (package manager) and `fake` (library for simplifying .NET builds), which are used by the provided build pipeline.

### 4. Restore dependencies

You can restore the dependencies associated with your favourite solution by simplying calling:

`dotnet restore [path/to/solution]`

### 5. Build solution

In Visual Studio, you can select Build Solution from the BUILD menu (or issue the standard shortcut Ctrl+Alt+B). If any projects fail, try building again before attempting to diagnose the problem.

If you'd prefer not to open Visual Studio, you can run `msbuild` on the command-line. Simply add the directory containing msbuild (e.g. `C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin`) to your `PATH` environment variable. Then you can build as:

`msbuild [path/to/solution]`

Various switches can be added to `msbuild`, such as changing configuration (`/p:Configuration=Release`) and/or restoring dependencies (`/restore`).

### 6. Running examples

Each program is run in different ways. 
- **CLI tools**. After building, an .exe file will be in the corresponding `bin/x64/[Configuration]/netcoreapp3.1` folder. Each CLI tool has a different set of command-line arguments. Call `CliCRN.exe --help` for details. The basic idea is that a model file (`.crn`, `.gec` or `.dsd`) is supplied as the last argument, and different actions can be called (e.g. `--simulate`, `--infer [DATA-FOLDER]`).
- **Server tools**. After building, an .exe file will be in the corresponding `bin/x64/[Configuration]/netcoreapp3.1` folder. Simply run the executable to bring up the GUI.
- **HTML5-only tools**. In each of the HTML5 project folders (`CRNEngine\HTML5CRN`, `ClassicDSD\ClassicDSDHTML5`, `ClassicGEC\ClassicGECHTML5`), the `build.fsx` file describes the build targets, which includes a `run` action. This can be called as `dotnet fake build --target run` (this shortcut is in the `run.cmd` file). This will eventually open your default browser running the HTML5 GUI.


## Contributions

There have been several contributors to this codebase, prior to its migration to this location on GitHub. Significant contributions have come from:
- Andrew Phillips ([ph1ll1ps](https://github.com/ph1ll1ps))
- Neil Dalchau ([ndalchau](https://github.com/ndalchau))
- Colin Gravill ([cgravill](https://github.com/cgravill))
- Filippo Polo ([FilippoPolo](https://github.com/FilippoPolo))
- Prashant Vaidyanathan ([PrashantVaidyanathan](https://github.com/PrashantVaidyanathan))
- Carlo Spaccasassi ([CSpaccasassi](https://github.com/CSpaccasassi))
- Boyan Yordanov ([byoyo](https://github.com/byoyo))
- Rasmus Petersen ([yoff](https://github.com/yoff))
- Matthew Lakin ([matthewlakin](https://github.com/matthewlakin))
- Michael Pedersen ([mdpedersen](https://github.com/mdpedersen))


## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft 
trademarks or logos is subject to and must follow 
[Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general).
Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship.
Any use of third-party trademarks or logos are subject to those third-party's policies.
