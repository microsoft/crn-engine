[![Windows-CLI](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml)
[![Windows-Server](https://github.com/microsoft/CRN/actions/workflows/build-localhost.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-localhost.yml)
[![Windows-HTML](https://github.com/microsoft/CRN/actions/workflows/build-html.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-html.yml)

# Project

Chemical Reaction Networks Engine (CRN-Engine) is an open source repository that enables programming and analysis of (bio)chemical systems. The implementation for several domain-specific programming languages (DSLs) are included, and have been published previously in web tools (Visual DSD, Visual GEC, Visual CRN) and the scientific literature. The languages compile to chemical reaction networks, which is a mathematical object that defines parameterised chemical systems. The included analysis methods on CRNs include stochastic simulation, approximate simulation of stochastic dynamics using moment closure techniques, integration of the chemical master equation, as well as satisfiability analysis for stable systems. Finally, the parameters of CRNs can be inferred from observation data, using Markov chain Monte Carlo (MCMC).

## Getting Started

The main tools are located in the folders \CRNEngine, \ClassicGEC and \ClassicDSD. For each tool, there are solutions for a command-line interface (CLI), or with a visual front-end, either deployed as server-client (run-on-localhost) or fully HTML. To build these solutions, you will need to configure Visual Studio with the necessary add-ins, and also install a few more dependencies.

The instructions for building on Windows are as follows:

1. **Install Visual Studio 2019**

    The components required are:
    - Workloads tab:
        - .NET desktop development
        - Desktop development with C++
        - Azure development
    - Individual Components tab, ".NET" section:
        - .NET Framework 4.7.2 SDK
        - .NET Framework 4.7.2 targeting pack
    - Individual Components tab, "Compilers, build tools, and runtimes" section:
        - MSVC (XYZ) - VS 2019; Libs for Spectre (x86 and x64) [where XYZ is the highest version corresponding to the VC++ 2019 version (XYZ) latest v142 tools, which might have been selected already]
    - Individual Components tab, "Development activities" section:
        - F# desktop language support
        - F# language support
        - F# language support for web projects
    - Individual Components tab, "SDKs, libraries, and frameworks" section:
        - Windows 10 SDK (10.0.17763.0)

2. **Install .NET Core SDKs**. These are available [here](https://dotnet.microsoft.com/download/visual-studio-sdks). The required versions are 2.1 and 3.1.

3. **Install NodeJS**. N.B. This may involve restarting your computer several times.

4. **Install Yarn**. The instructions (for Windows) are [here](https://classic.yarnpkg.com/en/docs/install/#windows-stable).


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
