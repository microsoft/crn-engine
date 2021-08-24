[![Windows-CLI](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-cli.yml)
[![Windows-Server](https://github.com/microsoft/CRN/actions/workflows/build-server.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-server.yml)
[![Windows-HTML](https://github.com/microsoft/CRN/actions/workflows/build-html.yml/badge.svg)](https://github.com/microsoft/CRN/actions/workflows/build-html.yml)

# Project

Chemical Reaction Networks Engine (CRN-Engine) is an open source repository that enables programming and analysis of (bio)chemical systems. The implementation for several domain-specific programming languages (DSLs) are included, and have been published previously in web tools (Visual DSD, Visual GEC, Visual CRN) and the scientific literature. The languages compile to chemical reaction networks, which is a mathematical object that defines parameterised chemical systems. The included analysis methods on CRNs include stochastic simulation, approximate simulation of stochastic dynamics using moment closure techniques, integration of the chemical master equation, as well as satisfiability analysis for stable systems. Finally, the parameters of CRNs can be inferred from observation data, using Markov chain Monte Carlo (MCMC).

## Getting Started

For building on Windows, the following are required:

- Visual Studio 2019
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
- .NET Core SDK 3.0.* 
- .NET Core SDK 3.1.* 
- NodeJS  (this may involve restarting your computer several times)
- Yarn 
- C++ CRT  (C Run Time Library)

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
