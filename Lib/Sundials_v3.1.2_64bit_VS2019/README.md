# Building SUNDIALS

We build Sundials from source: https://computation.llnl.gov/projects/sundials 

Last completed with:
- Visual Studio 16.4.2 64Bit
- Sundials 3.1.2

There are Windows build instructions in the download, there are issues and changes needed.

- Target * Win64 as the generator.
- It's cleaner if you disable BUILD_SHARED_LIBS
- On the CMake you currently need to disable building examples
- The install target currently fails, just don't do that.
- You need to enable spectre mitigations. Doing this via CMake seems like a good idea but adding the /QSpectre flag wasn't enough. A direct way is to add the flag just after everywhere (VS Code replace helps). E.g.
    Replace 
    ```
    <PlatformToolset>v142</PlatformToolset>
    ``` 
    with
    ```
    <PlatformToolset>v142</PlatformToolset>
    <SpectreMitigation>Spectre</SpectreMitigation>
    ```

We statically link the internals of Sundials. Also statically link CRT for easier deployment

- `CMAKE_C_FLAGS_RELWITHDEBINFO`
- `/MD -> /MT`

We build for release with debugging info. You need to manually extract pdbs due to Sundials build issues.

Outputs should be placed in e.g. \Lib\Sundials_v3.1.2_64bit_VS2019

Extract the include files from src & build
Extract the individual lib and pdbs from each build directory
Fix the config file so it's correct for import, see previous fixes.