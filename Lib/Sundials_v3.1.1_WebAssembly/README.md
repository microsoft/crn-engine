# Building SUNDIALS for Web Assembly

## Building on Linux
Instructions for regenerating SundialsSolver.js and SundialsSolver.wasm on Linux.

Prepare prerequisites:
1. Install CMake 3.10.2
2. Install Emscripten SDK 1.38.4

WebAssembly build procedure:
1. Download and extract Sundials CVODE 3.1.1 sources into some SRC_DIR directory.
2. Create SRC_DIR/biology and put contents of builddef dir there.
3. Copy SundialsSolver.h,SundialsSolver.cpp,stdafx.h SundialsSolver\SundialsSolver15 from SundialsSolver\SundialsSolver15 to SRC_DIR/biology
4. Add "ADD_SUBDIRECTORY(biology)" to SRC_DIR/CMakeLists.txt
5. Activate emscripten environment by executing "source ./emsdk_env.sh".
6. Generate Makefiles with CMAKE (you can use cmake-gui) setting the following:
	(see example of working cmake options at the end of this readme)
	- Produce static lib
	- "emcc" as C compiler
	- "em++" as C++ compiler
	- sundials index type variable as "int32_t"
	- choose some BUILD_DIR
7. Change dir to BUILD_DIR/biology and execute "make".
8. SundialsSolver.js and SundialsSolver.wasm are generated.


Example of CMake options:

```
-DBUILD_SHARED_LIBS:BOOL="0" -DCMAKE_INSTALL_PREFIX:PATH="/usr/local" -DCMAKE_C_COMPILER_RANLIB:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ranlib" -DCMAKE_AR:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ar" -DCMAKE_CXX_FLAGS_DEBUG:STRING="-g" -DCMAKE_LINKER:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/emcc" -DCMAKE_RANLIB:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ranlib" -DCMAKE_CXX_COMPILER:FILEPATH="/home/dmitry/Desktop/emsdk/emscripten/1.38.4/em++" -DCMAKE_CXX_COMPILER_AR:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ar" -DCMAKE_CXX_COMPILER_RANLIB:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ranlib" -DEXAMPLES_INSTALL_PATH:PATH="/home/dmitry/Desktop/instdir/examples" -DCMAKE_C_COMPILER:FILEPATH="/home/dmitry/Desktop/emsdk/emscripten/1.38.4/emcc" -DCMAKE_BUILD_TYPE:STRING="" -DCMAKE_CXX_FLAGS:STRING="" -DCMAKE_COLOR_MAKEFILE:BOOL="1" -DCMAKE_C_COMPILER_AR:FILEPATH="/home/dmitry/Desktop/emsdk/clang/e1.38.4_64bit/llvm-ar" -DSUNDIALS_INDEX_TYPE:STRING="int32_t" 
```


## Building on Windows (experimental)

```
enable_language(CXX)
```

```
SET(EM_JS_OUT_FLAGS "-s RESERVED_FUNCTION_POINTERS=5 -s \"EXPORTED_FUNCTIONS=['_fnSundialsSolver','_fnSundialsCMESolver']\" -s \"EXTRA_EXPORTED_RUNTIME_METHODS=['ccall', 'cwrap', 'addFunction', 'removeFunction', '_free', '_malloc']\" -s MODULARIZE=1 -s \"EXPORT_NAME='CVode'\"")
```

```
C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvars64.bat
```

```
emconfigure cmake -G "NMake Makefiles" -DBUILD_SHARED_LIBS:BOOL="0" -DEXAMPLES_ENABLE_C:BOOL="0" -DSUNDIALS_INDEX_TYPE:STRING="int32_t" E:\dev\sundials-3.1.2
```