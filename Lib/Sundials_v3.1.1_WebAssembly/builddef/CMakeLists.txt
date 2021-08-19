SET(CVODE_LIB sundials_cvode_static)
SET(NVECS_LIB sundials_nvecserial_static)

# Set-up linker flags and link libraries
SET(SUNDIALS_LIBS ${CVODE_LIB} ${NVECS_LIB} ${EXTRA_LINK_LIBS})

SET(EM_JS_OUT_FLAGS "-s RESERVED_FUNCTION_POINTERS=5 -s EXPORTED_FUNCTIONS='[\"_fnSundialsSolver\",\"_fnSundialsCMESolver\"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='[\"ccall\", \"cwrap\", \"addFunction\", \"removeFunction\", \"_free\", \"_malloc\"]' -s MODULARIZE=1 -s \"EXPORT_NAME='CVode'\"")

SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${EM_JS_OUT_FLAGS}" )

ADD_EXECUTABLE(SundialsSolver SundialsSolver.cpp)
SET_TARGET_PROPERTIES(SundialsSolver PROPERTIES SUFFIX ".js")
# libraries to link against
TARGET_LINK_LIBRARIES(SundialsSolver ${SUNDIALS_LIBS})
