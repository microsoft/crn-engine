#ifndef __EMSCRIPTEN__
#if defined(_MSC_VER)
//  Microsoft 
// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the SUNDIALSSOLVER_EXPORTS
// symbol defined on the command line. This symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// SUNDIALSSOLVER_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef SUNDIALSSOLVER15_EXPORTS
#define SUNDIALSSOLVER_API __declspec(dllexport)
#else
#define SUNDIALSSOLVER_API __declspec(dllimport)
#endif
#elif defined(_GCC)
//  GCC
#define SUNDIALSSOLVER_API __attribute__((visibility("default")))
#define SUNDIALSSOLVER_API
#else
// unkown compiler, do nothing?
#define SUNDIALSSOLVER_API
#pragma warning Unknown dynamic link import/export semantics.
#endif

typedef double(*FunctionalRates)(double, double*, double*);

typedef double(*SundialsOutput)(double, double*);

extern "C"
{
	SUNDIALSSOLVER_API int fnSundialsSolver(
		const int sN,
		const int rNM,
		const int *stoichM,
		const int *powersM,
		const double* ratesM,
		const int rNF,
		const int *stoichF,
		FunctionalRates functionRates,
		double* concs, //Mutated during algorithm
		const double* timesToReturnParam,
		const int numTimesToReturnParam,
		SundialsOutput sundialsOutput,
		const bool stiff,
		const double abstol,
		const double reltol);

	SUNDIALSSOLVER_API int fnSundialsCMESolver(
		const int stateN,
		const int transN,
		const double* propensities,
		const int* sources,
		const int* targets,
		const double* diagonals,
		double* prob_mass,
		const double* timesToReturnParam,
		const int numTimesToReturnParam,
		SundialsOutput sundialsOutput,
		const bool stiff,
		const double abstol,
		const double reltol);

	//SUNDIALSSOLVER_API int testSundialsStoich();
	//SUNDIALSSOLVER_API int testSundialsBase();
}
#else
typedef double(*SundialsOutput)(double, double*, int);
typedef void(*FunctionalRates)(double, double*, double*, int);

extern "C"
{
	int fnSundialsSolver(
		const int sN,
		const int rNM,
		const int *stoichM,
		const int *powersM,
		const double* ratesM,
		const int rNF,
		const int *stoichF,
		FunctionalRates functionRates,
		double* concs,
		const double* timesToReturnParam,
		const int numTimesToReturnParam,
		SundialsOutput sundialsOutput,
		const bool stiff,
		const double abstol,
		const double reltol,
		double fluxesDynamicM[],
		double fluxesDynamicF[]);
}
#endif