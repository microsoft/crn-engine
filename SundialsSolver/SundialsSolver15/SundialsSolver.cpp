// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// SundialsSolver.cpp : Defines the exported functions for the DLL application.

#include "stdafx.h"

#include "SundialsSolver.h"
#include <math.h>
#include <stdlib.h>
#include <memory>
#include <utility>
#include <functional>

#ifdef __EMSCRIPTEN__
//setting SUNDIALSSOLVER_API to empty token (set in SundialsSolver.h)
#define SUNDIALSSOLVER_API
#endif

//#include <arkode/arkode.h>
//#include <arkode/arkode_dense.h>

#include <cvode/cvode.h>
#include <cvode/cvode_direct.h>
#include <nvector/nvector_serial.h>
#include <sunlinsol/sunlinsol_dense.h>

#define Ith(v,i) NV_Ith_S(v,i)     // Ith numbers components 0..NEQ-1
#define IJth(A,i,j) DENSE_ELEM(A,i,j) // IJth numbers rows,cols 0..NEQ-1

#include <vector>
#include <array>

#ifdef NIX
#define ALLOCA alloca
#else
#define ALLOCA _alloca // CG: Not using _malloca as we want it to fail if it doesn't fit on the stack
#endif

struct UserData {
	std::vector<std::vector<int> > stoichM;
	std::vector<std::vector<int> > powersM;
	std::vector<std::vector<int> > stoichF;
	std::vector<double> ratesM;
	FunctionalRates functionalRates;
	int sN, rNM, rNF;
#ifdef __EMSCRIPTEN__
	double *fluxesDynamicM, *fluxesDynamicF;
#endif
};

struct CMEUserData {
	std::vector<int> targets;
	std::vector<int> sources;
	std::vector<double> propensities;
	std::vector<double> diagonals;
	int stateN, totaltransN;
};

struct Scoped_N_Vector {
#ifndef __EMSCRIPTEN__
	Scoped_N_Vector(long vec_length) { v = N_VNew_Serial(vec_length); }
#else
	Scoped_N_Vector(sunindextype vec_length) { v = N_VNew_Serial(vec_length); }
#endif
	~Scoped_N_Vector() {
		N_VDestroy_Serial(v);
	}
	N_Vector v;
};

struct Scoped_CVode {
	Scoped_CVode(int lmm, int iter) { cvode = CVodeCreate(lmm, iter); }
	~Scoped_CVode() {
		CVodeFree(&cvode);
	}
	void* cvode;
};

struct Scoped_SUNDenseMatrix {
	Scoped_SUNDenseMatrix(sunindextype M, sunindextype N) { a = SUNDenseMatrix(M, N); }
	~Scoped_SUNDenseMatrix() {
		SUNMatDestroy(a);
	}
	SUNMatrix a;
};

extern "C"
{

	int f(realtype t, N_Vector y, N_Vector ydot, void *user_data);
	int fAM(realtype t, N_Vector y, N_Vector ydot, void *user_data);
	int fCME(realtype t, N_Vector y, N_Vector ydot, void *user_data);

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
		const double reltol)
	{
		//printf("entered fnSundialsSolver");

		// Variables for the ODE solver
		CMEUserData paras;
		// Here give it the stuff it needs to compute the answer
		paras.stateN = stateN;
		paras.totaltransN = transN;

		//// Write the input arrays into a matrix structure (Mass action rates)
		for (auto i = 0; i < transN; i++)
		{
			paras.targets.push_back(targets[i]);
			paras.sources.push_back(sources[i]);
			paras.propensities.push_back(propensities[i]);
		}

		// Create initial conditions vector y, inital probability mass
		auto scoped_y = Scoped_N_Vector(stateN);
		N_Vector y = scoped_y.v;
		
		for (int i = 0; i < stateN; i++)
		{
			Ith(y, i) = prob_mass[i];
			paras.diagonals.push_back(diagonals[i]);
			//printf("\nx0[%d] = %f", i, Ith(y,i));
		}

		//// Set up the ODE problem
		realtype t0 = timesToReturnParam[0];
		int sundialsResultFlag, lmm, iter;
		if (stiff)
		{
			lmm = CV_BDF;
			iter = CV_NEWTON;
		}
		else
		{
			lmm = CV_ADAMS;
			iter = CV_FUNCTIONAL;
		}

		auto cvode = Scoped_CVode(lmm, iter);
		auto cvode_mem = cvode.cvode;
		sundialsResultFlag = CVodeInit(cvode_mem, fCME, t0, y);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSStolerances(cvode_mem, reltol, abstol);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSetUserData(cvode_mem, &paras);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		
#ifdef __EMSCRIPTEN__
		SUNMatrix A;
		SUNLinearSolver LS;
#else
		//We need to map the C style API to something exception safe. This is the
		//cleanest way I've found to support custom deleters and conditional initialization
		std::unique_ptr<_generic_SUNMatrix, decltype(&SUNMatDestroy)> A(nullptr, &SUNMatDestroy);
		std::unique_ptr<_generic_SUNLinearSolver, decltype(&SUNLinSolFree)> LS(nullptr, &SUNLinSolFree);
#endif

		if (iter == CV_NEWTON)
		{
			//sundialsResultFlag = CVDense(cvode_mem, sN);
#ifdef __EMSCRIPTEN__
			A = SUNDenseMatrix((sunindextype)stateN, (sunindextype)stateN);
			LS = SUNDenseLinearSolver(y, A);

			sundialsResultFlag = CVDlsSetLinearSolver(cvode_mem, LS, A);
#else
			A = { SUNDenseMatrix(stateN, stateN), &SUNMatDestroy };
			LS = { SUNDenseLinearSolver(y, A.get()), &SUNLinSolFree };

			sundialsResultFlag = CVDlsSetLinearSolver(cvode_mem, LS.get(), A.get());
#endif
			if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		}

		sundialsResultFlag = CVodeSetMaxNumSteps(cvode_mem, 50000);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSetMaxHnilWarns(cvode_mem, 0);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }

		///*void * arkode_mem = ARKodeCreate();
		//flag = ARKodeInit(arkode_mem, NULL, f, t0, y);
		//flag = ARKodeSStolerances(arkode_mem, reltol, abstol);
		//flag = ARKodeSetUserData(arkode_mem, &paras);
		//flag = ARKDense(arkode_mem, sN);
		//flag = ARKodeSetMaxNumSteps(arkode_mem, 50000);
		//flag = ARKodeSetMaxHnilWarns(arkode_mem, 0);*/

		auto y_data = NV_DATA_S(y);

		//Write out initial conditions
#ifdef __EMSCRIPTEN__
		sundialsOutput(t0, y_data, stateN);
#else
		sundialsOutput(t0, y_data);
#endif

		realtype t = t0;
		realtype t_previous = t;

		for (auto i = 1; i < numTimesToReturnParam; i++) {
			auto new_t = timesToReturnParam[i];
			if (new_t < t_previous) {
				printf("Error: requested times must be non-decreasing. i=%i, t=%f, t_previous=%f\n", i, new_t, t_previous);
				return -1;
			}
			t_previous = new_t;
		}

		// Integrate for some time, calling the output function the solution at each point
		for (int i = 1; i < numTimesToReturnParam; i++)
		{
			auto new_t = timesToReturnParam[i];
			if (new_t > t) // Should we just assume that new_t = t if this is false? A non-decreasing check occurs above.
			{
				sundialsResultFlag = CVode(cvode_mem, timesToReturnParam[i], y, &t, CV_NORMAL);
				if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
				//flag = ARKode(arkode_mem, timesToReturnParam[i], y, &t, ARK_NORMAL);
			}
#ifdef __EMSCRIPTEN__
			sundialsOutput(t, y_data, stateN);
#else
			sundialsOutput(t, y_data);
#endif
		}
		//
		for (int i = 0; i < stateN; i++)
			prob_mass[i] = y_data[i];

		return sundialsResultFlag;
	}

	// y is the input set of states
	// ydot is the output vector of changes to the states. 
	int fCME(realtype t, N_Vector y, N_Vector ydot, void *user_data)
	{
		const CMEUserData* data = static_cast<CMEUserData*>(user_data);
		const int stateN = data->stateN;
		const int totaltransN = data->totaltransN;
		const std::vector<int>& targets = data->targets;
		const std::vector<int>& sources = data->sources;
		const std::vector<double>& propensities = data->propensities;
		const std::vector<double>& diagonals = data->diagonals;

		auto y_data = NV_DATA_S(y);

		std::vector<double> accumulator(stateN, 0.0);

		for (int i = 0; i < totaltransN; i++)
		{
			accumulator[targets[i]] += y_data[sources[i]] * propensities[i];
		}

		for (int s = 0; s < stateN; s++)
		{
			Ith(ydot, s) = accumulator[s] - (y_data[s] * diagonals[s]);
		}

		return(0);
	}

	SUNDIALSSOLVER_API int fnSundialsSolver(
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
		const double reltol
#ifdef __EMSCRIPTEN__
		,double fluxesDynamicM[],
		double fluxesDynamicF[]
#endif
		)
	{
		//printf("entered fnSundialsSolver");

		// Variables for the ODE solver
		UserData paras;
		paras.rNM = rNM;
		paras.rNF = rNF;
		paras.sN = sN;
		paras.functionalRates = functionRates;
#ifdef __EMSCRIPTEN__
		paras.fluxesDynamicM = fluxesDynamicM;
		paras.fluxesDynamicF = fluxesDynamicF;
#endif

		// Write the input arrays into a matrix structure (Mass action rates)
		for (auto i = 0; i < rNM; i++)
		{
			paras.ratesM.push_back(ratesM[i]);

			paras.stoichM.push_back(std::vector<int>());
			paras.powersM.push_back(std::vector<int>());
			for (auto j = 0; j < sN; j++)
			{
				paras.stoichM[i].push_back(stoichM[i*sN + j]);
				paras.powersM[i].push_back(powersM[i*sN + j]);
			}
		}

		// Write the input arrays into a matrix structure (Functional rates)
		for (auto i = 0; i < rNF; i++)
		{
			paras.stoichF.push_back(std::vector<int>());
			for (auto j = 0; j < sN; j++)
			{
				paras.stoichF[i].push_back(stoichF[i*sN + j]);
			}
		}

		// Create serial vector of length NEQ for I.C's, and push back into the solution
		auto scoped_y = Scoped_N_Vector(sN);
		N_Vector y = scoped_y.v;

		for (int i = 0; i < sN; i++)
		{
			Ith(y, i) = concs[i];
			//printf("\nx0[%d] = %f", i, Ith(y,i));
		}

		// Set up the ODE problem
		realtype t0 = timesToReturnParam[0];
		int sundialsResultFlag, lmm, iter;
		if (stiff)
		{
			lmm = CV_BDF;
			iter = CV_NEWTON;
		}
		else
		{
			lmm = CV_ADAMS;
			iter = CV_FUNCTIONAL;
		}

		auto cvode = Scoped_CVode(lmm, iter);
		auto cvode_mem = cvode.cvode;
		sundialsResultFlag = CVodeInit(cvode_mem, f, t0, y);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSStolerances(cvode_mem, reltol, abstol);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSetUserData(cvode_mem, &paras);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }

#ifdef __EMSCRIPTEN__
		SUNMatrix A;
		SUNLinearSolver LS;
#else
		//We need to map the C style API to something exception safe. This is the
		//cleanest way I've found to support custom deleters and conditional initialisation
		std::unique_ptr<_generic_SUNMatrix, decltype(&SUNMatDestroy)> A(nullptr, &SUNMatDestroy);
		std::unique_ptr<_generic_SUNLinearSolver, decltype(&SUNLinSolFree)> LS(nullptr, &SUNLinSolFree);
#endif

		if (iter == CV_NEWTON)
		{
			//sundialsResultFlag = CVDense(cvode_mem, sN);
#ifdef __EMSCRIPTEN__
			A = SUNDenseMatrix((sunindextype)sN, (sunindextype)sN);
			LS = SUNDenseLinearSolver(y, A);

			sundialsResultFlag = CVDlsSetLinearSolver(cvode_mem, LS, A);
#else
			A = { SUNDenseMatrix(sN, sN), &SUNMatDestroy };
			LS = { SUNDenseLinearSolver(y, A.get()), &SUNLinSolFree};

			sundialsResultFlag = CVDlsSetLinearSolver(cvode_mem, LS.get(), A.get());
#endif
			if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		}
		sundialsResultFlag = CVodeSetMaxNumSteps(cvode_mem, 500000);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
		sundialsResultFlag = CVodeSetMaxHnilWarns(cvode_mem, 0);
		if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }

		/*void * arkode_mem = ARKodeCreate();
		flag = ARKodeInit(arkode_mem, NULL, f, t0, y);
		flag = ARKodeSStolerances(arkode_mem, reltol, abstol);
		flag = ARKodeSetUserData(arkode_mem, &paras);
		flag = ARKDense(arkode_mem, sN);
		flag = ARKodeSetMaxNumSteps(arkode_mem, 50000);
		flag = ARKodeSetMaxHnilWarns(arkode_mem, 0);*/

		auto y_data = NV_DATA_S(y);

     	realtype t = t0;
		realtype t_previous = t;

		for (auto i = 1; i < numTimesToReturnParam; i++) {
			auto new_t = timesToReturnParam[i];
			if (new_t < t_previous) {
				printf("Error: requested times must be non-decreasing. i=%i, t=%f, t_previous=%f\n", i, new_t, t_previous);
				return -1;
			}
			t_previous = new_t;
		}

		//Write out initial conditions
#ifdef __EMSCRIPTEN__
		sundialsOutput(t0, y_data, sN);
#else
		sundialsOutput(t0, y_data);
#endif

		// Integrate for some time, calling the output function the solution at each point
		for (int i = 1; i < numTimesToReturnParam; i++)
		{
			auto new_t = timesToReturnParam[i];
			if (new_t > t) // Should we just assume that new_t = t if this is false? A non-decreasing check occurs above.
			{
				sundialsResultFlag = CVode(cvode_mem, new_t, y, &t, CV_NORMAL);

				/*printf("\nTime: %f\n[ %1.3g", new_t, Ith(y,0));
				for (int j = 1; j < sN; j++)
						printf(", %1.3g", Ith(y,j));
				printf(" ]\n");*/

				if (CV_SUCCESS != sundialsResultFlag) { return sundialsResultFlag; }
				//flag = ARKode(arkode_mem, timesToReturnParam[i], y, &t, ARK_NORMAL);
			}
#ifdef __EMSCRIPTEN__
			sundialsOutput(t, y_data, sN);
#else
			sundialsOutput(t, y_data);
#endif
		}

		for (int i = 0; i < sN; i++)
			concs[i] = y_data[i];

		return sundialsResultFlag;
	}

	int f(realtype t, N_Vector y, N_Vector ydot, void *user_data)
	{
		const UserData* data = static_cast<UserData*>(user_data);
		const int sN = data->sN;
		const int rNM = data->rNM;
		const int rNF = data->rNF;
		const std::vector<std::vector<int> >& stoichM = data->stoichM;
		const std::vector<std::vector<int> >& powersM = data->powersM;
		const std::vector<double>& ratesM = data->ratesM;
		const std::vector<std::vector<int> >& stoichF = data->stoichF;

		const FunctionalRates functionalRates = data->functionalRates;

#ifdef __EMSCRIPTEN__
		double* fluxesDynamicM = data->fluxesDynamicM;
		double* fluxesDynamicF = data->fluxesDynamicF;
#else
		//Dynamic stack allocate version to avoid heap allocation/access cost
		auto fluxesDynamicM = static_cast<double*>(ALLOCA(rNM*sizeof(double)));
		auto fluxesDynamicF = static_cast<double*>(ALLOCA(rNF*sizeof(double)));

		//Should this return a non-zero code? Would that stop the process?
		if (fluxesDynamicM == NULL || fluxesDynamicF == NULL) {
			printf("Error: failed to allocate fluxes");
			return 0;
		}

#endif

		//Use the vector version if you need to do debugging.
		//auto fluxesDynamicM = std::vector<double>(rNM, 0.0);
		//auto fluxesDynamicF = std::vector<double>(rNF, 0.0);

		auto y_data = NV_DATA_S(y);
		auto ydot_data = NV_DATA_S(ydot);

		// Calculate fluxes from the reaction propensities (for the Mass Action reactions)
		for (int i = 0; i < rNM; i++)
		{
			auto acc = ratesM[i];
			const auto& powersM_i = powersM[i];
			for (int j = 0; j < sN; j++)
			{
				const auto power = powersM_i[j];
				if (power > 0)
				{
					if (power > 1)
						acc *= pow(y_data[j], power);
					else
						acc *= y_data[j];
				}
			}
			fluxesDynamicM[i] = acc;
		}

		//Functional rates are done back in the .net code for compatiblity, and to avoid re-implementing the expression system here.
		//Keep an eye on performance implications - Colin
#ifdef __EMSCRIPTEN__
		functionalRates(t, fluxesDynamicF, y_data, sN);
#else
		functionalRates(t, &fluxesDynamicF[0], y_data);
#endif

		// Now do stoich*fluxes
		//printf("\n    ****    \nt = %f", t);
		for (int j = 0; j < sN; j++)
		{
			double acc = 0.0;
			for (int i = 0; i < rNM; i++)
				acc += stoichM[i][j] * fluxesDynamicM[i];
			for (int i = 0; i < rNF; i++)
				acc += stoichF[i][j] * fluxesDynamicF[i];
			Ith(ydot, j) = acc;
			//printf("\ny[%d] = %f", j, Ith(y, j));
			//printf("\nydot[%d] = %f", j, Ith(ydot,j));
		}

		return(0);
	}



	int fAM(realtype t, N_Vector y, N_Vector ydot, void *user_data)
	{
		auto yarr = NV_DATA_S(y);

		double B = yarr[0];
		double X = yarr[1];
		double Y = yarr[2];

		Ith(ydot, 0) = 2.0*X*Y - B*(X + Y);
		Ith(ydot, 1) = B*X - X*Y;
		Ith(ydot, 2) = B*Y - X*Y;

		return(0);
	}

}
