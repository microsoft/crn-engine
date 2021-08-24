// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

class WsEmGlue {
    constructor(public Module: any) { }
    /*
            sN:int,
            rNM:int,
            stoichM:int[], //len = rNM x sN
            powersM:int[], //len = rNM x sN
            ratesM:double[],
            rNF:int,
            stoichF:int[], //len = rNF x sN
            functionalRates: FunctionalRates,
            concs:double[], //len = sN
            t:double[], //len = tN
            tN:int,
            sundialsOutput:SundialsOutput,
            stiff:bool,
            abstol:double,
            reltol:double,
            fluxesDynamicM: double[], //len = rNM
            fluxesDynamicF: double[] // len = rNF
            ) = 0
     */

    //var prepAcc = 0.0;
    //var runtimeAcc = 0.0;
    //var cleanAcc = 0.0;
    //var callbackAcc = 0.0
    //var f_read_prep_CallbackAcc = 0.0
    //var f_pure_CallbackAcc = 0.0
    //var f_write_res_CallbackAcc = 0.0

    //function printStats() {
    //    console.log("Glue code perf counters. tatal prep " + prepAcc.toFixed(3) +
    //        " ms; tatal wasm  " + runtimeAcc.toFixed(3) +
    //        " ms (callbacks to sundialsode.fs are included); outputCallback " + callbackAcc.toFixed(3) +
    //        " ms; fRates read " + f_read_prep_CallbackAcc.toFixed(3) +
    //        " ms; fRates pure " + f_pure_CallbackAcc.toFixed(3) +
    //        " ms; fRates write res " + f_write_res_CallbackAcc.toFixed(3) +
    //        " ms; total cleanup " + cleanAcc.toFixed(3) + " ms")
    //    setTimeout(printStats, 10000);
    //}

    //printStats();

    public allocEmHeap(nDataBytes: any) {
        var dataPtr = this.Module._malloc(nDataBytes);
        return dataPtr;
    }

    // data - number[], returns EmscriptenPtr
    public allocFloat64EmHeapAndCopyTo(data: any) {
        var typedData = new Float64Array(data)
        // Get data byte size, allocate memory on Emscripten heap, and get pointer
        var nDataBytes = typedData.length * typedData.BYTES_PER_ELEMENT
        var dataPtr = this.Module._malloc(nDataBytes)
        // Copy data to Emscripten heap (directly accessed from Module.HEAPU8)
        var dataHeap = new Uint8Array(this.Module.HEAPU8.buffer, dataPtr, nDataBytes)
        dataHeap.set(new Uint8Array(typedData.buffer))

        return dataHeap.byteOffset;
    }

    // data - number[], returns EmscriptenPtr
    public allocInt32EmHeapAndCopyTo(data: any) {
        var typedData = new Int32Array(data)
        // Get data byte size, allocate memory on Emscripten heap, and get pointer
        var nDataBytes = typedData.length * typedData.BYTES_PER_ELEMENT
        var dataPtr = this.Module._malloc(nDataBytes)
        // Copy data to Emscripten heap (directly accessed from Module.HEAPU8)
        var dataHeap = new Uint8Array(this.Module.HEAPU8.buffer, dataPtr, nDataBytes)
        dataHeap.set(new Uint8Array(typedData.buffer))

        return dataHeap.byteOffset;
    }

    public freeEmHeap(ptr: any) {
        this.Module._free(ptr);
    }

    private extractionCache: any = {}
    private writeCache: any = {}

    public extractFloat64ArrayFromEmMemory(dataPtr: any, N: any) {
        var key = dataPtr + "_" + N;
        if (!this.extractionCache[key])
            this.extractionCache[key] = new Float64Array(this.Module.HEAPU8.buffer, dataPtr, N);
        return this.extractionCache[key];
    }

    public writeNumArrayToEmMemory(dataPtr: any, arrayToWrite: any) {
        var N = arrayToWrite.length;
        var key = dataPtr + "_" + N;
        if (!this.writeCache[key])
            this.writeCache[key] = new Float64Array(this.Module.HEAPU8.buffer, dataPtr, N);
        this.writeCache[key].set(arrayToWrite);
    }

    //allocating emscripten memory for array parameters and filling it with the data
    private sizeOfFloat64 = 8

    private fnSundialsSolverWasm = this.Module.cwrap('fnSundialsSolver', 'number', ['number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number'])

    public fnSundialsSolver(arg: any) {
        //console.log("Glue code: got fnSundialsSolver call");
        //console.log(arg);
        //var bench1 = performance.now()            
        var self = this;

        var sundialsOutput = arg.sundialsOutput; // float * float array -> unit
        //this one is to be registered with callable emscripten function pointer address
        var sundialsOutput2 = function (t: any, conc: any, concN: any) {
            //var cbStart = performance.now()
            //conc is a emscripten mem pointer, we need to extract concN float64 values from there
            var extractedConc = self.extractFloat64ArrayFromEmMemory(conc, concN);
            sundialsOutput(t, extractedConc);
            //callbackAcc += performance.now() - cbStart;
        }
        // Each character within a signature string represents a type. The first character represents the return type of a function, and remaining characters are for parameter types.
        // https://kripken.github.io/emscripten-site/docs/porting/connecting_cpp_and_javascript/Interacting-with-code.html#interacting-with-code-call-function-pointers-from-c
        var outputFunPtr = this.Module.addFunction(sundialsOutput2, 'ddii') //double(*SundialsOutput)(double, double*, int);

        var functionalRates = arg.functionalRates; //float * float array -> float array
        //this one is to be regestered with callable emscripten function pointer address
        var functionalRates2 = function (t: any, fluxAddr: any, concAddr: any, concN: any) {
            //var cb1 = performance.now()
            var extractedConc = self.extractFloat64ArrayFromEmMemory(concAddr, concN);
            //var cb2 = performance.now()
            var fluxes = functionalRates(t, extractedConc);
            //var cb3 = performance.now()        
            self.writeNumArrayToEmMemory(fluxAddr, fluxes);
            //f_write_res_CallbackAcc += performance.now() - cb3;
            //f_pure_CallbackAcc += cb3 - cb2
            //f_read_prep_CallbackAcc += cb2 - cb1
        }
        var functionalRatesFunPtr = this.Module.addFunction(functionalRates2, 'vdiii') //void(*FunctionalRates)(double, double*, double*, int);        

        var emStoichM = this.allocInt32EmHeapAndCopyTo(arg.stoichM);
        var emPowersM = this.allocInt32EmHeapAndCopyTo(arg.powersM);
        var emRates = this.allocFloat64EmHeapAndCopyTo(arg.ratesM);
        var emStoichF = this.allocInt32EmHeapAndCopyTo(arg.stoichF);
        var emConcs = this.allocFloat64EmHeapAndCopyTo(arg.concs);
        var emT = this.allocFloat64EmHeapAndCopyTo(arg.t);
        var emFluxesDynamicM = this.allocEmHeap(arg.rNM * this.sizeOfFloat64);
        var emFluxesDynamicF = this.allocEmHeap(arg.rNF * this.sizeOfFloat64);
        /*
            sN:int,
            rNM:int,
            stoichM:int[], //len = rNM x sN
            powersM:int[], //len = rNM x sN
            ratesM:double[],
            rNF:int,
            stoichF:int[], //len = rNF x sN
            functionalRates: FunctionalRates,
            concs:double[], //len = sN
            t:double[], //len = tN
            tN:int,
            sundialsOutput:SundialsOutput,
            stiff:bool,
            abstol:double,
            reltol:double,
            fluxesDynamicM: double[], //len = rNM
            fluxesDynamicF: double[] // len = rNF
            ) = 0
     */

        //var bench2 = performance.now()

        var exitCode = this.fnSundialsSolverWasm(
            arg.sN,
            arg.rNM,
            emStoichM,
            emPowersM,
            emRates,
            arg.rNF,
            emStoichF,
            functionalRatesFunPtr,
            emConcs,
            emT,
            arg.tN,
            outputFunPtr,
            arg.stiff,
            arg.abstol,
            arg.reltol,
            emFluxesDynamicM,
            emFluxesDynamicF
        )

        // Returning back cons as side effect on parameter (native code does the same by writing to "conc" array address)    
        var conc = this.extractFloat64ArrayFromEmMemory(emConcs, arg.concs.length);
        arg.conc = conc;

        //var bench3 = performance.now()
        //console.log('fnSundialsSolver exit code ' + exitCode)

        this.Module.removeFunction(functionalRatesFunPtr);
        this.Module.removeFunction(outputFunPtr);

        this.freeEmHeap(emFluxesDynamicF);
        this.freeEmHeap(emFluxesDynamicM);
        this.freeEmHeap(emT);
        this.freeEmHeap(emConcs);
        this.freeEmHeap(emStoichF);
        this.freeEmHeap(emRates);
        this.freeEmHeap(emPowersM);
        this.freeEmHeap(emStoichM);
        //var bench4 = performance.now();

        //prepAcc += bench2 - bench1;
        //runtimeAcc += bench3 - bench2;
        //cleanAcc += bench4 - bench3;    

        //console.log("Glue code: codePrep " + (bench2 - bench1).toPrecision(3) + " ms; wasmIvokation " + (bench3 - bench2).toPrecision(3) + " ms (callbacks to sundialsode.fs are included); cleanup " + (bench4 - bench3).toPrecision(3) + " ms")
        //console.log("Glue code: tatal fnSundialsSolver time " + (bench4 - bench1).toPrecision(3)+" ms")
        return exitCode;
    };

    private fnSundialsCMESolverWasm = this.Module.cwrap('fnSundialsCMESolver', 'number', ['number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number', 'number'])

    public fnSundialsCMESolver(arg: any) {
        // const int stateN,
        // const int transN,
        // const double* propensities,
        // const int* sources,
        // const int* targets,
        // const double* diagonals,
        // double* prob_mass,
        // const double* timesToReturnParam,
        // const int numTimesToReturnParam,
        // SundialsOutput sundialsOutput,
        // const bool stiff,
        // const double abstol,
        // const double reltol
        var self = this;

        var sundialsOutput = arg.sundialsOutput; // float * float array -> unit
        //this one is to be registered with callable emscripten function pointer address
        var sundialsOutput2 = function (t: any, conc: any, concN: any) {
            //var cbStart = performance.now()
            //conc is a emscripten mem pointer, we need to extract concN float64 values from there
            var extractedConc = self.extractFloat64ArrayFromEmMemory(conc, concN);
            sundialsOutput(t, extractedConc);
            //callbackAcc += performance.now() - cbStart;
        }
        // Each character within a signature string represents a type. The first character represents the return type of a function, and remaining characters are for parameter types.
        // https://kripken.github.io/emscripten-site/docs/porting/connecting_cpp_and_javascript/Interacting-with-code.html#interacting-with-code-call-function-pointers-from-c
        var outputFunPtr = this.Module.addFunction(sundialsOutput2, 'ddii') //double(*SundialsOutput)(double, double*, int);        

        var emStoichM = this.allocInt32EmHeapAndCopyTo(arg.stoichM);

        var emPropensities = this.allocFloat64EmHeapAndCopyTo(arg.propensities);
        var emSources = this.allocInt32EmHeapAndCopyTo(arg.sources);
        var emTargets = this.allocInt32EmHeapAndCopyTo(arg.targets);
        var emDiagonals = this.allocFloat64EmHeapAndCopyTo(arg.diagonals);
        var emProb_mass = this.allocFloat64EmHeapAndCopyTo(arg.prob_mass);
        var emTimesToReturnParam = this.allocFloat64EmHeapAndCopyTo(arg.timesToReturnParam);

        var exitcode = this.fnSundialsCMESolverWasm(
            arg.stateN,
            arg.transN,
            emPropensities,
            emSources,
            emTargets,
            emDiagonals,
            emProb_mass,
            emTimesToReturnParam,
            arg.numTimesToReturnParam,
            outputFunPtr,
            arg.stiff,
            arg.abstol,
            arg.reltol
        );

        // Returning back "prob_mass" as side effect on parameter (native code does the same by writing to "prob_mass" array address)
        var prob_mass = this.extractFloat64ArrayFromEmMemory(emProb_mass, arg.prob_mass.length);
        arg.prob_mass = prob_mass;

        this.freeEmHeap(emTimesToReturnParam);
        this.freeEmHeap(emProb_mass);
        this.freeEmHeap(emDiagonals);
        this.freeEmHeap(emTargets);
        this.freeEmHeap(emSources);
        this.freeEmHeap(emPropensities);

        this.Module.removeFunction(outputFunPtr);

        return exitcode;
    };
}

export default WsEmGlue;