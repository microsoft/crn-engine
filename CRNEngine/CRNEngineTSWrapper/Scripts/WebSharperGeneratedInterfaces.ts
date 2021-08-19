//These interfaces are code generated from F#, any changes to this file will be lost.
export namespace WebSharperGeneratedInterfaces {

    export namespace Microsoft.Research.Filzbach.Parameters {
        export interface AcceptanceStatistics {
            accepted: number;
            altered: number;
        }
        export interface EvaluatedValues {
            values: Microsoft.Research.Filzbach.DataStructures.AssociativeArray<number>;
            logLikelihood: number;
            logPrior: number;
            iteration: number;
        }
        export interface LogNormal {
            mu: number;
            sigma: number;
        }
        export interface Normal {
            mean: number;
            stdev: number;
        }
        export interface ParameterRange {
            pType: Microsoft.Research.Filzbach.Parameters.ParameterType;
            lb: number;
            ub: number;
        }
        export interface TruncatedNormal {
            mean: number;
            stdev: number;
            lb: number;
            ub: number;
            denominator: number;
        }
        export type ParameterType = "Real" | "Log" | "Fixed"
        export const ParameterTypeSelect = ["Real", "Log", "Fixed"]
        export interface PriorValue {
            NormalPrior?: Microsoft.Research.Filzbach.Parameters.Normal;
            LogNormalPrior?: Microsoft.Research.Filzbach.Parameters.LogNormal;
            TruncatedNormalPrior?: Microsoft.Research.Filzbach.Parameters.TruncatedNormal;
        }
    }

    export namespace Microsoft.Research.CRNEngine {
        export interface Assignment {
            variables: Array<string>;
            values: Array<Array<Microsoft.Research.CRNEngine.Expression.t<string>>>;
        }
        export interface Attributes {
            name: string;
            structure: string;
            svg: string;
        }
        export interface Calculus<s> {
            react: (p0: Array<s>) => (p0: s) => Array<Microsoft.Research.CRNEngine.Reaction<s, Microsoft.Research.CRNEngine.Expression.t<string>, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<s>>>>;
        }
        export interface Column<v> {
            name: string;
            values: Array<v>;
        }
        export interface Crn {
            name: string;
            settings: Microsoft.Research.CRNEngine.Crn_settings<Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Microsoft.Research.CRNEngine.Species>>>;
            reactions: Array<Microsoft.Research.CRNEngine.Reaction<Microsoft.Research.CRNEngine.Species, Microsoft.Research.CRNEngine.Expression.t<string>, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Microsoft.Research.CRNEngine.Species>>>>;
            initials: Array<Microsoft.Research.CRNEngine.Initial<Microsoft.Research.CRNEngine.Species, Microsoft.Research.CRNEngine.Expression.t<string>>>;
            attributes: { [key: string]: Microsoft.Research.CRNEngine.Attributes };
        }
        export interface Crn_settings<e> {
            simulation: Microsoft.Research.CRNEngine.Simulation_settings<e>;
            simulations: Array<Microsoft.Research.CRNEngine.Simulation_settings<e>>;
            stochastic: Microsoft.Research.CRNEngine.Stochastic_settings;
            deterministic: Microsoft.Research.CRNEngine.Deterministic_settings;
            spatial: Microsoft.Research.CRNEngine.Spatial_settings<e>;
            inference: Microsoft.Research.CRNEngine.Inference_settings;
            moment_closure: Microsoft.Research.CRNEngine.Moment_closure_settings.t<Microsoft.Research.CRNEngine.Expression.t<Array<Opaque.FSharpTuple>>>;
            synthesis: Microsoft.Research.CRNEngine.Synthesis_settings;
            data: Array<Microsoft.Research.CRNEngine.Dataset>;
            units: Microsoft.Research.CRNEngine.Units;
            simulator: Microsoft.Research.CRNEngine.Simulator;
            parameters: Array<Microsoft.Research.CRNEngine.Parameter>;
            sweeps: Array<Microsoft.Research.CRNEngine.Sweep>;
            rates: { [key: string]: e };
            plot: Microsoft.Research.CRNEngine.Plot_settings<e>;
        }
        export interface Ctmc {
            graph: Microsoft.Research.CRNEngine.Dictionary.t<Opaque.FSharpMap<number, number>, Array<Microsoft.Research.CRNEngine.Transition>>;
            initial_state: Opaque.FSharpMap<number, number>;
        }
        export interface Dataset {
            file: string;
            data: Array<Microsoft.Research.CRNEngine.Table<number>>;
        }
        export interface Deterministic_settings {
            stiff: boolean;
            abstolerance: number;
            reltolerance: number;
        }
        export interface Event<s, time, v> {
            time: time;
            target: Microsoft.Research.CRNEngine.Target<s, v>;
        }
        export interface Gui {
            name: string;
            settings: Microsoft.Research.CRNEngine.Crn_settings<string>;
            reactions: Array<Microsoft.Research.CRNEngine.Reaction<string, string, string>>;
            initials: Array<Microsoft.Research.CRNEngine.Initial<string, string>>;
            attributes: { [key: string]: Microsoft.Research.CRNEngine.Attributes };
        }
        export interface GuiIG {
            task?: Microsoft.Research.CRNEngine.Task;
            nodes: { [key: string]: Microsoft.Research.CRNEngine.GuiModel };
            edges: { [key: string]: Array<Microsoft.Research.CRNEngine.GuiIGEdge> };
            expanded: boolean;
        }
        export interface GuiIGEdge {
            location: Microsoft.Research.CRNEngine.InferenceSiteGraph.Location;
            parameters: Array<Microsoft.Research.CRNEngine.GuiIGTargetParameter>;
        }
        export interface GuiIGTargetParameter {
            source: string;
            target: string;
            prior: Microsoft.Research.CRNEngine.InferenceSiteGraph.PriorKind;
        }
        export interface GuiModel {
            top: Microsoft.Research.CRNEngine.Gui;
            systems: Array<Microsoft.Research.CRNEngine.Gui>;
        }
        export interface Inference_settings {
            name: string;
            burnin: number;
            samples: number;
            thin: number;
            noise_model: Microsoft.Research.CRNEngine.Noise_model;
            noise_parameter: Microsoft.Research.CRNEngine.Noise_parameter;
            prune: boolean;
            seed: number;
            seeds: Array<number>;
            lowerbound?: number;
            timer: boolean;
            partial_evaluation: boolean;
            print_console: boolean;
            print_summary: boolean;
        }
        export interface Initial<s, v> {
            constant: boolean;
            value: v;
            species: s;
            time?: v;
            spatial?: Microsoft.Research.CRNEngine.Spatial_initial.t;
        }
        export interface Instance<e> {
            model: string;
            sweep: string;
            assignment: string;
            environment: { [key: string]: number };
            settings: Microsoft.Research.CRNEngine.Simulation_settings<e>;
            name: string;
        }
        export interface LogNormal {
            mu: number;
            sigma: number;
        }
        export interface Model {
            top: Microsoft.Research.CRNEngine.Crn;
            systems: Array<Microsoft.Research.CRNEngine.Crn>;
        }
        export interface Normal {
            mean: number;
            stdev: number;
        }
        export interface Parameter {
            name: string;
            value: number;
            prior?: Microsoft.Research.CRNEngine.Prior;
        }
        export interface Plot_settings<e> {
            x_label: string;
            y_label: string;
            title: string;
            label_font_size: number;
            tick_font_size: number;
            x_ticks: Array<number>;
            y_ticks: Array<number>;
            x_min?: number;
            x_max?: number;
            y_min?: number;
            y_max?: number;
            v_boundaries: Array<e>;
            h_boundaries: Array<e>;
        }
        export interface Point {
            mean: number;
            stdev: number;
        }
        export interface Population<species, value> {
            constant: boolean;
            value: value;
            species: species;
            initial: boolean;
            input: boolean;
            spatial_initial?: Microsoft.Research.CRNEngine.Spatial_initial.t;
            max?: number;
        }
        export interface Populations<s, v> {
            index_to_species: Array<Microsoft.Research.CRNEngine.Population<s, v>>;
            species_to_index: Opaque.Dictionary<s, number>;
        }
        export interface Prior {
            interval: Microsoft.Research.CRNEngine.Interval;
            distribution: Microsoft.Research.CRNEngine.Distribution;
            variation: Microsoft.Research.CRNEngine.Variation;
        }
        export interface Probabilities {
            times: Array<number>;
            stateprobabilities: Array<Array<number>>;
            stoichiometry: Array<Array<number>>;
            species: { [key: string]: number };
        }
        export interface Reaction<s, v, e> {
            catalysts: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reactants: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reverse?: Microsoft.Research.CRNEngine.Rate<v, e>;
            rate: Microsoft.Research.CRNEngine.Rate<v, e>;
            products: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
        }
        export interface Result<v> {
            instance: Microsoft.Research.CRNEngine.Instance<Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Microsoft.Research.CRNEngine.Species>>>;
            table: Microsoft.Research.CRNEngine.Table<v>;
        }
        export interface Row<v> {
            time: number;
            values: Array<v>;
        }
        export interface Simulation_settings<e> {
            name: string;
            initial: number;
            final: number;
            points: number;
            plots: Array<e>;
            plotcolours: Array<string>;
            seed?: number;
            kinetics: Microsoft.Research.CRNEngine.Kinetics;
            times: Array<number>;
            multicore: boolean;
            data: Array<string>;
            sweeps: Array<string>;
        }
        export interface Spatial_settings<s> {
            parameters: Array<Microsoft.Research.CRNEngine.Parameter>;
            diffusibles: Array<Opaque.FSharpTuple>;
            dimensions: number;
            boundary: Microsoft.Research.CRNEngine.Boundary;
            xmax: number;
            nx: number;
            dt: number;
            default_diffusion: number;
            random: number;
        }
        export interface Species {
            name: string;
        }
        export interface Stochastic_settings {
            scale: number;
            steps?: number;
            trajectories: number;
            stationary_skiptime?: number;
        }
        export interface Sweep {
            name: string;
            assignments: Array<Microsoft.Research.CRNEngine.Assignment>;
        }
        export interface Synthesis_settings {
            mode: Microsoft.Research.CRNEngine.Synthesis_mode
            solver: Microsoft.Research.CRNEngine.Z3Solver
            timeout?: number;
            seed?: number;
        }
        export interface Table<v> {
            times: Array<number>;
            columns: Array<Microsoft.Research.CRNEngine.Column<v>>;
        }
        export interface Task {
            task_type: Microsoft.Research.CRNEngine.TaskType;
            copies: number;
            copy_id: number;
            nodes: number;
        }
        export interface Transition {
            target: Opaque.FSharpMap<number, number>;
            propensity: Microsoft.Research.CRNEngine.Expression.t<string>;
        }
        export interface Truncated {
            mean: number;
            stdev: number;
            min: number;
            max: number;
        }
        export interface Uniform {
            min: number;
            max: number;
        }
        export interface Units {
            concentration: Microsoft.Research.CRNEngine.Concentration;
            time: Microsoft.Research.CRNEngine.Time;
            space: Microsoft.Research.CRNEngine.Space;
        }
        export interface ctmc_result<s> {
            ctmc: Microsoft.Research.CRNEngine.Ctmc;
            to_species: Array<s>;
        }
        export type Boundary = "Periodic" | "ZeroFlux"
        export const BoundarySelect = ["Periodic", "ZeroFlux"]
        export interface Concentration {
            Molar: number;
        }
        export interface Distribution {
            Uniform?: Microsoft.Research.CRNEngine.Uniform;
            Normal?: Microsoft.Research.CRNEngine.Normal;
            LogNormal?: Microsoft.Research.CRNEngine.LogNormal;
            TruncatedNormal?: Microsoft.Research.CRNEngine.Truncated;
        }
        export interface Inlined<s> {
            Species?: s;
            Time?: string;
        }
        export type Interval = "Real" | "Log"
        export const IntervalSelect = ["Real", "Log"]
        export interface Key<s> {
            Parameter?: string;
            Rate?: string;
            Species?: s;
            Time?: string;
        }
        export type Kinetics = "Contextual" | "Stochastic" | "Deterministic"
        export const KineticsSelect = ["Contextual", "Stochastic", "Deterministic"]
        export type Noise_model = "Constant" | "Proportional"
        export const Noise_modelSelect = ["Constant", "Proportional"]
        export type Noise_parameter = { Fixed: number } | "Random" | "Multiple"
        export interface Rate<v, e> {
            MassAction?: v;
            Function?: e;
        }
        export type Simulator = "Oslo" | "Sundials" | "SSA" | "CME" | "CMESundials" | "LNA" | "PDE" | "MC"
        export const SimulatorSelect = ["Oslo", "Sundials", "SSA", "CME", "CMESundials", "LNA", "PDE", "MC"]
        export interface Space {
            Metres: number;
        }
        export type Synthesis_mode = "Multistability" | "Turing"
        export const Synthesis_modeSelect = ["Multistability", "Turing"]
        export interface Target<s, v> {
            Species?: Microsoft.Research.CRNEngine.Populations<s, v>;
            OutputPoint?: string;
        }
        export type TaskType = "Parse" | "Simulate" | "Infer"
        export const TaskTypeSelect = ["Parse", "Simulate", "Infer"]
        export type Time = { Seconds: number }
        export type Variation = "Random" | "Fixed" | "Initial2" | "Multiple"
        export const VariationSelect = ["Random", "Fixed", "Initial2", "Multiple"]
        export type Z3Solver = "NLSat" | "Portfolio"
        export const Z3SolverSelect = ["NLSat", "Portfolio"]
    }

    export namespace Microsoft.Research.Filzbach.DataStructures {
        export interface AssociativeArray<a> {
            //CopyWithNewValues<a> : () => Microsoft.Research.Filzbach.DataStructures.AssociativeArray<a>;
            //Item : () => a;
            //Item : () => a;
            //ToArray : () => Array<a>;
            //ToMap : () => { [key: string]: a };
            //Names : () => Array<string>;
            //_dataArray : () => Array<a>;
            //_namesMap : () => Microsoft.Research.Filzbach.DataStructures.NamesMap;
            //ofArray<a> : () => Microsoft.Research.Filzbach.DataStructures.AssociativeArray<a>;
            //ofSeq<a> : () => Microsoft.Research.Filzbach.DataStructures.AssociativeArray<a>;
        }
        export interface NamedObject<a> {
            //Data : () => a;
            //Name : () => string;
        }
        export interface NamesMap {
            //Item : () => number;
            //Keys : () => Array<string>;
        }
    }

    export namespace System {

    }

    export namespace Microsoft.Research.Filzbach.Filzbach {
        export interface Burnin {
            space: Microsoft.Research.Filzbach.DataStructures.AssociativeArray<Microsoft.Research.Filzbach.Parameters.ParameterRange>;
            state: Microsoft.Research.Filzbach.Parameters.EvaluatedValues;
            thinningSkippedCount: number;
            chain: Array<Microsoft.Research.Filzbach.Parameters.EvaluatedValues>;
            stats: Array<Microsoft.Research.Filzbach.Parameters.AcceptanceStatistics>;
            randGen: Microsoft.Research.Filzbach.Lib.IRng;
            innovationGens: Opaque.FSharpMap<number, Microsoft.Research.Filzbach.Lib.NormalGenerator>;
            mle: Microsoft.Research.Filzbach.Parameters.EvaluatedValues;
            priors: Array<Microsoft.Research.Filzbach.Parameters.PriorValue | null>;
            indexes: Array<number>;
        }
        export interface Sampling {
            space: Microsoft.Research.Filzbach.DataStructures.AssociativeArray<Microsoft.Research.Filzbach.Parameters.ParameterRange>;
            state: Microsoft.Research.Filzbach.Parameters.EvaluatedValues;
            thinningSkippedCount: number;
            chain: Array<Microsoft.Research.Filzbach.Parameters.EvaluatedValues>;
            burnin: Array<Microsoft.Research.Filzbach.Parameters.EvaluatedValues>;
            randGen: Microsoft.Research.Filzbach.Lib.IRng;
            innovationGens: Opaque.FSharpMap<number, Microsoft.Research.Filzbach.Lib.NormalGenerator>;
            mle: Microsoft.Research.Filzbach.Parameters.EvaluatedValues;
            priors: Array<Microsoft.Research.Filzbach.Parameters.PriorValue | null>;
            indexes: Array<number>;
            accepted: number;
        }
        export interface RunPhase {
            BurninPhase?: Microsoft.Research.Filzbach.Filzbach.Burnin;
            SamplingPhase?: Microsoft.Research.Filzbach.Filzbach.Sampling;
        }
    }

    export namespace Microsoft.Research.CRNEngine.InferenceSiteGraph {
        export interface IGraph {
            task?: Microsoft.Research.CRNEngine.Task;
            nodes: Opaque.FSharpMap<string, Microsoft.Research.CRNEngine.Model>;
            edges: Opaque.FSharpMap<Microsoft.Research.CRNEngine.InferenceSiteGraph.Location, Array<Opaque.FSharpTuple>>;
            expanded: boolean;
        }
        export interface Location {
            NodeLoc?: string;
            SystemLoc?: Opaque.FSharpTuple;
        }
        export type PriorKind = "Fixed2" | "Normal" | "LogNormal" | "TruncatedNormal"
        export const PriorKindSelect = ["Fixed2", "Normal", "LogNormal", "TruncatedNormal"]
    }

    export namespace Microsoft.Research.Filzbach.Lib {
        export interface IRng {
            //NextDouble : () => Opaque.FSharpTuple;
            //NextInt32 : () => Opaque.FSharpTuple;
        }
        export interface NormalGenerator {
            //Mean : () => number;
            //Sigma : () => number;
            //State : () => Microsoft.Research.Filzbach.Lib.NormalGeneratorState;
            //nextDouble : () => Opaque.FSharpTuple;
        }
        export interface NormalGeneratorState {
            rnorm_phase: boolean;
            rnorm_2: number;
            rnorm_f: number;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Graph {
        export type LineStyle = "Normal" | "Dashed"
        export type Shape = "Ellipse" | "Box"
        export interface Node {
            id: string;
            label?: string;
            stroke?: string;
            fill?: string;
            shape?: Shape;
        }
        export interface Edge {
            source: string;
            destination: string;
            label?: string;
            style?: LineStyle;
            stroke?: string;
        }
        export interface Graph {
            nodes: Node[];
            edges: Edge[];
        }
    }

    export namespace Microsoft.Research.CRNEngine.JSAPI {
        export interface SynthesisDispersionResult {
            markersX: number[];
            markersY: number[];
            plotX: number[];
            plotY: number[];
            xMin: number;
            xMax: number;
            yMin: number;
            yMax: number;
        }
        export interface SynthesisValue {
            value: number;
            lowerBound?: number;
            upperBound?: number;
        }
        export interface SynthesisEquations {
            rateEquations: string;
            jacobian: string;
            diffusion: string;
            csts: string;
        }
        export interface SynthesisResult {
            message: string;
            values: { [variable: string]: SynthesisValue };
            equations: Microsoft.Research.CRNEngine.JSAPI.SynthesisEquations;
            dispersion?: Microsoft.Research.CRNEngine.JSAPI.SynthesisDispersionResult;
            jacobian?: Microsoft.Research.CRNEngine.Graph.Graph;
            crn?: Gui;
            code?: string;
        }
        export interface BistabilityPlot {
            speciesX: string;
            speciesY: string;
            state1x: number;
            state1y: number;
            state2x: number;
            state2y: number;
            initialsX: number[];
            initialsY: number[];
            state1simsX: number[][];
            state1simsY: number[][];
            state2simsX: number[][];
            state2simsY: number[][];
        }
        export interface InferenceParameter {
            name: string;
            range: Microsoft.Research.CRNEngine.JSAPI.InferenceParameterRange;
            initValue?: number;
        }
        export interface InferenceParameterRange {
            pType: Microsoft.Research.CRNEngine.JSAPI.InferenceParameterType;
            lb: number;
            ub: number;
        }
        export interface InferenceParameters {
            nodeId: string;
            parameters: Array<Microsoft.Research.CRNEngine.JSAPI.InferenceParameter>;
        }
        export interface column_array<v> {
            name: string;
            values: Array<v>;
        }
        export interface export_def {
            content_type: string;
            id: string;
            display_name: string;
            node_id?: string;
            instance?: string;
            content?: string[];
            save_content?: string;
        }
        export interface inference_burnin {
            space: { [key: string]: Microsoft.Research.Filzbach.Parameters.ParameterRange };
            state: Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values;
            stats: Array<Microsoft.Research.Filzbach.Parameters.AcceptanceStatistics>;
            mle: Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values;
            priors: Array<Microsoft.Research.Filzbach.Parameters.PriorValue | null>;
            indexes: Array<number>;
        }
        export interface inference_evaluated_values {
            values: { [key: string]: number };
            lglk: number;
        }
        export interface inference_result {
            nodeId: string;
            iteration: number;
            lkincreased: boolean;
            state: Microsoft.Research.CRNEngine.JSAPI.inference_phase;
            mlesims?: Array<Microsoft.Research.CRNEngine.Result<number>>;
            summary: string;
        }
        export interface inference_sampling {
            space: { [key: string]: Microsoft.Research.Filzbach.Parameters.ParameterRange };
            state: Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values;
            thinningSkippedCount: number;
            chain: Array<Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values>;
            mle: Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values;
            priors: Array<Microsoft.Research.Filzbach.Parameters.PriorValue | null>;
            indexes: Array<number>;
        }
        export interface jit<s> {
            jit: Microsoft.Research.CRNEngine.Jit.t<s>;
            calculus: Microsoft.Research.CRNEngine.Calculus<s>;
        }
        export interface probabilityMap {
            times: Array<number>;
            values: Array<number>;
            probabilities: Array<Array<number>>;
        }
        export interface sim_run {
            value_type: Microsoft.Research.CRNEngine.JSAPI.ValueType;
            instances: Array<Microsoft.Research.CRNEngine.Instance<string>>;
        }
        export interface state {
            species: { [key: string]: number };
            transitions: Array<Microsoft.Research.CRNEngine.JSAPI.transition>;
        }
        export interface state_space {
            states: Array<Microsoft.Research.CRNEngine.JSAPI.state>;
            start_index: number;
            attributes: Array<Microsoft.Research.CRNEngine.Attributes>;
        }
        export interface table_array<v> {
            times: Array<number>;
            columns: Array<Microsoft.Research.CRNEngine.JSAPI.column_array<v>>;
        }
        export interface transition {
            target: number;
            propensity: string;
        }
        export type InferenceParameterType = "Real" | "Log" | "Fixed"
        export const InferenceParameterTypeSelect = ["Real", "Log", "Fixed"]
        export type ValueType = "Float" | "MeanStdev" | "MeanStdevProbabilities" | "Spatial1D" | "Spatial2D" | "MeanStdevTable"
        export const ValueTypeSelect = ["Float", "MeanStdev", "MeanStdevProbabilities", "Spatial1D", "Spatial2D", "MeanStdevTable"]
        export interface inference_phase {
            BurninPhase?: Microsoft.Research.CRNEngine.JSAPI.inference_burnin;
            SamplingPhase?: Microsoft.Research.CRNEngine.JSAPI.inference_sampling;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Jit {
        export interface JitReaction<s, v, e> {
            id: number;
            payload: Microsoft.Research.CRNEngine.Reaction<s, v, e>;
            reacts: Array<Opaque.FSharpTuple>;
            prods: Array<Opaque.FSharpTuple>;
            prop: number;
        }
        export interface JitSpecies<s> {
            id: number;
            payload: s;
            population: number;
            isExplored: boolean;
            isConstant: boolean;
            deps: Array<number>;
        }
        export interface newplottable {
            name: string;
            structural?: string;
            svg?: string;
        }
        export interface t<s> {
            simulation: Microsoft.Research.CRNEngine.Simulation_settings<Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Inlined<s>>>;
            stochastic: Microsoft.Research.CRNEngine.Stochastic_settings;
            rates: { [key: string]: Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Inlined<s>> };
            parameters: Array<Microsoft.Research.CRNEngine.Parameter>;
            random: Microsoft.Research.CRNEngine.Rng.Random;
            sidDict: Opaque.Dictionary<s, number>;
            spDict: Opaque.Dictionary<number, Microsoft.Research.CRNEngine.Jit.JitSpecies<s>>;
            rxDict: Opaque.Dictionary<number, Microsoft.Research.CRNEngine.Jit.JitReaction<s, number, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Inlined<s>>>>;
            plotDict: Opaque.Dictionary<s, Array<number>>;
            mustPlotAll: boolean;
            printSpecies: (p0: s) => Microsoft.Research.CRNEngine.Jit.newplottable;
            equalsSpecies: (p0: s) => (p0: s) => boolean;
            matchesSpecies: (p0: s) => (p0: s) => boolean;
            now: number;
            stepsDone: number;
            nextPrintTime: number;
            events: Array<Microsoft.Research.CRNEngine.Event<s, number, number>>;
            totalProp: number;
            nextSID: number;
            nextRID: number;
            dataPoints: Array<Microsoft.Research.CRNEngine.Row<number>>;
        }
    }

    export namespace Microsoft.FSharp.Core {
        export interface Unit { }
        export interface Ref<T> {
            contents: T;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Rng {
        export interface Random {
            //Next : () => number;
            //Next : () => number;
            //NextConstrained : () => number;
            //NextDouble : () => number;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Expression {
        export interface divide<k> {
            div1: Microsoft.Research.CRNEngine.Expression.t<k>;
            div2: Microsoft.Research.CRNEngine.Expression.t<k>;
        }
        export interface minus<k> {
            sub1: Microsoft.Research.CRNEngine.Expression.t<k>;
            sub2: Microsoft.Research.CRNEngine.Expression.t<k>;
        }
        export interface modulo<k> {
            div: Microsoft.Research.CRNEngine.Expression.t<k>;
            modulo: Microsoft.Research.CRNEngine.Expression.t<k>;
        }
        export interface power<k> {
            base_: Microsoft.Research.CRNEngine.Expression.t<k>;
            exponent: Microsoft.Research.CRNEngine.Expression.t<k>;
        }
        export interface bexp<k> {
            BTrue?: string;
            BFalse?: string;
            BLT?: Microsoft.Research.CRNEngine.Expression.t<k>[];
            BLeq?: Microsoft.Research.CRNEngine.Expression.t<k>[];
            BEq?: Microsoft.Research.CRNEngine.Expression.t<k>[];
            BGeq?: Microsoft.Research.CRNEngine.Expression.t<k>[];
            BGT?: Microsoft.Research.CRNEngine.Expression.t<k>[];
            BNot?: Microsoft.Research.CRNEngine.Expression.bexp<k>;
            BAnd?: Microsoft.Research.CRNEngine.Expression.bexp<k>[];
            BOr?: Microsoft.Research.CRNEngine.Expression.bexp<k>[];
        }
        export interface t<k> {
            Key?: k;
            Float?: number;
            Times?: Array<Microsoft.Research.CRNEngine.Expression.t<k>>;
            Divide?: Microsoft.Research.CRNEngine.Expression.divide<k>;
            Power?: Microsoft.Research.CRNEngine.Expression.power<k>;
            Plus?: Array<Microsoft.Research.CRNEngine.Expression.t<k>>;
            Minus?: Microsoft.Research.CRNEngine.Expression.minus<k>;
            Absolute?: Microsoft.Research.CRNEngine.Expression.t<k>;
            Log?: Microsoft.Research.CRNEngine.Expression.t<k>;
            Modulo?: Microsoft.Research.CRNEngine.Expression.modulo<k>;
            Ceiling?: Microsoft.Research.CRNEngine.Expression.t<k>;
            Floor?: Microsoft.Research.CRNEngine.Expression.t<k>;
            Round?: Microsoft.Research.CRNEngine.Expression.t<k>;
            If?: (Microsoft.Research.CRNEngine.Expression.bexp<k> | Microsoft.Research.CRNEngine.Expression.t<k>)[];
        }
    }

    export namespace Microsoft.Research.CRNEngine.Spatial_initial {
        export interface core {
            inner: number;
            outer: number;
            width: number;
        }
        export interface point {
            x: number;
            y: number;
            width: number;
            value: number;
        }
        export interface rectangle {
            xmin: number;
            xmax: number;
            ymin: number;
            ymax: number;
            value: number;
        }
        export interface t {
            random: number;
            core?: Microsoft.Research.CRNEngine.Spatial_initial.core;
            points: Array<Microsoft.Research.CRNEngine.Spatial_initial.point>;
            rectangles: Array<Microsoft.Research.CRNEngine.Spatial_initial.rectangle>;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Mset {
        export interface entry<a> {
            element: a;
            multiplicity: number;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Inference {
        export interface mcmc_intermediate_result {
            iteration: number;
            state: Microsoft.Research.Filzbach.Filzbach.RunPhase;
            mlesims: Array<Microsoft.Research.CRNEngine.Result<number>>;
            summary: string;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Moment_closure_settings {
        export interface t<a> {
            order: number;
            initial_minimum: number;
            log_evaluation: boolean;
            plots: Array<a>;
        }
    }

    export namespace Microsoft.Research.CRNEngine.Dictionary {
        export interface t<a, b> {
            map: Opaque.FSharpMap<a, b>;
        }
    }

    export namespace Opaque {
        export interface FSharpMap<K, V> { }
        export interface Dictionary<K, V> { }
        export interface FSharpTuple { }
    }
    export namespace System {
        export interface Object { }
    }

    export interface valueTypeCaseString {
        (_arg1: Microsoft.Research.CRNEngine.JSAPI.ValueType): string
    }

    export interface parse_code {
        (s: string): Microsoft.Research.CRNEngine.InferenceSiteGraph.IGraph
    }

    export interface prepare_Model {
        (gui: Microsoft.Research.CRNEngine.GuiIG, model_id: string): Microsoft.Research.CRNEngine.Model
    }

    export interface prepare_CRN_for_sim {
        (gui: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>): Microsoft.Research.CRNEngine.Crn
    }

    export interface pop_to_svg {
        (crn: Microsoft.Research.CRNEngine.Crn, x: Microsoft.Research.CRNEngine.Populations<Microsoft.Research.CRNEngine.Species, number>): string
    }

    export interface get_instances {
        (gui: Microsoft.Research.CRNEngine.GuiIG, model_id: string): Array<Microsoft.Research.CRNEngine.Instance<string>>
    }

    export interface make_instance_name {
        (instance: Microsoft.Research.CRNEngine.Instance<string>): string
    }

    export interface simulateFloat {
        (ig: Microsoft.Research.CRNEngine.GuiIG, node_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, output: (p0: Microsoft.Research.CRNEngine.Row<number>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface simulateMeanStdev {
        (ig: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, output: (p0: Microsoft.Research.CRNEngine.Row<Microsoft.Research.CRNEngine.Point>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface simulateSpatial1D {
        (ig: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, output: (p0: Microsoft.Research.CRNEngine.Row<Array<number>>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface simulateSpatial2D {
        (ig: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, output: (p0: Microsoft.Research.CRNEngine.Row<Array<Array<number>>>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface simulateMeanStdevTable {
        (ig: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, output: (p0: Microsoft.Research.CRNEngine.Table<Microsoft.Research.CRNEngine.Point>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface convert_state_space<t> {
        (ctmc_result: Microsoft.Research.CRNEngine.ctmc_result<t>, namer: (p0: t) => Microsoft.Research.CRNEngine.Attributes): Microsoft.Research.CRNEngine.JSAPI.state_space
    }

    export interface simulateMeanStdevProbabilities {
        (ig: Microsoft.Research.CRNEngine.GuiIG, model_id: string, instance: Microsoft.Research.CRNEngine.Instance<string>, ctmc_output: (p0: Microsoft.Research.CRNEngine.JSAPI.state_space) => void, output: (p0: Microsoft.Research.CRNEngine.Row<Microsoft.Research.CRNEngine.Point>) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): Microsoft.Research.CRNEngine.Probabilities
    }

    export interface getProbabilityMap {
        (p: Microsoft.Research.CRNEngine.Probabilities, speciesName: string, lowerBound: number): Microsoft.Research.CRNEngine.JSAPI.probabilityMap
    }

    export interface simulateFloatJIT<a> {
        (gui: Microsoft.Research.CRNEngine.GuiModel, jit: Microsoft.Research.CRNEngine.JSAPI.jit<a>, output: (p0: Microsoft.Research.CRNEngine.Row<number>) => void, output_plottable: (p0: Microsoft.Research.CRNEngine.Jit.newplottable) => void, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, output_program: (p0: Microsoft.Research.CRNEngine.GuiIG) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface user_get_sim_runs {
        (guiig: Microsoft.Research.CRNEngine.GuiIG, model_id: string): Microsoft.Research.CRNEngine.JSAPI.sim_run
    }

    export interface user_parse_code {
        (code: string): Microsoft.Research.CRNEngine.GuiIG
    }

    export interface model_to_single_export {
        (ig: Microsoft.Research.CRNEngine.InferenceSiteGraph.IGraph, nodeId: string, id: string): Microsoft.Research.CRNEngine.JSAPI.export_def
    }

    export interface model_to_export {
        (final: boolean, ig: Microsoft.Research.CRNEngine.InferenceSiteGraph.IGraph, nodeId: string): Array<Microsoft.Research.CRNEngine.JSAPI.export_def>
    }

    export interface user_get_exports {
        (final: boolean, gui: Microsoft.Research.CRNEngine.GuiIG, nodeId: string): Array<Microsoft.Research.CRNEngine.JSAPI.export_def>
    }

    export interface user_get_export {
        (gui: Microsoft.Research.CRNEngine.GuiIG, nodeId: string, id: string): Microsoft.Research.CRNEngine.JSAPI.export_def
    }

    export interface convert_evaluated_values {
        (ev: Microsoft.Research.Filzbach.Parameters.EvaluatedValues): Microsoft.Research.CRNEngine.JSAPI.inference_evaluated_values
    }

    export interface convert_inference_result {
        (nodeId: string, res: Microsoft.Research.CRNEngine.Inference.mcmc_intermediate_result, lkIncreased: boolean): Microsoft.Research.CRNEngine.JSAPI.inference_result
    }

    export interface combine_dynchar_exports {
        (exports: Array<Opaque.FSharpTuple>): Microsoft.Research.CRNEngine.JSAPI.export_def
    }

    export interface user_infer_gui {
        (gui: Microsoft.Research.CRNEngine.GuiIG, output_export: (p0: Microsoft.Research.CRNEngine.JSAPI.export_def) => void, output_parameter_definitions: (p0: Microsoft.Research.CRNEngine.JSAPI.InferenceParameters) => void, output_inference: (p0: Microsoft.Research.CRNEngine.JSAPI.inference_result) => void, cancel: Microsoft.FSharp.Core.Ref<boolean>): void
    }

    export interface user_state_space {
        (gui: Microsoft.Research.CRNEngine.GuiIG): Microsoft.Research.CRNEngine.JSAPI.state_space
    }

    export interface user_state_space_jit<a> {
        (jit: Microsoft.Research.CRNEngine.JSAPI.jit<a>): Microsoft.Research.CRNEngine.JSAPI.state_space
    }

    export interface expression_to_string {
        (exp: Microsoft.Research.CRNEngine.Expression.t<string>): string
    }

    export interface mcplot_to_string {
        (mcplot: Microsoft.Research.CRNEngine.Expression.t<Array<Opaque.FSharpTuple>>): string
    }

    export interface test_crn_deterministic {
        (crn: string): Microsoft.Research.CRNEngine.JSAPI.table_array<number>
    }

    export interface test_crn_sundials {
        (crn: string): Microsoft.Research.CRNEngine.JSAPI.table_array<number>
    }

    export interface prepare_for_cme {
        (crnStr: string): Opaque.FSharpTuple
    }

    export interface test_crn_cme_sundials {
        (crn: string): Microsoft.Research.CRNEngine.JSAPI.table_array<Microsoft.Research.CRNEngine.Point>
    }

}