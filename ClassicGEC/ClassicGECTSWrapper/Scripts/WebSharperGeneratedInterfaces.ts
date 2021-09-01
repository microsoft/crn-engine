// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import { rSBOLDocument } from "./Interfaces"

//These interfaces are code generated from F#, any changes to this file will be lost.
export namespace WebSharperGeneratedInterfaces {

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
        export interface Column<v> {
            name: string;
            values: Array<v>;
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
            quiet: boolean;
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
        export interface LogNormal {
            mu: number;
            sigma: number;
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
        export interface Prior {
            interval: Microsoft.Research.CRNEngine.Interval;
            distribution: Microsoft.Research.CRNEngine.Distribution;
            variation: Microsoft.Research.CRNEngine.Variation;
        }
        export interface Reaction<s, v, e> {
            catalysts: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reactants: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reverse?: Microsoft.Research.CRNEngine.Rate<v, e>;
            rate: Microsoft.Research.CRNEngine.Rate<v, e>;
            products: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
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
            prune: boolean;
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
            mode: Microsoft.Research.CRNEngine.Synthesis_mode;
            solver: Microsoft.Research.CRNEngine.Z3Solver;
            timeout?: number;
            seed?: number;
        }
        export interface Table<v> {
            times: Array<number>;
            columns: Array<Microsoft.Research.CRNEngine.Column<v>>;
        }
        export interface Task {
            task_type?: Microsoft.Research.CRNEngine.TaskType;
            copies: number;
            copy_id: number;
            nodes: number;
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
        export type Interval = "Real" | "Log"
        export const IntervalSelect = ["Real", "Log"]
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
        export type TaskType = "Parse" | "Simulate" | "Infer"
        export const TaskTypeSelect = ["Parse", "Simulate", "Infer"]
        export type Time = { Seconds: number }
        export type Variation = "Random" | "Fixed" | "Initial2" | "Multiple"
        export const VariationSelect = ["Random", "Fixed", "Initial2", "Multiple"]
        export type Z3Solver = "NLSat" | "Portfolio"
        export const Z3SolverSelect = ["NLSat", "Portfolio"]
    }

    export namespace System {

    }

    export namespace Microsoft.Research.GEC.JSAPI {
        export interface ClassicResult {
            solution: Microsoft.Research.GEC.GECEngine.t;
            solutionCount: number;
            model: Microsoft.Research.CRNEngine.GuiIG;
            jsbol: rSBOLDocument;
            sbol: System.Object;
        }
        export interface LogicResult {
            solution: Microsoft.Research.GEC.GECEngine.t;
            solutionCount: number;
            model: Microsoft.Research.CRNEngine.GuiIG;
            jsbol: rSBOLDocument;
            sbol: System.Object;
        }
        export interface solution_result {
            model: Microsoft.Research.CRNEngine.GuiIG;
            jsbol: rSBOLDocument;
            sbol: System.Object;
            crnstring: string;
        }
        export interface solve_result {
            ClassicGEC?: Microsoft.Research.GEC.JSAPI.ClassicResult;
            LogicGEC?: Microsoft.Research.GEC.JSAPI.LogicResult;
        }
    }

    export namespace Microsoft.Research.CRNEngine.InferenceSiteGraph {
        export interface Location {
            NodeLoc?: string;
            SystemLoc?: Opaque.FSharpTuple;
        }
        export type PriorKind = "Fixed2" | "Normal" | "LogNormal" | "TruncatedNormal"
        export const PriorKindSelect = ["Fixed2", "Normal", "LogNormal", "TruncatedNormal"]
    }

    export namespace Microsoft.FSharp.Core {

    }

    export namespace Microsoft.Research.GEC.Ast {
        export interface aexp {
            FloatAExp?: number;
            IdAExp?: string;
            PlusAExp?: Microsoft.Research.GEC.Ast.aexp[];
            MinusAExp?: Microsoft.Research.GEC.Ast.aexp[];
            MulAExp?: Microsoft.Research.GEC.Ast.aexp[];
            DivAExp?: Microsoft.Research.GEC.Ast.aexp[];
            PowAExp?: Microsoft.Research.GEC.Ast.aexp[];
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

    export namespace Microsoft.Research.GEC.Database {
        export interface entry<a> {
            value: a;
            enabled: boolean;
            comments: string;
        }
        export interface t {
            parts: { [key: string]: Microsoft.Research.GEC.Database.entry<Microsoft.Research.GEC.Database.partType> };
            devices: Array<Opaque.FSharpTuple>;
            reactions: Array<Microsoft.Research.GEC.Database.entry<Microsoft.Research.GEC.Gecreaction.t>>;
        }
        export interface partType {
            PCR?: Microsoft.Research.GEC.Database.pcrProperty;
            PROM?: Array<Microsoft.Research.GEC.Database.promProperty>;
            RBS?: Microsoft.Research.GEC.Database.rbsProperty;
            TER?: string;
        }
        export interface pcrProperty {
            CODES: (Array<string> | number)[];
        }
        export interface promProperty {
            POS?: (Array<string> | number)[];
            NEG?: (Array<string> | number)[];
            CON?: number;
            FRATE?: Microsoft.Research.GEC.Ast.aexp;
        }
        export type rbsProperty = { RATE: number }
    }

    export namespace Microsoft.Research.CRNEngine.Mset {
        export interface entry<a> {
            element: a;
            multiplicity: number;
        }
    }

    export namespace Microsoft.Research.GEC.GECEngine {
        export interface t {
            options: Microsoft.Research.GEC.Options.t;
            database: Microsoft.Research.GEC.Database.t;
            solution?: Opaque.FSharpTuple;
        }
    }

    export namespace Microsoft.Research.GEC.Options {
        export interface t { }
    }

    export namespace Microsoft.Research.GEC.Gecreaction {
        export interface t { }
    }

    export namespace Microsoft.Research.CRNEngine.Moment_closure_settings {
        export interface t<a> {
            order: number;
            initial_minimum: number;
            log_evaluation: boolean;
            plots: Array<a>;
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

    export interface compile {
        (program: string, dbParts: string, dbReactions: string): Microsoft.Research.GEC.JSAPI.solve_result
    }

    export interface get_solution {
        (so: Microsoft.Research.GEC.JSAPI.solve_result, i: number): Microsoft.Research.GEC.JSAPI.solution_result
    }

}
