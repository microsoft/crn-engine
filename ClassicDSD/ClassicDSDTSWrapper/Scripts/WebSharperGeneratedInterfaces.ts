// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

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
        export interface Calculus<s> {
            react: (p0: Array<s>) => (p0: s) => Array<Microsoft.Research.CRNEngine.Reaction<s, Microsoft.Research.CRNEngine.Expression.t<string>, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<s>>>>;
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
        export interface Reaction<s, v, e> {
            catalysts: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reactants: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
            reverse?: Microsoft.Research.CRNEngine.Rate<v, e>;
            rate: Microsoft.Research.CRNEngine.Rate<v, e>;
            products: Array<Microsoft.Research.CRNEngine.Mset.entry<s>>;
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

    export namespace RulesDSD.Syntax {
        export interface Clause {
            head: RulesDSD.Syntax.Predicate;
            body: Array<RulesDSD.Syntax.Literal>;
        }
        export interface Domain {
            name: string;
            isComplementary: boolean;
            isToehold: boolean;
            tag?: RulesDSD.Syntax.Term;
        }
        export interface Bond {
            Bond?: number;
            Var?: Opaque.FSharpTuple;
        }
        export interface DomainT {
            Dom?: RulesDSD.Syntax.Domain;
            Var?: (Opaque.FSharpTuple | RulesDSD.Syntax.Term | null)[];
        }
        export interface Literal {
            Pos?: RulesDSD.Syntax.Predicate;
            Neg?: RulesDSD.Syntax.Predicate;
        }
        export interface Pattern {
            Nicking?: Array<Opaque.FSharpTuple>[];
            Inner?: Array<Opaque.FSharpTuple>;
            Strand?: Array<Opaque.FSharpTuple>;
            ThreePrime?: Array<Opaque.FSharpTuple>;
            FivePrime?: Array<Opaque.FSharpTuple>;
            Nihil?: string;
        }
        export interface Predicate {
            Pred: (string | Array<RulesDSD.Syntax.Term>)[];
        }
        export interface Process {
            Proc: Opaque.FSharpMap<number, Array<RulesDSD.Syntax.SiteT>>;
        }
        export interface Site {
            Unbound?: RulesDSD.Syntax.DomainT;
            Bound?: (RulesDSD.Syntax.DomainT | RulesDSD.Syntax.Bond)[];
        }
        export interface SiteT {
            Site?: RulesDSD.Syntax.Site;
            Var?: Opaque.FSharpTuple;
        }
        export interface Term {
            Var?: (number | string)[];
            Const?: string;
            Float?: number;
            Func?: (string | Array<RulesDSD.Syntax.Term>)[];
            TList?: Array<RulesDSD.Syntax.Term>;
            TCons?: RulesDSD.Syntax.Term[];
            Proc?: RulesDSD.Syntax.Process;
            Pat?: RulesDSD.Syntax.Pattern;
        }
    }

    export namespace System {

    }

    export namespace Microsoft.Research.DNA.Measures {
        export interface C { }
        export interface kcal { }
        export interface mol { }
    }

    export namespace Microsoft.Research.DNA.Dsd {
        export interface ClassicBundle {
            settings: Microsoft.Research.CRNEngine.Crn_settings<Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Microsoft.Research.DNA.Species.t>>>;
            meta: Microsoft.Research.DNA.Dsd.metadata;
            sreactions: Array<Microsoft.Research.CRNEngine.Reaction<Microsoft.Research.DNA.Species.t, Microsoft.Research.CRNEngine.Expression.t<string>, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<Microsoft.Research.DNA.Species.t>>>>;
            initials: Array<Microsoft.Research.CRNEngine.Initial<Microsoft.Research.DNA.Species.t, Microsoft.Research.CRNEngine.Expression.t<string>>>;
            dsdOptions: Microsoft.Research.DNA.Options.t;
        }
        export interface RulesBundle {
            settings: Microsoft.Research.CRNEngine.Crn_settings<Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<RulesDSD.Syntax.Process>>>;
            initials: Array<Microsoft.Research.CRNEngine.Initial<RulesDSD.Syntax.Process, Microsoft.Research.CRNEngine.Expression.t<string>>>;
            reactions: Array<Microsoft.Research.CRNEngine.Reaction<RulesDSD.Syntax.Process, Microsoft.Research.CRNEngine.Expression.t<string>, Microsoft.Research.CRNEngine.Expression.t<Microsoft.Research.CRNEngine.Key<RulesDSD.Syntax.Process>>>>;
            plotsCache: Opaque.Dictionary<RulesDSD.Syntax.Process, string>;
            dsdOptions: Microsoft.Research.DNA.Options.t;
            rules: Opaque.Dictionary<Opaque.FSharpTuple, Array<RulesDSD.Syntax.Clause>>;
        }
        export interface metadata {
            meta: Microsoft.Research.DNA.Sequence.mapping;
            info: Opaque.FSharpTuple;
            domain_colours: { [key: string]: string };
            subdomains: Opaque.Dictionary<Array<string>, Microsoft.Research.DNA.Domain.t>;
        }
        export interface bundle {
            ClassicDSD?: Microsoft.Research.DNA.Dsd.ClassicBundle;
            Rules?: Microsoft.Research.DNA.Dsd.RulesBundle;
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

    export namespace Microsoft.Research.CRNEngine.InferenceSiteGraph {
        export interface Location {
            NodeLoc?: string;
            SystemLoc?: Opaque.FSharpTuple;
        }
        export type PriorKind = "Fixed2" | "Normal" | "LogNormal" | "TruncatedNormal"
        export const PriorKindSelect = ["Fixed2", "Normal", "LogNormal", "TruncatedNormal"]
    }

    export namespace Microsoft.FSharp.Core.CompilerServices {
        export interface MeasureInverse<Measure> { }
        export interface MeasureOne { }
        export interface MeasureProduct<Measure1, Measure2> { }
    }

    export namespace Microsoft.FSharp.Core {

    }

    export namespace Microsoft.Research.DNA.JSAPI {
        export interface ParseObject {
            code: string;
            toeholds: string;
            specificities: string;
        }
        export interface ParseResult {
            bundle: Microsoft.Research.DNA.Dsd.bundle;
            settings: Microsoft.Research.DNA.Options.t;
            unexpanded: Microsoft.Research.CRNEngine.GuiIG;
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

    export namespace Microsoft.Research.DNA.Options {
        export interface renderer_branches {
            rotate_labels: boolean;
            arrange: Microsoft.Research.DNA.Options.arrange_mode;
        }
        export interface rendering {
            renderer: Microsoft.Research.DNA.Options.renderer;
            branches: Microsoft.Research.DNA.Options.renderer_branches;
            classic: Microsoft.Research.DNA.Options.renderer_classic;
        }
        export interface t {
            rules: Microsoft.Research.DNA.Options.semantics;
            leaks: boolean;
            pin_leaks: boolean;
            polymers: boolean;
            unproductive: boolean;
            sequence_rates: boolean;
            temperature: number;
            stabilityCorrection: number;
            coaxialDangle: number;
            doubleCoaxialDangle: number;
            terminalDangleFactor: number;
            coaxialCorrection: Microsoft.Research.CRNEngine.Expression.t<string>;
            declare_domains: boolean;
            check_dna: boolean;
            colour_toeholds: boolean;
            program: string;
            toeholds: string;
            specificities: string;
            plot_names: boolean;
            state_images: boolean;
            toehold_bind_rate: number;
            toehold_unbind_rate: number;
            leak_rate_l: Microsoft.Research.CRNEngine.Expression.t<string>;
            leak_rate_w: Microsoft.Research.CRNEngine.Expression.t<string>;
            pinleak_rate?: Microsoft.Research.CRNEngine.Expression.t<string>;
            tau_rate: Microsoft.Research.CRNEngine.Expression.t<string>;
            elementary_migration_rate: Microsoft.Research.CRNEngine.Expression.t<string>;
            toehold_length: number;
            specificity_length: number;
            local_concentrations: Array<Opaque.FSharpTuple>;
            generate_predicates: Microsoft.Research.DNA.Options.generate_predicates;
            rendering: Microsoft.Research.DNA.Options.rendering;
            is_jit: boolean;
            rulesProgram?: Opaque.FSharpMap<Opaque.FSharpTuple, Array<RulesDSD.Syntax.Clause>>;
        }
        export type arrange_mode = "Wide" | "Babylon"
        export const arrange_modeSelect = ["Wide", "Babylon"]
        export type generate_predicates = "All_predicates" | "No_predicates"
        export const generate_predicatesSelect = ["All_predicates", "No_predicates"]
        export type renderer = "Classic" | "Circles" | "Branches"
        export const rendererSelect = ["Classic", "Circles", "Branches"]
        export type renderer_classic = "Complement" | "Condensed" | "Nucleotides"
        export const renderer_classicSelect = ["Complement", "Condensed", "Nucleotides"]
        export type semantics = "Infinite" | "Default" | "Finite" | "Detailed"
        export const semanticsSelect = ["Infinite", "Default", "Finite", "Detailed"]
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

    export namespace Microsoft.Research.DNA.Origami {
        export interface content {
            C_strand?: Microsoft.Research.DNA.Strand.t;
            C_gate?: Array<Array<Microsoft.Research.DNA.Segment.t>>;
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

    export namespace Microsoft.Research.DNA.Value {
        export type fn = "Float2Int" | "Int2Float" | "Sqrt" | "Rate"
        export const fnSelect = ["Float2Int", "Int2Float", "Sqrt", "Rate"]
        export type op = "Plus" | "Minus" | "Mul" | "Div" | "Power" | "Max" | "Modulo"
        export const opSelect = ["Plus", "Minus", "Mul", "Div", "Power", "Max", "Modulo"]
        export interface t {
            String?: (string | Opaque.FSharpTuple | null)[];
            Int?: (number | Opaque.FSharpTuple | null)[];
            Domain?: (string | number | Microsoft.Research.CRNEngine.Expression.t<string> | Opaque.FSharpTuple | null)[];
            DomainS?: (string | number | Microsoft.Research.CRNEngine.Expression.t<string> | Opaque.FSharpTuple | null)[];
            Bool?: (boolean | Opaque.FSharpTuple | null)[];
            Char?: (string | Opaque.FSharpTuple | null)[];
            Float?: (number | Opaque.FSharpTuple | null)[];
            Variable?: (string | Opaque.FSharpTuple | null)[];
            Op?: (Microsoft.Research.DNA.Value.t | Microsoft.Research.DNA.Value.op | Opaque.FSharpTuple | null)[];
            Neg?: (Microsoft.Research.DNA.Value.t | Opaque.FSharpTuple | null)[];
            Show?: (Microsoft.Research.DNA.Value.t | Opaque.FSharpTuple | null)[];
            Function?: (Microsoft.Research.DNA.Value.fn | Microsoft.Research.DNA.Value.t | Opaque.FSharpTuple | null)[];
            Tuple?: (Array<Microsoft.Research.DNA.Value.t> | Opaque.FSharpTuple | null)[];
        }
    }

    export namespace Microsoft.Research.CRNEngine.JSAPI {
        export interface jit<s> {
            jit: Microsoft.Research.CRNEngine.Jit.t<s>;
            calculus: Microsoft.Research.CRNEngine.Calculus<s>;
        }
    }

    export namespace Microsoft.Research.DNA.Sequence {
        export interface mapping {
            toeholds: Array<string>;
            specificities: Array<string>;
            assigned: Array<Opaque.FSharpTuple>;
        }
    }

    export namespace Microsoft.Research.DNA.Species {
        export interface t {
            STRAND?: Microsoft.Research.DNA.Strand.t;
            GATE?: Array<Array<Microsoft.Research.DNA.Segment.t>>;
            ORIGAMI?: Array<Microsoft.Research.DNA.Origami.content>;
            UNKNOWN?: string;
            LogicDsdProcess?: RulesDSD.Syntax.Process;
        }
    }

    export namespace Microsoft.Research.DNA.Strand {
        export interface t {
            Upper?: Array<Microsoft.Research.DNA.Domain.t>;
            Lower?: Array<Microsoft.Research.DNA.Domain.t>;
        }
    }

    export namespace Microsoft.Research.DNA.Segment {
        export interface t {
            Double?: Opaque.FSharpTuple;
            Hairpin?: Opaque.FSharpTuple;
        }
    }

    export namespace Microsoft.Research.DNA.Domain {
        export interface t {
            Toe?: (Microsoft.Research.DNA.Value.t | boolean | Opaque.FSharpTuple | null)[];
            Normal?: (Microsoft.Research.DNA.Value.t | boolean | Opaque.FSharpTuple | null)[];
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

    export namespace Opaque {
        export interface FSharpMap<K, V> { }
        export interface Dictionary<K, V> { }
        export interface FSharpTuple { }
    }
    export namespace System {
        export interface Object { }
    }

    export interface set_options {
        (result: Microsoft.Research.DNA.JSAPI.ParseResult, settings: Microsoft.Research.DNA.Options.t): Microsoft.Research.DNA.JSAPI.ParseResult
    }

    export interface parser {
        (): (p0: string) => (p0: string) => (p0: string) => Microsoft.Research.DNA.Dsd.bundle
    }

    export interface user_parse {
        (code: Microsoft.Research.DNA.JSAPI.ParseObject): Microsoft.Research.DNA.JSAPI.ParseResult
    }

    export interface user_expand {
        (bundle: Microsoft.Research.DNA.Dsd.bundle): Microsoft.Research.CRNEngine.GuiModel
    }

    export interface user_compile {
        (code: Microsoft.Research.DNA.JSAPI.ParseObject): Microsoft.Research.CRNEngine.GuiModel
    }

    export interface user_parse_oldsyntax {
        (code: Microsoft.Research.DNA.JSAPI.ParseObject): Microsoft.Research.DNA.JSAPI.ParseResult
    }

    export interface user_compile_oldsyntax {
        (code: Microsoft.Research.DNA.JSAPI.ParseObject): Microsoft.Research.CRNEngine.GuiModel
    }

    export interface get_jit_classic {
        (bundle: Microsoft.Research.DNA.Dsd.bundle): Microsoft.Research.CRNEngine.JSAPI.jit<Microsoft.Research.DNA.Species.t>
    }

    export interface get_jit_rules {
        (bundle: Microsoft.Research.DNA.Dsd.bundle): Microsoft.Research.CRNEngine.JSAPI.jit<RulesDSD.Syntax.Process>
    }

    export interface is_jit {
        (bundle: Microsoft.Research.DNA.Dsd.bundle): boolean
    }

    export interface is_classic {
        (bundle: Microsoft.Research.DNA.Dsd.bundle): boolean
    }

}