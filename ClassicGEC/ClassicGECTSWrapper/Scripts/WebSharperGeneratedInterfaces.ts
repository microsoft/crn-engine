// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//These interfaces are code generated from F#, any changes to this file will be lost.
export namespace WebSharperGeneratedInterfaces {

    export namespace FSBOL.ComponentInstance {
        export type Access = "Public" | "Private"
        export const AccessSelect = ["Public", "Private"]
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

    export namespace FSBOL.Attachment {
        export interface Attachment {
            //format : () => string | null;
            //hash : () => string | null;
            //size : () => System.Int64 | null;
            //source : () => string;
        }
    }

    export namespace System {

    }

    export namespace FSBOL.Implementation {
        export interface Implementation {
            //built : () => FSBOL.Implementation.Built | null;
        }
        export interface Built {
            CD?: FSBOL.ComponentDefinition.ComponentDefinition;
            MD?: FSBOL.ModuleDefinition.ModuleDefinition;
        }
    }

    export namespace FSBOL.Collection {
        export interface Collection {
            //members : () => Array<string>;
        }
    }

    export namespace FSBOL.CombinatorialDerivation {
        export interface CombinatorialDerivation {
            //strategy : () => FSBOL.CombinatorialDerivation.Strategy | null;
            //template : () => string;
            //variableComponents : () => Array<FSBOL.VariableComponent.VariableComponent>;
        }
        export type Strategy = "Enumerate" | "Sample"
        export const StrategySelect = ["Enumerate", "Sample"]
    }

    export namespace FSBOL.Component {
        export interface Component {
            //roleIntegrations : () => Array<FSBOL.Component.RoleIntegration>;
            //roles : () => Array<FSBOL.Role.Role>;
        }
        export type RoleIntegration = "OverrideRoles" | "MergeRoles"
        export const RoleIntegrationSelect = ["OverrideRoles", "MergeRoles"]
    }

    export namespace FSBOL.ComponentDefinition {
        export interface ComponentDefinition {
            //components : () => Array<FSBOL.Component.Component>;
            //roles : () => Array<FSBOL.Role.Role>;
            //sequenceAnnotations : () => Array<FSBOL.SequenceAnnotation.SequenceAnnotation>;
            //sequenceConstraints : () => Array<FSBOL.SequenceConstraint.SequenceConstraint>;
            //sequences : () => Array<FSBOL.Sequence.Sequence>;
            //types : () => Array<FSBOL.ComponentDefinition.ComponentDefinitionType>;
        }
        export interface ComponentDefinitionType {
            DNA?: string;
            RNA?: string;
            Protein?: string;
            SmallMolecule?: string;
            Complex?: string;
            Linear?: string;
            Circular?: string;
            SingleStranded?: string;
            DoubleStranded?: string;
            OtherType?: string;
        }
    }

    export namespace FSBOL.FunctionalComponent {
        export interface FunctionalComponent {
            //direction : () => FSBOL.FunctionalComponent.Direction;
        }
        export type Direction = "In" | "Out" | "InOut" | "NoDirection"
        export const DirectionSelect = ["In", "Out", "InOut", "NoDirection"]
    }

    export namespace FSBOL.Sequence {
        export interface Sequence {
            //elements : () => string;
            //encoding : () => FSBOL.Sequence.Encoding;
        }
        export interface Encoding {
            IUPACDNA?: string;
            IUPACPROTEIN?: string;
            SMILES?: string;
            OtherEncoding?: string;
        }
    }

    export namespace FSBOL.Model {
        export interface Model {
            //framework : () => FSBOL.Model.Framework;
            //language : () => FSBOL.Model.Language;
            //source : () => string;
        }
        export interface Framework {
            Continuous?: string;
            Discrete?: string;
            OtherFramework?: string;
        }
        export interface Language {
            SBML?: string;
            CellML?: string;
            BioPax?: string;
            OtherLanguage?: string;
        }
    }

    export namespace FSBOL.Interaction {
        export interface Interaction {
            //participations : () => Array<FSBOL.Participation.Participation>;
            //types : () => Array<FSBOL.Interaction.InteractionType>;
        }
        export interface InteractionType {
            Inhibition?: string;
            Stimulation?: string;
            BiochemicalReaction?: string;
            NonCovalentBinding?: string;
            Degradation?: string;
            GeneticProduction?: string;
            Control?: string;
            OtherInteractionType?: string;
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

    export namespace FSBOL.Location {
        export interface Location {
            //orientation : () => FSBOL.Location.Orientation;
        }
        export interface Orientation {
            Inline?: string;
            ReverseComplement?: string;
            OtherOrientation?: string;
        }
    }

    export namespace FSBOL.MapsTo {
        export interface MapsTo {
            //local : () => string;
            //refinement : () => FSBOL.MapsTo.Refinement;
            //remote : () => string;
        }
        export interface Refinement {
            UseRemote?: string;
            UseLocal?: string;
            VerifyIdentical?: string;
            Merge?: string;
            OtherRefinement?: string;
        }
    }

    export namespace FSBOL.Module {
        export interface Module {
            //definition : () => string;
            //mapsTos : () => Array<FSBOL.MapsTo.MapsTo>;
        }
    }

    export namespace FSBOL.ModuleDefinition {
        export interface ModuleDefinition {
            //functionalComponents : () => Array<FSBOL.FunctionalComponent.FunctionalComponent>;
            //interactions : () => Array<FSBOL.Interaction.Interaction>;
            //models : () => Array<FSBOL.Model.Model>;
            //modules : () => Array<FSBOL.Module.Module>;
            //roles : () => Array<FSBOL.Role.Role>;
        }
    }

    export namespace FSBOL.VariableComponent {
        export interface VariableComponent {
            //operator : () => FSBOL.VariableComponent.Operator;
            //variable : () => string;
            //variantCollections : () => Array<string>;
            //variantDerivations : () => Array<string>;
            //variants : () => Array<string>;
        }
        export type Operator = "ZeroOrOne" | "One" | "ZeroOrMore" | "OneOrMore"
        export const OperatorSelect = ["ZeroOrOne", "One", "ZeroOrMore", "OneOrMore"]
    }

    export namespace Microsoft.FSharp.Core {

    }

    export namespace FSBOL.Participation {
        export interface Participation {
            //participant : () => FSBOL.FunctionalComponent.FunctionalComponent;
            //roles : () => Array<FSBOL.Participation.ParticipationRole>;
        }
        export interface ParticipationRole {
            Inhibitor?: string;
            Inhibited?: string;
            Stimulator?: string;
            Stimulated?: string;
            Reactant?: string;
            Product?: string;
            PromoterParticipation?: string;
            Modifier?: string;
            Modified?: string;
            Template?: string;
            OtherParticipationRole?: string;
        }
    }

    export namespace FSBOL.SequenceConstraint {
        export interface SequenceConstraint {
            //object : () => FSBOL.Component.Component;
            //restriction : () => FSBOL.SequenceConstraint.Restriction;
            //subject : () => FSBOL.Component.Component;
        }
        export interface Restriction {
            Precedes?: string;
            SameOrientationAs?: string;
            OppositeOrientationAs?: string;
            DifferentFrom?: string;
            OtherRestriction?: string;
        }
    }

    export namespace FSBOL.Role {
        export interface Role {
            Promoter?: string;
            RBS?: string;
            CDS?: string;
            Terminator?: string;
            Gene?: string;
            Operator?: string;
            EngineeredGene?: string;
            MRNA?: string;
            Effector?: string;
            TranscriptionFactor?: string;
            OtherRole?: string;
        }
    }

    export namespace FSBOL.SBOLDocument {
        export interface SBOLDocument {
            //attachments : () => Array<FSBOL.Attachment.Attachment>;
            //collections : () => Array<FSBOL.Collection.Collection>;
            //combinatorialDerivations : () => Array<FSBOL.CombinatorialDerivation.CombinatorialDerivation>;
            //componentDefinitions : () => Array<FSBOL.ComponentDefinition.ComponentDefinition>;
            //implementations : () => Array<FSBOL.Implementation.Implementation>;
            //models : () => Array<FSBOL.Model.Model>;
            //moduleDefinitions : () => Array<FSBOL.ModuleDefinition.ModuleDefinition>;
            //sequences : () => Array<FSBOL.Sequence.Sequence>;
        }
    }

    export namespace FSBOL.SequenceAnnotation {
        export interface SequenceAnnotation {
            //componentObj : () => FSBOL.Component.Component | null;
            //locations : () => Array<FSBOL.Location.Location>;
            //roles : () => Array<FSBOL.Role.Role>;
        }
    }

    export namespace FSBOL.TopLevel {
        export interface TopLevel {
            //attachments : () => Array<string>;
        }
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

    export namespace FSBOL.JsonSerializer {
        export interface rAnnotation {
            qName: FSBOL.JsonSerializer.rQName;
            valueType: string;
            literal: FSBOL.JsonSerializer.rLiteral;
            uri: string;
            nestedQName: FSBOL.JsonSerializer.rQName;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
        }
        export interface rAttachment {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            source: string;
            format: string;
            size: number;
            hash: string;
        }
        export interface rCollection {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            members: Array<string>;
        }
        export interface rCombinatorialDerivation {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            strategy: string;
            template: string;
            variableComponents: Array<FSBOL.JsonSerializer.rVariableComponent>;
        }
        export interface rComponent {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            definition: string;
            access: string;
            mapsTos: Array<FSBOL.JsonSerializer.rMapsTo>;
            roles: Array<string>;
            roleIntegrations: Array<string>;
        }
        export interface rComponentDefinition {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            components: Array<FSBOL.JsonSerializer.rComponent>;
            sequenceAnnotations: Array<FSBOL.JsonSerializer.rSequenceAnnotation>;
            sequenceConstraints: Array<FSBOL.JsonSerializer.rSequenceConstraint>;
            sequences: Array<string>;
            types: Array<string>;
            roles: Array<string>;
        }
        export interface rCut {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            orientation: string;
            at: number;
        }
        export interface rFunctionalComponent {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            definition: string;
            access: string;
            mapsTos: Array<FSBOL.JsonSerializer.rMapsTo>;
            direction: string;
        }
        export interface rGenericLocation {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            orientation: string;
        }
        export interface rImplementation {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            built: string;
        }
        export interface rInteraction {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            types: Array<string>;
            participations: Array<FSBOL.JsonSerializer.rParticipation>;
        }
        export interface rLiteral {
            literalType: string;
            string: string;
            int: number;
            int64: number;
            double: number;
            bool: boolean;
        }
        export interface rMapsTo {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            local: string;
            remote: string;
            refinment: string;
        }
        export interface rModel {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            source: string;
            language: string;
            framework: string;
        }
        export interface rModule {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            definition: string;
            mapsTos: Array<FSBOL.JsonSerializer.rMapsTo>;
        }
        export interface rModuleDefinition {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            roles: Array<string>;
            functionalComponents: Array<FSBOL.JsonSerializer.rFunctionalComponent>;
            interactions: Array<FSBOL.JsonSerializer.rInteraction>;
            modules: Array<FSBOL.JsonSerializer.rModule>;
            models: Array<string>;
        }
        export interface rParticipation {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            roles: Array<string>;
            participant: string;
        }
        export interface rQName {
            qNameType: string;
            name: string;
            prefix: string;
            nameSpaceUri: string;
        }
        export interface rRange {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            orientation: string;
            startIndex: number;
            endIndex: number;
        }
        export interface rSBOLDocument {
            attachments: Array<FSBOL.JsonSerializer.rAttachment>;
            sequences: Array<FSBOL.JsonSerializer.rSequence>;
            componentDefinitions: Array<FSBOL.JsonSerializer.rComponentDefinition>;
            moduleDefinitions: Array<FSBOL.JsonSerializer.rModuleDefinition>;
            models: Array<FSBOL.JsonSerializer.rModel>;
            implementations: Array<FSBOL.JsonSerializer.rImplementation>;
            collections: Array<FSBOL.JsonSerializer.rCollection>;
            CombinatorialDerivation: Array<FSBOL.JsonSerializer.rCombinatorialDerivation>;
        }
        export interface rSequence {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            attachments: Array<string>;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            elements: string;
            encoding: string;
        }
        export interface rSequenceAnnotation {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            ranges: Array<FSBOL.JsonSerializer.rRange>;
            cuts: Array<FSBOL.JsonSerializer.rCut>;
            genericLocations: Array<FSBOL.JsonSerializer.rGenericLocation>;
            roles: Array<string>;
            componentObj: string;
        }
        export interface rSequenceConstraint {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            subject: string;
            object: string;
            restriction: string;
        }
        export interface rVariableComponent {
            uri: string;
            version: string;
            name: string;
            displayId: string;
            persistentIdentity: string;
            description: string;
            annotations: Array<FSBOL.JsonSerializer.rAnnotation>;
            operator: string;
            variants: Array<string>;
            variantCollections: Array<string>;
            variantDerivations: Array<string>;
            variable: string;
        }
    }

    export namespace Microsoft.Research.GEC.JSAPI {
        export interface solution_result {
            model: Microsoft.Research.CRNEngine.GuiIG;
            jsbol: FSBOL.JsonSerializer.rSBOLDocument;
            sbol: FSBOL.SBOLDocument.SBOLDocument;
            crnstring: string;
        }
        export interface solve_result {
            solution: Microsoft.Research.GEC.GECEngine.t;
            solutionCount: number;
            model: Microsoft.Research.CRNEngine.GuiIG;
            jsbol: FSBOL.JsonSerializer.rSBOLDocument;
            sbol: FSBOL.SBOLDocument.SBOLDocument;
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
        (o: Microsoft.Research.GEC.JSAPI.solve_result, i: number): Microsoft.Research.GEC.JSAPI.solution_result
    }

}