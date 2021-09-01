// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

import * as ko from 'knockout';
import { WebSharperGeneratedInterfaces as WGI } from "./../../../CRNEngine/CRNEngineTSWrapper/Scripts/WebSharperGeneratedInterfaces";
// https://github.com/Microsoft/TypeScript/issues/5711
import * as Serializable from '../../../CRNEngine/CRNEngineTSWrapper/Scripts/Interfaces';

export function expressionToString(e: Serializable.Expression): string {
    if (e.Key != null)
        return e.Key;
    else if (e.Float != null)
        return e.Float.toString();
    else if (e.Times != null)
        return '(' + e.Times.map(expressionToString).join('*') + ')';
    else if (e.Divide != null)
        return '(' + e.Divide.div1 + '/' + e.Divide.div2 + ')';
    else if (e.Power != null)
        return '(' + e.Power.base_ + '^' + e.Power.exponent + ')';
    else if (e.Plus != null)
        return '(' + e.Plus.map(expressionToString).join('+') + ')';
    else if (e.Minus != null)
        return '(' + e.Minus.sub1 + '+' + e.Minus.sub2 + ')';
    else if (e.Absolute != null)
        return 'Abs(' + expressionToString(e.Absolute) + ')';
    return null;
}

export class CRNAssignments {
    variables: string[] = [];
    values: Serializable.Expression[][] = [[{}]];
}

export class CRNSweep {
    constructor(source?: WGI.Microsoft.Research.CRNEngine.Sweep) {
        if (source == null)
            source = { name: "", assignments: [] };
        this._source = source;
        this.Name(source.name);
        this.Name.subscribe(v => source.name = v);
        this.Assignments(source.assignments);
        this.Assignments.subscribe(v => source.assignments = v);
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Sweep;
    public get source(): WGI.Microsoft.Research.CRNEngine.Sweep { return this._source; }
    Name = ko.observable<string>("");
    Assignments = ko.observableArray<CRNAssignments>([]);
}

export class CRNSimSettings {
    constructor() {
        this.Name.subscribe(v => { if (this.source != null) this.source.name = v; });
        this.Points.subscribe(v => { if (this.source != null) this.source.points = v; });
        this.Initial.subscribe(v => { if (this.source != null) this.source.initial = v; });
        this.Final.subscribe(v => { if (this.source != null) this.source.final = v; });
        this.Plots.subscribe(v => { if (this.source != null) this.source.plots = v; });
        this.IsSeedEnabled.subscribe(v => { if (this.source != null) { if (v) this.source.seed = v ? this.Seed() : null; else delete this.source.seed; } });
        this.Seed.subscribe(v => { if (this.source != null) { if (v == null || !this.IsSeedEnabled()) delete this.source.seed; else this.source.seed = v; } });
        this.Kinetics.subscribe(v => { if (this.source != null) this.source.kinetics = v; });
        this.Times.subscribe(v => { if (this.source != null) this.source.times = v; });
        this.Multicore.subscribe(v => { if (this.source != null) this.source.multicore = v; });
        this.Data.subscribe(v => { if (this.source != null) this.source.data = v; });
        this.Sweeps.subscribe(v => { if (this.source != null) this.source.sweeps = v; });
    }
    private _source: Serializable.SimSettings;
    public get source(): Serializable.SimSettings { return this._source; }
    public set source(source: Serializable.SimSettings) {
        this._source = source;
        this.Name(source == null ? null : source.name);
        this.Points(source == null ? null : source.points);
        this.Initial(source == null ? null : source.initial);
        this.Final(source == null ? null : source.final);
        this.Plots(source == null ? null : source.plots);
        this.IsSeedEnabled(source == null ? null : source.seed != null);
        this.Seed(source == null || source.seed == null ? null : source.seed);
        this.Kinetics(source == null ? null : source.kinetics);
        this.Times(source == null ? null : source.times);
        this.Multicore(source == null ? null : source.multicore);
        this.Data(source == null ? null : source.data);
        this.Sweeps(source == null ? null : source.sweeps);
    }
    Name = ko.observable<string>(null);
    Points = ko.observable<number>(null).extend({ numeric: false });
    Initial = ko.observable<number>(null).extend({ numeric: false });
    Final = ko.observable<number>(null).extend({ numeric: false });
    Plots = ko.observableArray<string>(null);
    IsSeedEnabled = ko.observable<boolean>(null);
    Seed = ko.observable<number>(null).extend({ numeric: true });
    Kinetics = ko.observable<Serializable.Kinetics>(null);
    Times = ko.observableArray<number>(null);
    Multicore = ko.observable<boolean>(null);
    Data = ko.observableArray<string>(null);
    Sweeps = ko.observableArray<string>(null);
}

export class CRNStochasticSettings {
    constructor() {
        this.Scale.subscribe(v => { if (this.source != null) this.source.scale = v; });
        this.Trajectories.subscribe(v => { if (this.source != null) { if (v == null) delete this.source.trajectories; else this.source.trajectories = v; } });
        this.AreStepsAccounted.subscribe(v => { if (this.source != null) { if (v) this.source.steps = this.Steps(); else delete this.source.steps; } });
        this.Steps.subscribe(v => { if (this.source != null) { if (v == null || !this.AreStepsAccounted()) delete this.source.steps; else this.source.steps = v; } });
        this.StationarySkiptime.subscribe(v => { if (this.source != null) { if (v == null) delete this.source.stationary_skiptime; else this.source.stationary_skiptime = v; } });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Stochastic_settings
    public get source(): WGI.Microsoft.Research.CRNEngine.Stochastic_settings { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Stochastic_settings) {
        this._source = source;
        this.Scale(source == null ? null : source.scale);
        this.Trajectories(source == null ? null : source.trajectories);
        this.AreStepsAccounted(source == null ? null : source.steps != null);
        this.Steps(source == null || source.steps == null ? null : source.steps);
        this.StationarySkiptime(source == null || source.stationary_skiptime == null ? null : source.stationary_skiptime);
    }
    Scale = ko.observable<number>(null).extend({ numeric: false });
    Trajectories = ko.observable<number>(null).extend({ numeric: false });
    AreStepsAccounted = ko.observable<boolean>(null);
    Steps = ko.observable<number>(null).extend({ numeric: true });
    StationarySkiptime = ko.observable<number>(null).extend({ numeric: true });
}

export class CRNDeterministicSettings {
    constructor() {
        this.Stiff.subscribe(v => { if (this.source != null) this.source.stiff = v; });
        this.AbsTolerance.subscribe(v => { if (this.source != null) this.source.abstolerance = v; });
        this.RelTolerance.subscribe(v => { if (this.source != null) this.source.reltolerance = v; });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Deterministic_settings;
    public get source(): WGI.Microsoft.Research.CRNEngine.Deterministic_settings { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Deterministic_settings) {
        this._source = source;
        this.Stiff(source == null ? null : source.stiff);
        this.AbsTolerance(source == null ? null : source.abstolerance);
        this.RelTolerance(source == null ? null : source.reltolerance);
    }
    Stiff = ko.observable<boolean>(null);
    AbsTolerance = ko.observable<number>(null).extend({ numeric: true });
    RelTolerance = ko.observable<number>(null).extend({ numeric: true });
}

export class CRNPDESettings {
    constructor() {
        this.Dimensions.subscribe(v => { if (this.source != null) this.source.dimensions = v; });
        this.Boundary.subscribe(v => { if (this.source != null) this.source.boundary = v; });
        this.XMax.subscribe(v => { if (this.source != null) this.source.xmax = v; });
        this.Nx.subscribe(v => { if (this.source != null) this.source.nx = v; });
        this.Dt.subscribe(v => { if (this.source != null) this.source.dt = v; });
        this.DefaultDiffusion.subscribe(v => { if (this.source != null) this.source.default_diffusion = v; });
        this.Random.subscribe(v => { if (this.source != null) this.source.random = v; });
        this.Parameters.subscribe(v => { if (this.source != null) this.source.parameters = v.map(p => p.source); });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Spatial_settings<string>;
    public get source(): WGI.Microsoft.Research.CRNEngine.Spatial_settings<string> { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Spatial_settings<string>) {
        this._source = source;
        this.Dimensions(source == null ? null : source.dimensions);
        this.Boundary(source == null ? null : source.boundary);
        this.XMax(source == null ? null : source.xmax);
        this.Nx(source == null ? null : source.nx);
        this.Dt(source == null ? null : source.dt);
        this.DefaultDiffusion(source == null ? null : source.default_diffusion);
        this.Random(source == null ? null : source.random);
        this.Parameters(source == null ? [] : source.parameters.map(p => new ParameterVM(p)));
    }
    Parameters = ko.observableArray<ParameterVM>([]);
    Dimensions = ko.observable<number>(null).extend({ numeric: false });
    Boundary = ko.observable<WGI.Microsoft.Research.CRNEngine.Boundary>(null);
    XMax = ko.observable<number>(null).extend({ numeric: false });
    Nx = ko.observable<number>(null).extend({ numeric: false });
    Dt = ko.observable<number>(null).extend({ numeric: false });
    DefaultDiffusion = ko.observable<number>(null).extend({ numeric: false });
    Random = ko.observable<number>(null).extend({ numeric: false });
}

export class CRNMomentClosureSettings {
    constructor() {
        this.Order.subscribe(v => { if (this.source != null) this.source.order = v; });
        this.InitialMinimum.subscribe(v => { if (this.source != null) this.source.initial_minimum = v; });
        this.LogEvaluation.subscribe(v => { if (this.source != null) this.source.log_evaluation = v; });
    }
    private _source: Serializable.MCSettings;
    public get source(): Serializable.MCSettings { return this._source; }
    public set source(source: Serializable.MCSettings) {
        this._source = source;
        this.Order(source == null ? null : source.order);
        this.InitialMinimum(source == null ? null : source.initial_minimum);
        this.LogEvaluation(source == null ? null : source.log_evaluation);
        this.Plots = (source == null ? [] : source.plots);
    }
    Order = ko.observable<number>(null).extend({ numeric: false });
    InitialMinimum = ko.observable<number>(null).extend({ numeric: false });
    LogEvaluation = ko.observable<boolean>(null);
    Plots: Serializable.MCPlottable[] = [];
}

export class CRNSynthesisSettings {
    constructor() {
        this.Mode.subscribe(v => { if (this.source != null) this.source.mode = v; });
        this.Solver.subscribe(v => { if (this.source != null) this.source.solver = v; });
        this.Timeout.subscribe(v => { if (this.source != null) this.source.timeout = v; });
        this.Seed.subscribe(v => { if (this.source != null) this.source.seed = v; });
    }
    private _source: Serializable.SynthesisSettings;
    public get source(): Serializable.SynthesisSettings { return this._source; }
    public set source(source: Serializable.SynthesisSettings) {
        this._source = source;
        this.Mode(source == null ? "Multistability" : source.mode);
        this.Solver(source == null ? "Portfolio" : source.solver);
        this.Timeout(source == null ? null : source.timeout);
        this.Seed(source == null ? null : source.seed);
    }
    Mode = ko.observable<WGI.Microsoft.Research.CRNEngine.Synthesis_mode>("Multistability");
    PossibleModes = WGI.Microsoft.Research.CRNEngine.Synthesis_modeSelect;
    Solver = ko.observable<WGI.Microsoft.Research.CRNEngine.Z3Solver>("Portfolio");
    PossibleSolvers = WGI.Microsoft.Research.CRNEngine.Z3SolverSelect;
    Timeout = ko.observable<number>(null);
    Seed = ko.observable<number>(null);
}

export class CRNUnits {
    constructor() {
        this.Concentration.subscribe(v => { if (this.source != null) this.source.concentration.Molar = v; });
        this.Time.subscribe(v => { if (this.source != null) this.source.time.Seconds = v; });
        this.Space.subscribe(v => { if (this.source != null) this.source.space.Metres = v; });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Units;
    public get source(): WGI.Microsoft.Research.CRNEngine.Units { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Units) {
        this._source = source;
        this.Concentration(source == null ? null : source.concentration.Molar);
        this.Time(source == null ? null : source.time.Seconds);
        this.Space(source == null ? null : source.space.Metres);
    }
    Concentration = ko.observable<number>(null).extend({ numeric: false });
    Time = ko.observable<number>(null).extend({ numeric: false });
    Space = ko.observable<number>(null).extend({ numeric: false });
}

export class CRNPlotSettings {
    constructor() {
        this.XLabel.subscribe(v => { if (this.source != null) this.source.x_label = v; });
        this.YLabel.subscribe(v => { if (this.source != null) this.source.y_label = v; });
        this.Title.subscribe(v => { if (this.source != null) this.source.title = v; });
        this.LabelFontSize.subscribe(v => { if (this.source != null) this.source.label_font_size = v; });
        this.TickFontSize.subscribe(v => { if (this.source != null) this.source.tick_font_size = v; });
        this.XTicks.subscribe(v => { if (this.source != null) this.source.x_ticks = v; });
        this.YTicks.subscribe(v => { if (this.source != null) this.source.y_ticks = v; });
        this.XMin.subscribe(v => { if (this.source != null) this.source.x_min = v; });
        this.XMax.subscribe(v => { if (this.source != null) this.source.x_max = v; });
        this.YMin.subscribe(v => { if (this.source != null) this.source.y_min = v; });
        this.YMax.subscribe(v => { if (this.source != null) this.source.y_max = v; });
        this.VBoundaries.subscribe(v => { if (this.source != null) this.source.v_boundaries = v; });
        this.HBoundaries.subscribe(v => { if (this.source != null) this.source.h_boundaries = v; });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Plot_settings<string>;
    public get source(): WGI.Microsoft.Research.CRNEngine.Plot_settings<string> { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Plot_settings<string>) {
        this._source = source;
        this.XLabel(source == null ? "Time" : source.x_label);
        this.YLabel(source == null ? "Concentration (nM)" : source.y_label);
        this.Title(source == null ? "" : source.title);
        this.LabelFontSize(source == null ? 16 : source.label_font_size);
        this.TickFontSize(source == null ? 16 : source.tick_font_size);
        this.XTicks(source == null ? [] : source.x_ticks);
        this.YTicks(source == null ? [] : source.y_ticks);
        this.XMin(source == null ? null : source.x_min);
        this.XMax(source == null ? null : source.x_max);
        this.YMin(source == null ? null : source.y_min);
        this.YMax(source == null ? null : source.y_max);
        this.VBoundaries(source == null ? [] : source.v_boundaries);
        this.HBoundaries(source == null ? [] : source.h_boundaries);
    }
    XLabel = ko.observable<string>("Time");
    YLabel = ko.observable<string>("Concentration (nM)");
    Title = ko.observable<string>("");
    LabelFontSize = ko.observable<number>(16).extend({ numeric: false });
    TickFontSize = ko.observable<number>(16).extend({ numeric: false });
    XTicks = ko.observable<number[]>([]).extend({ array: true });
    YTicks = ko.observable<number[]>([]).extend({ array: true });
    XMin = ko.observable<number>(null).extend({ numeric: true });
    XMax = ko.observable<number>(null).extend({ numeric: true });
    YMin = ko.observable<number>(null).extend({ numeric: true });
    YMax = ko.observable<number>(null).extend({ numeric: true });
    VBoundaries = ko.observable<string[]>([]).extend({ array: false });
    HBoundaries = ko.observable<string[]>([]).extend({ array: false });
}

export class TableColumn {
    name: string;
    values: number[];
}

export class CRNTableReplicate {
    times: number[];
    columns: TableColumn[];
};

export class DataSet {
    constructor(source?: WGI.Microsoft.Research.CRNEngine.Dataset) {
        if (source == null)
            source = { file: "", data: [] };
        this.source = source;
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Dataset;
    public get source(): WGI.Microsoft.Research.CRNEngine.Dataset { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Dataset) {
        this._source = source;
        this.name = source == null ? null : source.file;
        this.tables = source == null ? [] : source.data;
    }
    name: string;
    tables: CRNTableReplicate[];
}

export type DistributionType = "Uniform" | "Normal" | "LogNormal" | "TruncatedNormal";

export class ParameterVM {
    constructor(public source?: WGI.Microsoft.Research.CRNEngine.Parameter) {
        if (source == null)
            source = { name: "", value: null }
        this.name(source.name);
        this.name.subscribe(v => source.name = v);
        this.value(source.value);
        this.value.subscribe(v => source.value = v);
        if (source.prior == null) {
            this.interval(null);
            this.variation(null);
            this.distribution(null);
        }
        else {
            this.interval(source.prior.interval);
            this.variation(source.prior.variation);
            if (source.prior.distribution.Normal != null) {
                this.distribution("Normal");
                this.mean(source.prior.distribution.Normal.mean);
                this.stdev(source.prior.distribution.Normal.stdev);
            }
            else if (source.prior.distribution.LogNormal != null) {
                this.distribution("LogNormal");
                this.mu(source.prior.distribution.LogNormal.mu);
                this.sigma(source.prior.distribution.LogNormal.sigma);
            }
            else if (source.prior.distribution.Uniform != null) {
                this.distribution("Uniform");
                this.min(source.prior.distribution.Uniform.min);
                this.max(source.prior.distribution.Uniform.max);
            }
            else {
                this.distribution("TruncatedNormal");
                this.min(source.prior.distribution.TruncatedNormal.min);
                this.max(source.prior.distribution.TruncatedNormal.max);
                this.mean(source.prior.distribution.TruncatedNormal.mean);
                this.stdev(source.prior.distribution.TruncatedNormal.stdev);
            }
        }
        var self = this;
        function updatePrior(): void {
            source.prior = self.distribution() == null ? null : {
                interval: self.interval(),
                variation: self.variation(),
                distribution: self.distribution() == "Uniform" ? { Uniform: { min: self.min(), max: self.max() } } :
                    self.distribution() == "Normal" ? { Normal: { mean: self.mean(), stdev: self.stdev() } } :
                        self.distribution() == "LogNormal" ? { LogNormal: { mu: self.mu(), sigma: self.sigma() } } :
                            { TruncatedNormal: { min: self.min(), max: self.max(), mean: self.mean(), stdev: self.stdev() } }
            };
        }
        this.distribution.subscribe(updatePrior);
        this.min.subscribe(updatePrior);
        this.max.subscribe(updatePrior);
        this.stdev.subscribe(updatePrior);
        this.mean.subscribe(updatePrior);
        this.sigma.subscribe(updatePrior);
        this.mu.subscribe(updatePrior);
    }
    public getParameter(): WGI.Microsoft.Research.CRNEngine.Parameter {
        return {
            name: this.name(),
            value: this.value(),
            prior: this.distribution() == null ? null : {
                interval: this.interval(),
                variation: this.variation(),
                distribution: this.distribution() == "Uniform" ? { Uniform: { min: this.min(), max: this.max() } } :
                    this.distribution() == "Normal" ? { Normal: { mean: this.mean(), stdev: this.stdev() } } :
                        this.distribution() == "LogNormal" ? { LogNormal: { mu: this.mu(), sigma: this.sigma() } } :
                            { TruncatedNormal: { min: this.min(), max: this.max(), mean: this.mean(), stdev: this.stdev() } }
            }
        };
    }

    name = ko.observable<string>("");
    value = ko.observable<number>(null).extend({ numeric: true });

    interval = ko.observable<Serializable.PriorInterval>(null);
    variation = ko.observable<Serializable.PriorVariation>(null);
    distribution = ko.observable<DistributionType>(null);
    min = ko.observable<number>(null).extend({ numeric: true });
    max = ko.observable<number>(null).extend({ numeric: true });
    mean = ko.observable<number>(null).extend({ numeric: true });
    stdev = ko.observable<number>(null).extend({ numeric: true });
    mu = ko.observable<number>(null).extend({ numeric: true });
    sigma = ko.observable<number>(null).extend({ numeric: true });

    distributionSelect = [null, "Uniform", "Normal", "LogNormal", "TruncatedNormal"];
    intervalSelect = [null, "Log", "Real"];
    variationSelect = [null, "Random", "Fixed", "Initial", "Multiple"];
}

export class SimulationMethod {
    public DisplayName: string;
    public isSelectable: boolean;

    public static RK547M: SimulationMethod = {
        DisplayName: 'Deterministic (RK547M)', isSelectable: true
    }

    public static GEAR: SimulationMethod = {
        DisplayName: 'Deterministic (Stiff BDF)', isSelectable: true
    }

    public static RKF54: SimulationMethod = {
        DisplayName: 'Deterministic (RKF54)', isSelectable: false
    }

    public static PDE: SimulationMethod = {
        DisplayName: 'PDE', isSelectable: true
    }

    public static StochasticDirect: SimulationMethod = {
        DisplayName: 'Stochastic (SSA)', isSelectable: true
    }

    public static StochasticJIT: SimulationMethod = {
        DisplayName: 'Stochastic (JIT)', isSelectable: false
    }

    public static CMEIntegration: SimulationMethod = {
        DisplayName: 'CME Integration', isSelectable: false
    }

    public static CMEIntegrationStiff: SimulationMethod = {
        DisplayName: 'CME Integration (Stiff)', isSelectable: false
    }

    public static LNA: SimulationMethod = {
        DisplayName: 'LNA', isSelectable: true
    }

    public static Sundials: SimulationMethod = {
        DisplayName: 'Sundials', isSelectable: true
    }

    public static CMESundials: SimulationMethod = {
        DisplayName: 'CMESundials', isSelectable: true
    }

    public static MC: SimulationMethod = {
        DisplayName: 'Moment Closure', isSelectable: false
    }
}

export class InferenceSettingsVM {
    constructor() {
        this.Name.subscribe(v => { if (this.source != null) this.source.name = v; });
        this.Burnin.subscribe(v => { if (this.source != null) this.source.burnin = v; });
        this.Samples.subscribe(v => { if (this.source != null) this.source.samples = v; });
        this.Thin.subscribe(v => { if (this.source != null) this.source.thin = v; });
        this.Seed.subscribe(v => { if (this.source != null) this.source.seed = v; });
        this.NoiseParameter.subscribe(v => {
            if (this.source != null) {
                if (v == "Fixed")
                    this.source.noise_parameter = { Fixed: this.NoiseParameterValue() };
                else if (v == "Random")
                    this.source.noise_parameter = "Random";
                else if (v == "Multiple")
                    this.source.noise_parameter = "Multiple";
            }
        });
        this.NoiseParameterValue.subscribe(v => {
            if (this.source != null && (<any>this.source.noise_parameter).Fixed != null) {
                (<any>this.source.noise_parameter).Fixed = v;
            }
        });
        this.NoiseModel.subscribe(v => { if (this.source != null) this.source.noise_model = v; });
        this.Prune.subscribe(v => { if (this.source != null) this.source.prune = v; });
        this.Timer.subscribe(v => { if (this.source != null) this.source.timer = v; });
        this.PartialEvaluation.subscribe(v => { if (this.source != null) this.source.partial_evaluation = v; });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Inference_settings;
    public get source(): WGI.Microsoft.Research.CRNEngine.Inference_settings { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Inference_settings) {
        this._source = source;
        this.Name(source == null ? "default" : source.name);
        this.Burnin(source == null ? 100 : source.burnin);
        this.Samples(source == null ? 100 : source.samples);
        this.Thin(source == null ? 10 : source.thin);
        this.Seed(source == null ? 0 : source.seed);
        this.NoiseParameter(source == null ? "Random" : source.noise_parameter == "Random" ? "Random" : source.noise_parameter == "Multiple" ? "Multiple" : "Fixed");
        this.NoiseParameterValue(source == null || (<any>source.noise_parameter).Fixed == null ? 0 : (<any>source.noise_parameter).Fixed);
        this.NoiseModel(source == null ? "Constant" : source.noise_model);
        this.Prune(source == null ? false : source.prune);
        this.Timer(source == null ? false : source.timer);
        this.PartialEvaluation(source == null ? false : source.partial_evaluation);
    }
    Name = ko.observable<string>("default");
    Burnin = ko.observable<number>(100).extend({ numeric: true });
    Samples = ko.observable<number>(100).extend({ numeric: true });
    Thin = ko.observable<number>(10).extend({ numeric: true });
    Seed = ko.observable<number>(0).extend({ numeric: false });
    NoiseParameter = ko.observable<string>("Random"); // if fixed, then it should be { $: 0, Fixed: (value) }. Otherwise, it should be a string, either "Random" or "Multiple".
    NoiseParameterValue = ko.observable<number>(0).extend({ numeric: true });
    NoiseModel = ko.observable<WGI.Microsoft.Research.CRNEngine.Noise_model>("Constant");
    Prune = ko.observable<boolean>(false);
    Timer = ko.observable<boolean>(false);
    Simulator = ko.observable<WGI.Microsoft.Research.CRNEngine.Simulator>("Oslo");
    PartialEvaluation = ko.observable<boolean>(false);
    PrintSummary = ko.observable<boolean>(false);

    NoiseParameterOptions = ['Fixed', 'Random', 'Multiple'];
    NoiseModelOptions = ['Constant', 'Proportional'];
    SimulatorOptions = ['Oslo', 'SSA', 'CME', 'LNA'];
}

export class CRNSettingsVM {
    constructor() {
        this.Sweeps.subscribe(v => { if (this.source != null) this.source.sweeps = v.map(p => p.source); });
        this.Rates.subscribe(v => { if (this.source != null) this.source.rates = v; });
        this.Parameters.subscribe(v => { if (this.source != null) this.source.parameters = v.map(p => p.source); });
        this.Data.subscribe(v => { if (this.source != null) this.source.data = v.map(p => p.source); });
        this.ChosenSimulation.subscribe(v => {
            if (this.source != null)
                switch (v) {
                    case SimulationMethod.GEAR:
                    case SimulationMethod.RK547M:
                    case SimulationMethod.RKF54:
                        this.source.simulator = "Oslo";
                        break;
                    case SimulationMethod.CMEIntegration:
                    case SimulationMethod.CMEIntegrationStiff:
                        this.source.simulator = "CME";
                        break;
                    case SimulationMethod.StochasticDirect:
                        this.source.simulator = "SSA";
                        break;
                    case SimulationMethod.LNA:
                        this.source.simulator = "LNA";
                        break;
                    case SimulationMethod.PDE:
                        this.source.simulator = "PDE";
                        break;
                    case SimulationMethod.Sundials:
                        this.source.simulator = "Sundials";
                        break;
                    case SimulationMethod.MC:
                        this.source.simulator = "MC";
                        break;
                    case SimulationMethod.CMESundials:
                        this.source.simulator = "CMESundials";
                        break;
                    default:
                        throw "Unexpected choice";
                }
        });
    }
    private _source: WGI.Microsoft.Research.CRNEngine.Crn_settings<string>;
    public get source(): WGI.Microsoft.Research.CRNEngine.Crn_settings<string> { return this._source; }
    public set source(source: WGI.Microsoft.Research.CRNEngine.Crn_settings<string>) {
        this._source = source;
        this.SSA.source = source == null ? null : source.stochastic;
        this.ODE.source = source == null ? null : source.deterministic;
        this.PDE.source = source == null ? null : source.spatial;
        this.Sim.source = source == null ? null : source.simulation;
        this.MomentClosure.source = source == null ? null : source.moment_closure;
        this.Synthesis.source = source == null ? null : source.synthesis;
        this.Sims(source == null ? [] : source.simulations.map(simsource => {
            var ret = new CRNSimSettings();
            ret.source = simsource;
            return ret;
        }));
        this.Sweeps(source == null ? [] : source.sweeps.map(simsweep => new CRNSweep(simsweep)));
        this.Units.source = source == null ? null : source.units;
        this.PlotSettings.source = source == null ? null : source.plot;
        this.Inference.source = source == null ? null : source.inference;
        this.Rates(source == null ? {} : source.rates);
        this.Parameters(source == null ? [] : source.parameters.map(simpar => new ParameterVM(simpar)));
        this.Data(source == null ? [] : source.data.map(simdata => new DataSet(simdata)));

        switch (source == null ? "Oslo" : source.simulator) {
            case "Oslo":
                if (source != null && source.deterministic.stiff)
                    this.ChosenSimulation(SimulationMethod.GEAR);
                else
                    this.ChosenSimulation(SimulationMethod.RK547M);
                break;
            case "CME":
                if (source != null && source.deterministic.stiff)
                    this.ChosenSimulation(SimulationMethod.CMEIntegrationStiff);
                else
                    this.ChosenSimulation(SimulationMethod.CMEIntegration);
                break;
            case "SSA":
                this.ChosenSimulation(SimulationMethod.StochasticDirect);
                break;
            case "LNA":
                this.ChosenSimulation(SimulationMethod.LNA);
                break;
            case "PDE":
                this.ChosenSimulation(SimulationMethod.PDE);
                break;
            case "Sundials":
                this.ChosenSimulation(SimulationMethod.Sundials);
                break;
            case "CMESundials":
                this.ChosenSimulation(SimulationMethod.CMESundials);
                break;
            case "MC":
                this.ChosenSimulation(SimulationMethod.MC);
                break;
            default:
                throw "Unexpected simulation type";
        }
    }
    Rates = ko.observable<{ [key: string]: string }>({});
    SSA = new CRNStochasticSettings();
    ODE = new CRNDeterministicSettings();
    PDE = new CRNPDESettings();
    MomentClosure = new CRNMomentClosureSettings();
    Synthesis = new CRNSynthesisSettings();
    Sim = new CRNSimSettings();
    Sims = ko.observable<CRNSimSettings[]>([]);
    Sweeps = ko.observableArray<CRNSweep>([]);
    Inference = new InferenceSettingsVM();
    Parameters = ko.observableArray<ParameterVM>([]);
    FocusedSweep: KnockoutObservable<CRNSweep> = ko.observable<CRNSweep>(null);
    Data = ko.observableArray<DataSet>([]);
    Units = new CRNUnits();
    PlotSettings = new CRNPlotSettings();
    PossibleKinetics = new Array<string>('Deterministic', 'Stochastic', 'Contextual');
    PossibleMethods: Array<SimulationMethod> = [
        SimulationMethod.RK547M,
        SimulationMethod.GEAR,
        SimulationMethod.RKF54,
        SimulationMethod.PDE,
        SimulationMethod.StochasticDirect,
        SimulationMethod.StochasticJIT,
        SimulationMethod.CMEIntegration,
        SimulationMethod.CMEIntegrationStiff,
        SimulationMethod.LNA,
        SimulationMethod.Sundials,
        SimulationMethod.CMESundials,
        SimulationMethod.MC
    ];
    ChosenSimulation = ko.observable<SimulationMethod>(this.PossibleMethods[0]);
    ChosenSimulationName = ko.pureComputed<string>({
        read: () => this.ChosenSimulation().DisplayName,
        write: (value: any) => {
            var len = this.PossibleMethods.length;
            for (var i = 0; i < len; i++)
                if (this.PossibleMethods[i].DisplayName == value) {
                    this.ChosenSimulation(this.PossibleMethods[i]);
                    return;
                }
            throw "Option is not found. This can not happen. check logic";
        }
    });
    PossiblePDEBoundaryConditions = WGI.Microsoft.Research.CRNEngine.BoundarySelect; // new Array<string>('Periodic', 'ZeroFlux');

    public reset(): void {
        this.source = null;
        this.Parameters([]);
        this.FocusedSweep(null);
        this.ChosenSimulation(this.PossibleMethods[0]);
    }

    selectedParameter = ko.observable<ParameterVM>(null);

    ODEMethodSelected = ko.computed<boolean>({
        owner: this,
        read: () => {
            var res = this.PossibleMethods.slice(0, 2).indexOf(this.ChosenSimulation()) > -1;
            return res;
        }
    });

    PDEMethodSelected = ko.computed<boolean>({
        owner: this,
        read: () => {
            var res = this.PossibleMethods.slice(3, 4).indexOf(this.ChosenSimulation()) > -1;
            return res;
        }
    });


    SSAMethodSelected = ko.computed(() => {
        return this.PossibleMethods.slice(4, 6).indexOf(this.ChosenSimulation()) > -1;
    })

    CMEMethodSelected = ko.computed(() => {
        return this.PossibleMethods.slice(7, 8).indexOf(this.ChosenSimulation()) > -1;
    })

    AddParameter() {
        this.Parameters.push(new ParameterVM());
    }

    RemoveParameter = () => {
        this.Parameters.remove(this.selectedParameter());
    }

    ActivateSweep = (sweep: CRNSweep) => {
        this.FocusedSweep(sweep);
    }
    DeactivateSweep = (sweep: CRNSweep) => {
        this.FocusedSweep(null);
    }
    AddSweep = () => {
        var sweep = new CRNSweep();
        sweep.Name("One more sweep");
        var ass = new CRNAssignments();
        sweep.Assignments.push(ass);
        this.Sweeps.push(sweep);
    }
    DeleteSweep = () => {
        this.Sweeps.remove(this.FocusedSweep());
    }

    FocusedAssignments: KnockoutObservable<CRNAssignments> = ko.observable<CRNAssignments>(null);
    IsAnyAssignmentFocused: KnockoutObservable<boolean> = ko.pureComputed<boolean>(() => {
        return this.FocusedAssignments() == null ? false : true;
    }, this);
    IsAssignmentDeletable: KnockoutObservable<boolean> = ko.pureComputed<boolean>(() => {
        let fs = this.FocusedSweep();
        if (fs) {
            return (this.IsAnyAssignmentFocused() && (fs.Assignments().length > 1));
        } else
            return false;
    }, this);
    ActivateAssignments = (assignments: CRNAssignments) => {
        this.FocusedAssignments(assignments);
    }
    DeactivateAssignments = (assignments: CRNAssignments) => {
        this.FocusedAssignments(null);
    }
    AddVariable = () => {
        this.FocusedSweep().Assignments.push(new CRNAssignments());
    }
    DeleteVariable = () => {
        this.FocusedSweep().Assignments.remove(this.FocusedAssignments());
    }
}
