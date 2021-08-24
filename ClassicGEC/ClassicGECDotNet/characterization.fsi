// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module GEC.Characterization

open System.Collections.Generic

type GrowthModels = Gompertz =0| Richards =1 | Logistic =2
type GrowthModelType = {
    mu: float;
    lag: float;
    tm: float;
    K: float;
    res: int;
    model: (int * float -> float); 
}
type DataTypes = Data | Gain | Blank | Negative
type Well = {
    Row: char;
    Col: int;
    Content: string;
    Colony: string;
    DataType: DataTypes;
    Condition: Map<string,double>;
    Signals: double[][];
    SignalNames: string[];
    T: double[];
    GrowthModel: GrowthModelType option;
    Filter: int -> bool; //given an signal array index, is this part of the selection
    SteadyStates: Map<(string*string),(double*double)>;
    Activity: Map<(string*string),double>;
}

type Plate = {
    Name: string;
    Data: seq<Well>;
    Blanks: Map<string,double>;

    Conditions: Map<string,double[]>;
    Cname: string;
    C: double[]; //conditions
    Properties: double[][][]; //computed or measured properties
    PropertyNames: string[];    //name of properties

    Gain: Map<string*string,double>;
    log: string;
}

type ModelTypes = 
    | Constitutive = 0
    | Linear = 1
    | Michaelis_Menten_Activation = 2
    | Michaelis_Menten_Repression = 3
    | Michaelis_Menten_General = 4
    | Dimer_Activation = 5
    | Dimer_Repression =6
    | Dimer_General = 7
    | Hill_Activation = 8
    | Hill_Repression = 9
    | Hill_General = 10 
    | Sequential_Binding = 11

type ResponseTypes = 
    | EYFP_ECFP = 0
    | EYFP_OD = 1
    | ECFP_OD = 2
    | mu = 3
    | K = 4
    | lag = 5
    | EYFP_mRFP1 = 6
    | ECFP_mRFP1 = 7
    | mRFP1_OD = 8

type Parameter = {
    pname: string;
    value: double;
    dist: double[];
    log: bool;
}

type Model = {
    modelType: ModelTypes;
    AIC: double;
    BIC: double;
    CIC: double; //rename this information criterion to the right abbreviation
    model: int * double -> double;
    res: int;
    parameters: Parameter[];
    X: double[];
    bayestable: List<double[]>;
}

val string_of_response: ResponseTypes -> string
val LoadPlate: string -> Plate
val getSignal: Well * string -> double[] * double[] //return signal over time
val getSignals: Well * string * string -> double[] * double[] //return signal over time
val getFluorescence: Well -> double[] * seq<string*double[]>
val getFOd: Well -> double[] * seq<string*double[]>
val getFF: Well*string -> double[] * seq<string*double[]>

val Characterize: Plate*GrowthModels -> Plate
val ComputeProperties: string -> seq<string*double> -> Plate -> Plate
val FilterByDevice : Plate -> string -> Plate
val getKeys: Map<'a,'b> -> seq<'a>
val CharacterizeModel: seq<double*double[]>*ModelTypes -> Model
val getProperties: Plate*string -> seq<double*double[]>
val MergePlates: Plate * Plate -> Plate
val DataOutput: Plate -> string
val EmptyPlate: Plate