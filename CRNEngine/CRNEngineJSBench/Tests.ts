// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// var Papa: any;
declare var Papa: any;

class Tests {
    private static program_simulate_oslo_am = `directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator deterministic
directive deterministic { reltolerance=1e-5 }

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20`;


    private static program_simulate_oslo_am_sundials = `directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator sundials
directive deterministic { reltolerance=1e-5 }

X + Y ->{r} X + B |
Y + X ->{r} Y + B |
X + B ->{r} X + X |
Y + B ->{r} Y + Y |

init X 30 |
init Y 20`;

    private static program_simulate_oslo_am_functional = `directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator deterministic

X + Y ->[r * [X] * [Y]] X + B |
Y + X ->[r * [X] * [Y]] Y + B |
X + B ->[r * [X] * [B]] X + X |
Y + B ->[r * [B] * [Y]] Y + Y |

init X 30 |
init Y 20
`;

    private static program_simulate_oslo_am_sundials_functional = `directive simulation { final=1.0; points=300 }
directive parameters [r = 1.0]
directive simulator sundials

X + Y ->[r * [X] * [Y]] X + B |
Y + X ->[r * [X] * [Y]] Y + B |
X + B ->[r * [X] * [B]] X + X |
Y + B ->[r * [B] * [Y]] Y + Y |

init X 30 |
init Y 20
`;

    private static program_simulate_oslo_waves = `directive simulation {final=20000; plots=[SpeciesL; SpeciesR]; }
directive simulator deterministic
directive deterministic {reltolerance=1e-7}
 
| constant 1000 UnaryRx
| constant 1000 UnaryLxLL
| constant 100000 UnaryLxLL_1
| 1000 SpeciesR
| 1000 SpeciesL
| constant 30000 BinaryLRxRR
| constant 3000000 BinaryLRxRR_1
| UnaryLxLL + SpeciesL ->{0.00001} sp10 + sp9
| UnaryRx + SpeciesR ->{0.00001} sp8 + sp7
| BinaryLRxRR + SpeciesL <->{0.00001}{0.1} sp12
| sp12 + SpeciesR ->{0.00001} sp6 + sp5
| BinaryLRxRR + SpeciesR <->{0.00001}{0.1} sp11
| sp11 + SpeciesL ->{0.00001} sp6 + sp5
| UnaryLxLL_1 + sp9 ->{0.00001} sp4 + sp3 + 2SpeciesL
| BinaryLRxRR_1 + sp5 ->{0.00001} sp2 + sp1 + 2SpeciesR`

    private static program_simulate_sundials_waves = `directive simulation {final=20000; plots=[SpeciesL; SpeciesR]; }
directive simulator sundials
directive deterministic {reltolerance=1e-7}
 
| constant 1000 UnaryRx
| constant 1000 UnaryLxLL
| constant 100000 UnaryLxLL_1
| 1000 SpeciesR
| 1000 SpeciesL
| constant 30000 BinaryLRxRR
| constant 3000000 BinaryLRxRR_1
| UnaryLxLL + SpeciesL ->{0.00001} sp10 + sp9
| UnaryRx + SpeciesR ->{0.00001} sp8 + sp7
| BinaryLRxRR + SpeciesL <->{0.00001}{0.1} sp12
| sp12 + SpeciesR ->{0.00001} sp6 + sp5
| BinaryLRxRR + SpeciesR <->{0.00001}{0.1} sp11
| sp11 + SpeciesL ->{0.00001} sp6 + sp5
| UnaryLxLL_1 + sp9 ->{0.00001} sp4 + sp3 + 2SpeciesL
| BinaryLRxRR_1 + sp5 ->{0.00001} sp2 + sp1 + 2SpeciesR`

    private static program_infer_oslo_am = `directive inference  { burnin = 100; samples = 1000; thin = 100 }
    directive simulation { final=1.0; points=1000; plots=[B; Y; X] }
    directive simulator deterministic
    directive deterministic { reltolerance=1e-5 }
    directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]

    directive sweeps [dummySweep = [NotUsed = [0.0]]]
    directive data [AM_obs_noised]

    X + Y ->{r} X + B |
    Y + X ->{r} Y + B |
    X + B ->{r} X + X |
    Y + B ->{r} Y + Y |

    init X 30 |
    init Y 20 |
    init B 0`

    private static program_infer_sundials_am = `directive inference  { burnin = 100; samples = 1000; thin = 100 }
    directive simulation { final=1.0; points=1000; plots=[B; Y; X] }
    directive simulator sundials
    directive deterministic { reltolerance=1e-5 }
    directive parameters [r = 0.2,  { interval=Log; distribution=Uniform(1e-2,1e2); variation=Random }]

    directive sweeps [dummySweep = [NotUsed = [0.0]]]
    directive data [AM_obs_noised]

    X + Y ->{r} X + B |
    Y + X ->{r} Y + B |
    X + B ->{r} X + X |
    Y + B ->{r} Y + Y |

    init X 30 |
    init Y 20 |
    init B 0`

    private static observation_am_noised = `"Time","B","Y","X"
0,-0.536442494054767,20.1373542622727,30.1161661735444
2.5832472250925e-05,0.273683484284328,19.7997801844766,29.8019012135649
0.000129995666811106,-0.240872977615516,19.6402344174689,29.8743580140588
0.000613844249587313,0.146088271783824,19.6979121828816,29.2254996339609
0.00172098048782427,1.89504673906316,19.1267021820755,28.9229200371584
0.00397595771840628,4.21729340199145,18.3511437445258,28.4022066948249
0.00749234953349376,6.47887694926602,17.044755591736,27.1554977869724
0.012714742384666,9.02575125690993,14.5832634805908,25.8146583909142
0.0197077212439884,10.9344095472759,14.1760524145535,25.4894551306004
0.0288889549674744,12.6318734928114,11.9387715696896,24.4392617958623
0.0403318650444065,14.0865694522959,10.0466404447329,25.5239502094261
0.0543383823497538,14.7415991335299,9.22829946099198,26.5537426238476
0.070790408895852,12.9325194988848,6.94399320758205,29.5859864254051
0.0896152301812983,11.4127533188314,4.63469182859594,33.4321852638218
0.1096400568918,8.69031607697796,2.81401431951398,38.1216980010054
0.129401832618169,6.34384876415987,1.5226579496848,42.8304895341807
0.147781645423376,3.05440732046255,0.530830431809112,45.2104250175944
0.165174841743392,1.62153494129729,0.337960439117223,47.1512215787111
0.181972427739939,1.18106517508693,-0.363780750610002,48.3037621433809
0.197404747473022,1.49759157688198,0.111005441750795,49.071769312701
0.212457829747985,-0.245343279440953,-0.021944415729766,48.7463465918768
0.226898111392983,0.377853898468835,-0.230345805321965,50.037586113185
0.241158153131114,-0.00277529643408969,0.0602118323887257,50.4214469852185
0.2551479284319,-0.297811297564077,-0.487907543706742,50.4410978597849
0.269059321958377,0.111344750357648,0.156199277926499,49.4772552798094
0.282853328036658,-0.109374181071146,0.0370003812710531,49.8750894772398
0.296615194756094,-0.146153783876394,-0.13584641061469,50.2782292711912
0.310326801473583,-0.544929100902651,0.0687247940639225,50.0376178749607
0.325813802489516,-0.402962591803518,-0.0377118512332322,49.5020419892715
0.340802145768734,-0.0885625962777099,-0.0918606115426472,50.2944772641347
0.356218908664103,0.350278369915333,0.00884356432920278,49.9062494137911
0.371318846044973,-0.101695179138524,-0.230805464625233,50.2947476217737
0.38773741682253,0.410221818280297,-0.0793548312482712,49.9332741112813
0.405730392776637,0.41144519989057,0.160149101697547,49.7919420762886
0.426062153103297,0.278974983459989,-0.247384747025995,50.1449525748227
0.449303322086248,-0.244441078848971,-0.039837046904541,50.2264969211626
0.476627769574023,-0.290355199218827,-0.0984797371933064,49.5767954175402
0.509443449577963,-0.00164094790673284,0.011539693777374,49.7145988304887
0.550366270470462,-0.225564345600905,-0.12968680932527,50.0541458303811
0.603644454157268,0.165516978962229,0.0614737897435394,50.509240885318
0.678504586664917,0.0304603502421764,-0.447604408145629,50.1733129855888
0.800311075164844,0.00955861625499169,0.197326774297993,49.9466026039313
0.953769650721097,0.0300651292121575,-0.0635008698799478,50.3021857493972
0.999999999999,0.226519001079287,0.154830213817479,49.8867858697654
1,0.291295831470984,0.240153330171843,49.8378932835209`

    private static ParseFile(obsData: string): any {
        //see CrnDataSets.ts of HTML5CRN project
        var parsed = Papa.parse(obsData);
        var fields: string[] = parsed.data[0];
        // Eliminate the results of parsing blank rows.
        var sdata: string[][] = parsed.data.slice(1).filter((arr: any) => arr != null && arr.length > 0 && arr[0] != null && arr[0] != "");
        // Convert values from string to number.
        var data: any[] = sdata.map(d => d.map(parseFloat));
        var len = data.length
        return {
            ColumnNames: fields,
            Data: data
        };
    }

    private static ConvertTable(initialData: any) {
        var transposedData: number[][] = [];
        var rawdata = initialData.Data;
        var rowCount = rawdata.length;
        var colCount = rawdata[0].length;

        for (var i = 0; i < colCount; i++)
            transposedData[i] = [];
        for (var i = 0; i < rowCount; i++)
            for (var j = 0; j < colCount; j++)
                transposedData[j][i] = rawdata[i][j];

        var time = transposedData[0];
        var result: { file: string, data: any[] } = {
            file: name,
            data: []
        }

        result.data.push({
            times: time,
            columns: []
        });

        for (var i = 1; i < initialData.ColumnNames.length; i++) {
            var name = initialData.ColumnNames[i];
            var res = transposedData[i];
            var column = {
                name: name,
                values: res
            }
            
            result.data[0].columns.push(column);
        }
        return result;
    }

    private static run_crn_oslo_simulate(program_text: any): void {
        let crn = CrnEngine.JSAPI.parse_code(program_text);
        let ode = crn.top.to_ode();
        let oslo = ode.to_oslo(ode);
        let results = oslo.simulate();
    }

    private static run_crn_sundials_simulate(program_text: any): void {
        let crn = CrnEngine.JSAPI.parse_code(program_text);
        let ode = crn.top.to_ode();
        let sundials = ode.to_sundials(ode);
        let results = sundials.simulate();
    }

    private static run_crm_inference(program_text: any): void {
        let crn = CrnEngine.JSAPI.parse_code(program_text);

        let crn_encoded = CrnEngine.JSONAPI.encodeGui(crn);        
        let dataset = Tests.ParseFile(Tests.observation_am_noised);        
        let dataset2 = Tests.ConvertTable(dataset);
        dataset2.file = "AM_obs_noised";
        crn_encoded.top.settings.data = [dataset2];
        var crn_decoded = CrnEngine.JSONAPI.decodeGui(crn_encoded);

        crn.top.settings.data = crn_decoded.top.settings.data;

        crn.top.settings.inference.print_summary = false;

        crn.top.infer();
    }

    private static run_crm_inference_10000(program_text: any): void {
        let crn = CrnEngine.JSAPI.parse_code(program_text);

        let crn_encoded = CrnEngine.JSONAPI.encodeGui(crn);
        let dataset = Tests.ParseFile(Tests.observation_am_noised);
        let dataset2 = Tests.ConvertTable(dataset);
        dataset2.file = "AM_obs_noised";
        crn_encoded.top.settings.data = [dataset2];
        var crn_decoded = CrnEngine.JSONAPI.decodeGui(crn_encoded);

        crn.top.settings.data = crn_decoded.top.settings.data;
        crn.top.settings.inference.samples = 10000;
        crn.top.settings.inference.print_summary = false;

        crn.top.infer();
    }


    static Simulate_Oslo_AM() { Tests.run_crn_oslo_simulate(Tests.program_simulate_oslo_am) };
    static Simulate_Oslo_Waves() { Tests.run_crn_oslo_simulate(Tests.program_simulate_oslo_waves) };
    static Simulate_Oslo_AM_functional() { Tests.run_crn_oslo_simulate(Tests.program_simulate_oslo_am_functional) };
    static Simulate_Sundials_AM() { Tests.run_crn_sundials_simulate(Tests.program_simulate_oslo_am_sundials) };
    static Simulate_Sundials_Waves() { Tests.run_crn_sundials_simulate(Tests.program_simulate_sundials_waves) };
    static Simulate_Sundials_AM_functional() { Tests.run_crn_sundials_simulate(Tests.program_simulate_oslo_am_sundials_functional) };
    static Infer_Oslo_AM_1000() { Tests.run_crm_inference(Tests.program_infer_oslo_am) };
    static Infer_Sundials_AM_1000() { Tests.run_crm_inference(Tests.program_infer_sundials_am) };
    static Infer_Oslo_AM_10000() { Tests.run_crm_inference_10000(Tests.program_infer_oslo_am) };
    static Infer_Sundials_AM_10000() { Tests.run_crm_inference_10000(Tests.program_infer_sundials_am) };
}