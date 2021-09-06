// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Oslo.FSharp

open Oslo
open Oslo.GearBDF
open System

[<JavaScript>]
module GearSamples =

    let private mkVec1 x =
        let v = Vector.zeros 1
        v.[0] <- x
        v

    let Exponent t0 t1 =
        let t0 = 0.0
        let x0 = mkVec1 1000.0
        let f (t: double) (x: Vector) : Vector = mkVec1 (-0.01 * x.[0])
        let gear = init t0 x0 f  { defaults() with MaxStep = 5.0 }

        let rec solve (gear : state, t1) = 
            let gear2 = advance gear
            if gear2.t > t1 
            then []
            else (gear2.t, gear2.x.[0]) :: solve(gear2, t1)

        let points = (gear.t, gear.x.[0]) :: solve(gear,t1)
        let res = (points |> List.map (fun (t,_) -> t) |> List.toArray, 
                   points |> List.map (fun (_,x) -> x) |> List.toArray,
                   points |> List.map (fun (t,_) -> 1000.0 * Math.Exp(-0.01 * t)) |> List.toArray)
        res

    let Trigonometric t0 t1 =
        let t0 = 0.0
        let x0 = mkVec1 1.0
        let f (t: double) (x: Vector) = mkVec1 (- sin t)
        let gear = init t0 x0 f { defaults() with MaxStep = 0.1 }

        let rec solve (gear : state, t1) = 
            let gear2 = advance gear
            if gear2.t > t1 
            then []
            else (gear2.t, gear2.x.[0]) :: solve(gear2, t1)

        let points = (gear.t, gear.x.[0]) :: solve(gear,t1)
        let res = (points |> List.map (fun (t,_) -> t) |> List.toArray, points |> List.map (fun (_,x) -> x) |> List.toArray)
        res

    let private mkVec3 a b c =
        let v = Vector.zeros 3
        v.[0] <- a
        v.[1] <- b
        v.[2] <- c
        v

    // Solves Oreginator dynamic system on time segment [t0,t1]
    //
    // dy[0] / dt = 77.27d * (y[1] + y[0] * (1 - 8.375d * 1e-6d * y[0] - y[1])),
    // dy[1] / dt = 1 / 77.27d * (y[2] - (1 + y[0]) * y[1]),
    // dy[2] / dt = 0.161d * (y[0] - y[2])
    //
    // Returns tuple with four 1D arrays: t,x1,x2,x3
    // Note: this sample uses recursion that is not translated well by WebSharper 2.4
    // Large time intervals may cause stack overflows
    let Oregonator t0 t1 =

      let x0 = mkVec3 0.001 0.004 0.0002 // Initial point

      let f (t:double) (x: Vector) = // Oreginator's right part
        mkVec3
            (77.27 * (x.[1] + x.[0] * (1.0 - 8.375 * 1e-6 * x.[0] - x.[1])))
            (1.0 / 77.27 * (x.[2] - (1.0 + x.[0]) * x.[1]))
            (0.161 * (x.[0] - x.[2]))

      let gear = init t0 x0 f { defaults() with MaxStep = (t1 - t0) * 0.01 }

      let rec solve (gear : state, t1) = 
        let gear2 = advance gear
        if gear2.t > t1 
        then []
        else (gear2.t, gear2.x.[0], gear2.x.[1], gear2.x.[2]) :: solve(gear2, t1)

      let points = (gear.t, gear.x.[0], gear.x.[1], gear.x.[2]) :: solve(gear,t1)
      
      (points |> List.map (fun (t,_,_,_) -> t) |> List.toArray, 
       points |> List.map (fun (_,x1,_,_) -> x1) |> List.toArray, 
       points |> List.map (fun (_,_,x2,_) -> x2) |> List.toArray,
       points |> List.map (fun (_,_,_,x3) -> x3) |> List.toArray)

    let private setVec (v: Vector) (i: int) (d: double) =
        v.[i] <- d

    let oscillating_ics =
        let n = 187
        let x0 = Vector.zeros n
        setVec x0 1 1.0
        setVec x0 2 100.0
        setVec x0 3 100.0
        setVec x0 4 100.0
        setVec x0 5 100.0
        setVec x0 6 100.0
        setVec x0 7 100.0
        setVec x0 8 100.0
        setVec x0 14  1.0
        setVec x0 15 100.0
        setVec x0 16 100.0
        setVec x0 17 100.0
        setVec x0 18 100.0
        setVec x0 19 100.0
        setVec x0 20 100.0
        setVec x0 21 100.0
        setVec x0 27 1.0
        setVec x0 28 100.0
        setVec x0 29 100.0
        setVec x0 30 100.0
        setVec x0 31 100.0
        setVec x0 32 100.0
        setVec x0 33 100.0
        setVec x0 34 100.0
        setVec x0 40 2.0
        setVec x0 44 2.0
        setVec x0 76 3.0
        x0

    let oscillating_derivs (t: double) (x: Vector) : Vector =
        // Assign states (concentrations)
        let IGNORE = x.[0]
        let BJ2x2_23 = x.[1]
        let BJ2x2_22 = x.[2] 
        let BJ2x2_21 = x.[3] 
        let BJ2x2_20 = x.[4] 
        let BJ2x2_19 = x.[5] 
        let BJ2x2_18 = x.[6] 
        let BJ2x2_17 = x.[7] 
        let BJ2x2_16 = x.[8] 
        let sp_3 = x.[9] 
        let sp_5 = x.[10] 
        let sp_4 = x.[11] 
        let sp_7 = x.[12] 
        let sp_6 = x.[13] 
        let BJ2x2_15 = x.[14] 
        let BJ2x2_14 = x.[15] 
        let BJ2x2_13 = x.[16] 
        let BJ2x2_12 = x.[17] 
        let BJ2x2_11 = x.[18] 
        let BJ2x2_10 = x.[19] 
        let BJ2x2_9 = x.[20] 
        let BJ2x2_8 = x.[21] 
        let sp_8 = x.[22] 
        let sp_10 = x.[23] 
        let sp_9 = x.[24] 
        let sp_12 = x.[25] 
        let sp_11 = x.[26] 
        let BJ2x2_7 = x.[27] 
        let BJ2x2_6 = x.[28] 
        let BJ2x2_5 = x.[29] 
        let BJ2x2_4 = x.[30] 
        let BJ2x2_3 = x.[31] 
        let BJ2x2_2 = x.[32] 
        let BJ2x2_1 = x.[33] 
        let BJ2x2 = x.[34] 
        let sp_13 = x.[35] 
        let sp_15 = x.[36] 
        let sp_14 = x.[37] 
        let sp_17 = x.[38] 
        let sp_16 = x.[39] 
        let sp_2 = x.[40] 
        let sp_18 = x.[41] 
        let sp_20 = x.[42] 
        let sp_19 = x.[43] 
        let sp_1 = x.[44] 
        let sp_21 = x.[45] 
        let sp_23 = x.[46] 
        let sp_24 = x.[47] 
        let sp_26 = x.[48] 
        let sp_29 = x.[49] 
        let sp_28 = x.[50] 
        let sp_31 = x.[51] 
        let sp_32 = x.[52] 
        let sp_30 = x.[53] 
        let sp_33 = x.[54] 
        let sp_34 = x.[55] 
        let sp_27 = x.[56] 
        let sp_36 = x.[57] 
        let sp_37 = x.[58] 
        let sp_35 = x.[59] 
        let sp_38 = x.[60] 
        let sp_39 = x.[61] 
        let sp_25 = x.[62] 
        let sp_40 = x.[63] 
        let sp_42 = x.[64] 
        let sp_43 = x.[65] 
        let sp_45 = x.[66] 
        let sp_44 = x.[67] 
        let sp_47 = x.[68] 
        let sp_46 = x.[69] 
        let sp_41 = x.[70] 
        let sp_49 = x.[71] 
        let sp_48 = x.[72] 
        let sp_22 = x.[73] 
        let sp_51 = x.[74] 
        let sp_50 = x.[75] 
        let sp_0 = x.[76] 
        let sp_55 = x.[77] 
        let sp_57 = x.[78] 
        let sp_58 = x.[79] 
        let sp_60 = x.[80] 
        let sp_63 = x.[81] 
        let sp_62 = x.[82] 
        let sp_65 = x.[83] 
        let sp_66 = x.[84] 
        let sp_69 = x.[85] 
        let sp_70 = x.[86] 
        let sp_71 = x.[87] 
        let sp_68 = x.[88] 
        let sp_72 = x.[89] 
        let sp_73 = x.[90] 
        let sp_67 = x.[91] 
        let sp_74 = x.[92] 
        let sp_75 = x.[93] 
        let sp_64 = x.[94] 
        let sp_76 = x.[95] 
        let sp_77 = x.[96] 
        let sp_61 = x.[97] 
        let sp_79 = x.[98] 
        let sp_80 = x.[99] 
        let sp_83 = x.[100] 
        let sp_84 = x.[101] 
        let sp_85 = x.[102] 
        let sp_82 = x.[103] 
        let sp_86 = x.[104] 
        let sp_87 = x.[105] 
        let sp_81 = x.[106] 
        let sp_88 = x.[107] 
        let sp_89 = x.[108] 
        let sp_78 = x.[109] 
        let sp_90 = x.[110] 
        let sp_91 = x.[111] 
        let sp_59 = x.[112] 
        let sp_92 = x.[113] 
        let sp_94 = x.[114] 
        let sp_95 = x.[115] 
        let sp_97 = x.[116] 
        let sp_96 = x.[117] 
        let sp_99 = x.[118] 
        let sp_98 = x.[119] 
        let sp_93 = x.[120] 
        let sp_100 = x.[121] 
        let sp_56 = x.[122] 
        let sp_102 = x.[123] 
        let sp_101 = x.[124] 
        let sp_54 = x.[125] 
        let sp_104 = x.[126] 
        let sp_107 = x.[127] 
        let sp_106 = x.[128] 
        let sp_111 = x.[129] 
        let sp_112 = x.[130] 
        let sp_115 = x.[131] 
        let sp_116 = x.[132] 
        let sp_117 = x.[133] 
        let sp_114 = x.[134] 
        let sp_118 = x.[135] 
        let sp_119 = x.[136] 
        let sp_113 = x.[137] 
        let sp_120 = x.[138] 
        let sp_121 = x.[139] 
        let sp_110 = x.[140] 
        let sp_122 = x.[141] 
        let sp_123 = x.[142] 
        let sp_105 = x.[143] 
        let sp_127 = x.[144] 
        let sp_128 = x.[145] 
        let sp_131 = x.[146] 
        let sp_132 = x.[147] 
        let sp_133 = x.[148] 
        let sp_130 = x.[149] 
        let sp_134 = x.[150] 
        let sp_135 = x.[151] 
        let sp_129 = x.[152] 
        let sp_136 = x.[153] 
        let sp_137 = x.[154] 
        let sp_126 = x.[155] 
        let sp_138 = x.[156] 
        let sp_139 = x.[157] 
        let sp_103 = x.[158] 
        let sp_140 = x.[159] 
        let sp_142 = x.[160] 
        let sp_143 = x.[161] 
        let sp_145 = x.[162] 
        let sp_146 = x.[163] 
        let sp_144 = x.[164] 
        let sp_148 = x.[165] 
        let sp_147 = x.[166] 
        let sp_141 = x.[167] 
        let sp_149 = x.[168] 
        let sp_125 = x.[169] 
        let sp_150 = x.[170] 
        let sp_151 = x.[171] 
        let sp_124 = x.[172] 
        let sp_152 = x.[173] 
        let sp_153 = x.[174] 
        let sp_109 = x.[175] 
        let sp_154 = x.[176] 
        let sp_155 = x.[177] 
        let sp_108 = x.[178] 
        let sp_156 = x.[179] 
        let sp_157 = x.[180] 
        let sp_53 = x.[181] 
        let sp_158 = x.[182] 
        let sp_159 = x.[183] 
        let sp_52 = x.[184] 
        let sp_160 = x.[185] 
        let sp_161 = x.[186] 

        // Write out the reaction propensities
        let r_0 = 0.0003 * BJ2x2_17 * sp_160 
        let r_1 = 0.0003 * sp_103 * sp_160 
        let r_2 = 0.1126 * sp_52 
        let r_3 = 0.1126 * sp_52 
        let r_4 = 0.0003 * BJ2x2_17 * sp_158 
        let r_5 = 0.0003 * sp_103 * sp_158 
        let r_6 = 0.1126 * sp_53 
        let r_7 = 0.1126 * sp_53 
        let r_8 = 0.0003 * BJ2x2_17 * sp_156 
        let r_9 = 0.0003 * sp_103 * sp_156 
        let r_10 = 0.1126 * sp_108 
        let r_11 = 0.1126 * sp_108 
        let r_12 = 0.0003 * BJ2x2_17 * sp_154 
        let r_13 = 0.0003 * sp_103 * sp_154 
        let r_14 = 0.1126 * sp_109 
        let r_15 = 0.1126 * sp_109 
        let r_16 = 0.0003 * BJ2x2_17 * sp_152 
        let r_17 = 0.0003 * sp_103 * sp_152 
        let r_18 = 0.1126 * sp_124 
        let r_19 = 0.1126 * sp_124 
        let r_20 = 0.0003 * BJ2x2_17 * sp_150 
        let r_21 = 0.0003 * sp_103 * sp_150 
        let r_22 = 0.1126 * sp_125 
        let r_23 = 0.1126 * sp_125 
        let r_24 = 0.0003 * sp_141 * BJ2x2_21 
        let r_25 = 0.0003 * sp_141 * sp_142 
        let r_26 = 0.0003 * sp_144 * BJ2x2_22 
        let r_27 = 0.0003 * sp_144 * sp_145 
        let r_28 = 0.0003 * sp_19 * sp_145 
        let r_29 = 0.1126 * sp_143 
        let r_30 = 0.1126 * sp_143 
        let r_31 = 0.0003 * BJ2x2_20 * sp_142 
        let r_32 = 0.1126 * sp_140 
        let r_33 = 0.1126 * sp_140 
        let r_34 = 0.0003 * sp_103 * BJ2x2_19 
        let r_35 = 0.0003 * sp_103 * sp_104 
        let r_36 = 0.0003 * sp_103 * sp_122 
        let r_37 = 0.0003 * sp_103 * sp_138 
        let r_38 = 0.0003 * BJ2x2_17 * sp_138 
        let r_39 = 0.1126 * sp_126 
        let r_40 = 0.1126 * sp_126 
        let r_41 = 0.0003 * BJ2x2_1 * sp_136 
        let r_42 = 0.0003 * sp_59 * sp_136 
        let r_43 = 0.1126 * sp_129 
        let r_44 = 0.1126 * sp_129 
        let r_45 = 0.0003 * BJ2x2_1 * sp_134 
        let r_46 = 0.0003 * sp_59 * sp_134 
        let r_47 = 0.1126 * sp_130 
        let r_48 = 0.1126 * sp_130 
        let r_49 = 0.0003 * BJ2x2_1 * sp_132 
        let r_50 = 0.0003 * sp_59 * sp_132 
        let r_51 = 0.1126 * sp_131 
        let r_52 = 0.1126 * sp_131 
        let r_53 = 0.0003 * sp_1 * sp_128 
        let r_54 = 0.0003 * sp_62 * sp_128 
        let r_55 = 0.0003 * sp_61 * sp_128 
        let r_56 = 0.0003 * sp_56 * sp_128 
        let r_57 = 0.1126 * sp_127 
        let r_58 = 0.1126 * sp_127 
        let r_59 = 0.0003 * sp_105 * sp_15 
        let r_60 = 0.0003 * sp_105 * sp_20 
        let r_61 = 0.0003 * sp_105 * sp_32 
        let r_62 = 0.0003 * sp_105 * sp_37 
        let r_63 = 0.0003 * BJ2x2_17 * sp_122 
        let r_64 = 0.1126 * sp_110 
        let r_65 = 0.1126 * sp_110 
        let r_66 = 0.0003 * BJ2x2_1 * sp_120 
        let r_67 = 0.0003 * sp_59 * sp_120 
        let r_68 = 0.1126 * sp_113 
        let r_69 = 0.1126 * sp_113 
        let r_70 = 0.0003 * BJ2x2_1 * sp_118 
        let r_71 = 0.0003 * sp_59 * sp_118 
        let r_72 = 0.1126 * sp_114 
        let r_73 = 0.1126 * sp_114 
        let r_74 = 0.0003 * BJ2x2_1 * sp_116 
        let r_75 = 0.0003 * sp_59 * sp_116 
        let r_76 = 0.1126 * sp_115 
        let r_77 = 0.1126 * sp_115 
        let r_78 = 0.0003 * sp_1 * sp_112 
        let r_79 = 0.0003 * sp_62 * sp_112 
        let r_80 = 0.0003 * sp_61 * sp_112 
        let r_81 = 0.0003 * sp_56 * sp_112 
        let r_82 = 0.1126 * sp_111 
        let r_83 = 0.1126 * sp_111 
        let r_84 = 0.0003 * sp_106 * sp_15 
        let r_85 = 0.0003 * sp_106 * sp_20 
        let r_86 = 0.0003 * sp_106 * sp_32 
        let r_87 = 0.0003 * sp_106 * sp_37 
        let r_88 = 0.0003 * BJ2x2_17 * sp_104 
        let r_89 = 0.1126 * sp_54 
        let r_90 = 0.1126 * sp_54 
        let r_91 = 0.0003 * sp_56 * sp_57 
        let r_92 = 0.0003 * sp_56 * sp_97 
        let r_93 = 0.0003 * sp_93 * BJ2x2_5 
        let r_94 = 0.0003 * sp_93 * sp_94 
        let r_95 = 0.0003 * sp_96 * BJ2x2_6 
        let r_96 = 0.0003 * sp_96 * sp_97 
        let r_97 = 0.1126 * sp_95 
        let r_98 = 0.1126 * sp_95 
        let r_99 = 0.0003 * BJ2x2_4 * sp_94 
        let r_100 = 0.1126 * sp_92 
        let r_101 = 0.1126 * sp_92 
        let r_102 = 0.0003 * sp_59 * BJ2x2_3 
        let r_103 = 0.0003 * sp_59 * sp_60 
        let r_104 = 0.0003 * sp_59 * sp_76 
        let r_105 = 0.0003 * sp_59 * sp_90 
        let r_106 = 0.0003 * BJ2x2_1 * sp_90 
        let r_107 = 0.1126 * sp_78 
        let r_108 = 0.1126 * sp_78 
        let r_109 = 0.0003 * BJ2x2_9 * sp_88 
        let r_110 = 0.0003 * sp_25 * sp_88 
        let r_111 = 0.1126 * sp_81 
        let r_112 = 0.1126 * sp_81 
        let r_113 = 0.0003 * BJ2x2_9 * sp_86 
        let r_114 = 0.0003 * sp_25 * sp_86 
        let r_115 = 0.1126 * sp_82 
        let r_116 = 0.1126 * sp_82 
        let r_117 = 0.0003 * BJ2x2_9 * sp_84 
        let r_118 = 0.0003 * sp_25 * sp_84 
        let r_119 = 0.1126 * sp_83 
        let r_120 = 0.1126 * sp_83 
        let r_121 = 0.0003 * sp_2 * sp_80 
        let r_122 = 0.0003 * sp_28 * sp_80 
        let r_123 = 0.0003 * sp_27 * sp_80 
        let r_124 = 0.0003 * sp_22 * sp_80 
        let r_125 = 0.1126 * sp_79 
        let r_126 = 0.1126 * sp_79 
        let r_127 = 0.0003 * sp_61 * sp_10 
        let r_128 = 0.0003 * sp_61 * sp_57 
        let r_129 = 0.0003 * BJ2x2_1 * sp_76 
        let r_130 = 0.1126 * sp_64 
        let r_131 = 0.1126 * sp_64 
        let r_132 = 0.0003 * BJ2x2_9 * sp_74 
        let r_133 = 0.0003 * sp_25 * sp_74 
        let r_134 = 0.1126 * sp_67 
        let r_135 = 0.1126 * sp_67 
        let r_136 = 0.0003 * BJ2x2_9 * sp_72 
        let r_137 = 0.0003 * sp_25 * sp_72 
        let r_138 = 0.1126 * sp_68 
        let r_139 = 0.1126 * sp_68 
        let r_140 = 0.0003 * BJ2x2_9 * sp_70 
        let r_141 = 0.0003 * sp_25 * sp_70 
        let r_142 = 0.1126 * sp_69 
        let r_143 = 0.1126 * sp_69 
        let r_144 = 0.0003 * sp_2 * sp_66 
        let r_145 = 0.0003 * sp_28 * sp_66 
        let r_146 = 0.0003 * sp_27 * sp_66 
        let r_147 = 0.0003 * sp_22 * sp_66 
        let r_148 = 0.1126 * sp_65 
        let r_149 = 0.1126 * sp_65 
        let r_150 = 0.0003 * sp_62 * sp_10 
        let r_151 = 0.0003 * sp_62 * sp_57 
        let r_152 = 0.0003 * BJ2x2_1 * sp_60 
        let r_153 = 0.1126 * sp_58 
        let r_154 = 0.1126 * sp_58 
        let r_155 = 0.0003 * sp_1 * sp_57 
        let r_156 = 0.1126 * sp_55 
        let r_157 = 0.1126 * sp_55 
        let r_158 = 0.0003 * sp_0 * sp_15 
        let r_159 = 0.0003 * sp_0 * sp_20 
        let r_160 = 0.0003 * sp_0 * sp_32 
        let r_161 = 0.0003 * sp_0 * sp_37 
        let r_162 = 0.0003 * sp_22 * sp_23 
        let r_163 = 0.0003 * sp_22 * sp_45 
        let r_164 = 0.0003 * sp_41 * BJ2x2_13 
        let r_165 = 0.0003 * sp_41 * sp_42 
        let r_166 = 0.0003 * sp_44 * BJ2x2_14 
        let r_167 = 0.0003 * sp_44 * sp_45 
        let r_168 = 0.1126 * sp_43 
        let r_169 = 0.1126 * sp_43 
        let r_170 = 0.0003 * BJ2x2_12 * sp_42 
        let r_171 = 0.1126 * sp_40 
        let r_172 = 0.1126 * sp_40 
        let r_173 = 0.0003 * sp_25 * BJ2x2_11 
        let r_174 = 0.0003 * sp_25 * sp_26 
        let r_175 = 0.0003 * sp_25 * sp_33 
        let r_176 = 0.0003 * sp_25 * sp_38 
        let r_177 = 0.0003 * BJ2x2_9 * sp_38 
        let r_178 = 0.1126 * sp_35 
        let r_179 = 0.1126 * sp_35 
        let r_180 = 0.0003 * sp_19 * sp_37 
        let r_181 = 0.1126 * sp_36 
        let r_182 = 0.1126 * sp_36 
        let r_183 = 0.0003 * sp_27 * sp_5 
        let r_184 = 0.0003 * sp_27 * sp_23 
        let r_185 = 0.0003 * BJ2x2_9 * sp_33 
        let r_186 = 0.1126 * sp_30 
        let r_187 = 0.1126 * sp_30 
        let r_188 = 0.0003 * sp_19 * sp_32 
        let r_189 = 0.1126 * sp_31 
        let r_190 = 0.1126 * sp_31 
        let r_191 = 0.0003 * sp_28 * sp_5 
        let r_192 = 0.0003 * sp_28 * sp_23 
        let r_193 = 0.0003 * BJ2x2_9 * sp_26 
        let r_194 = 0.1126 * sp_24 
        let r_195 = 0.1126 * sp_24 
        let r_196 = 0.0003 * sp_2 * sp_23 
        let r_197 = 0.1126 * sp_21 
        let r_198 = 0.1126 * sp_21 
        let r_199 = 0.0003 * sp_1 * sp_10 
        let r_200 = 0.0003 * sp_19 * sp_20 
        let r_201 = 0.1126 * sp_18 
        let r_202 = 0.1126 * sp_18 
        let r_203 = 0.0003 * sp_2 * sp_5 
        let r_204 = 0.0003 * sp_14 * BJ2x2_2 
        let r_205 = 0.0003 * sp_14 * sp_15 
        let r_206 = 0.1126 * sp_13 
        let r_207 = 0.1126 * sp_13 
        let r_208 = 0.0003 * BJ2x2_7 * BJ2x2 
        let r_209 = 0.0003 * sp_9 * BJ2x2_10 
        let r_210 = 0.0003 * sp_9 * sp_10 
        let r_211 = 0.1126 * sp_8 
        let r_212 = 0.1126 * sp_8 
        let r_213 = 0.0003 * BJ2x2_15 * BJ2x2_8 
        let r_214 = 0.0003 * sp_4 * BJ2x2_18 
        let r_215 = 0.0003 * sp_4 * sp_5 
        let r_216 = 0.1126 * sp_3 
        let r_217 = 0.1126 * sp_3 
        let r_218 = 0.0003 * BJ2x2_23 * BJ2x2_16 
        let v = Vector.zeros 187
        do
            v.[0] <- 0.0
            v.[1] <- r_0 + r_4 + r_8 + r_12 + r_16 + r_20 + r_38 + r_63 + r_88 + r_216 - r_218
            v.[2] <- -r_26
            v.[3] <- -r_24
            v.[4] <- r_30 - r_31
            v.[5] <- r_33 - r_34
            v.[6] <- -r_214
            v.[7] <- -r_0 - r_4 - r_8 - r_12 - r_16 - r_20 - r_38 - r_63 - r_88
            v.[8] <- r_216 - r_218
            v.[9] <- r_215 - r_216 - r_217 + r_218
            v.[10] <- r_181 - r_183 + r_189 - r_191 + r_201 - r_203 - r_215 + r_217
            v.[11] <- -r_214 - r_215 + r_217
            v.[12] <- r_214
            v.[13] <- r_214
            v.[14] <- r_109 + r_113 + r_117 + r_132 + r_136 + r_140 + r_177 + r_185 + r_193 + r_211 - r_213
            v.[15] <- -r_166
            v.[16] <- -r_164
            v.[17] <- r_169 - r_170
            v.[18] <- r_172 - r_173
            v.[19] <- -r_209
            v.[20] <- -r_109 - r_113 - r_117 - r_132 - r_136 - r_140 - r_177 - r_185 - r_193
            v.[21] <- r_211 - r_213
            v.[22] <- r_210 - r_211 - r_212 + r_213
            v.[23] <- r_125 - r_127 + r_148 - r_150 + r_197 - r_199 - r_210 + r_212
            v.[24] <- -r_209 - r_210 + r_212
            v.[25] <- r_209
            v.[26] <- r_209
            v.[27] <- r_41 + r_45 + r_49 + r_66 + r_70 + r_74 + r_106 + r_129 + r_152 + r_206 - r_208
            v.[28] <- -r_95
            v.[29] <- -r_93
            v.[30] <- r_98 - r_99
            v.[31] <- r_101 - r_102
            v.[32] <- -r_204
            v.[33] <- -r_41 - r_45 - r_49 - r_66 - r_70 - r_74 - r_106 - r_129 - r_152
            v.[34] <- r_206 - r_208
            v.[35] <- r_205 - r_206 - r_207 + r_208
            v.[36] <- r_57 - r_59 + r_82 - r_84 + r_156 - r_158 - r_205 + r_207
            v.[37] <- -r_204 - r_205 + r_207
            v.[38] <- r_204
            v.[39] <- r_204
            v.[40] <- r_119 - r_121 + r_142 - r_144 + r_194 - r_196 + r_201 - r_203
            v.[41] <- r_200 - r_201 - r_202 + r_203
            v.[42] <- r_39 - r_60 + r_64 - r_85 + r_89 - r_159 - r_200 + r_202
            v.[43] <- -r_28 - r_180 + r_182 - r_188 + r_190 - r_200 + r_202
            v.[44] <- r_51 - r_53 + r_76 - r_78 + r_153 - r_155 + r_197 - r_199
            v.[45] <- r_162 - r_197 - r_198 + r_199
            v.[46] <- -r_162 + r_178 - r_184 + r_186 - r_192 + r_194 - r_196 + r_198
            v.[47] <- r_174 - r_194 - r_195 + r_196
            v.[48] <- -r_174 - r_193 + r_195
            v.[49] <- r_193
            v.[50] <- r_109 + r_113 + r_115 + r_117 - r_122 + r_132 + r_136 + r_138 + r_140 - r_145 + r_177 + r_185 + r_186 + r_189 - r_191 - r_192 + r_193
            v.[51] <- r_188 - r_189 - r_190 + r_191
            v.[52] <- r_6 + r_14 + r_22 - r_61 - r_86 - r_160 - r_188 + r_190
            v.[53] <- r_175 - r_186 - r_187 + r_192
            v.[54] <- -r_175 - r_185 + r_187
            v.[55] <- r_185
            v.[56] <- r_109 + r_111 + r_113 + r_117 - r_123 + r_132 + r_134 + r_136 + r_140 - r_146 + r_177 + r_178 + r_181 - r_183 - r_184 + r_185 + r_193
            v.[57] <- r_180 - r_181 - r_182 + r_183
            v.[58] <- r_2 + r_10 + r_18 - r_62 - r_87 - r_161 - r_180 + r_182
            v.[59] <- r_176 - r_178 - r_179 + r_184
            v.[60] <- -r_176 - r_177 + r_179
            v.[61] <- r_177
            v.[62] <- -r_110 + r_112 - r_114 + r_116 - r_118 + r_120 - r_133 + r_135 - r_137 + r_139 - r_141 + r_143 + r_172 - r_173 - r_174 - r_175 - r_176 + r_179 + r_187 + r_195
            v.[63] <- r_165 - r_171 - r_172 + r_173
            v.[64] <- -r_165 + r_169 - r_170 + r_171
            v.[65] <- r_167 - r_168 - r_169 + r_170
            v.[66] <- -r_163 - r_167 + r_168
            v.[67] <- -r_166 - r_167 + r_168
            v.[68] <- r_166
            v.[69] <- r_166
            v.[70] <- -r_164 - r_165 + r_171
            v.[71] <- r_164
            v.[72] <- r_28 + r_164
            v.[73] <- -r_124 + r_126 - r_147 + r_149 - r_162 - r_163 + r_198
            v.[74] <- r_163
            v.[75] <- r_93 + r_163
            v.[76] <- r_2 + r_6 + r_89 + r_156 - r_158 - r_159 - r_160 - r_161
            v.[77] <- r_91 - r_156 - r_157 + r_158
            v.[78] <- -r_91 + r_107 - r_128 + r_130 - r_151 + r_153 - r_155 + r_157
            v.[79] <- r_103 - r_153 - r_154 + r_155
            v.[80] <- -r_103 - r_152 + r_154
            v.[81] <- r_152
            v.[82] <- r_41 + r_45 + r_47 + r_49 - r_54 + r_66 + r_70 + r_72 + r_74 - r_79 + r_106 + r_129 + r_130 + r_148 - r_150 - r_151 + r_152
            v.[83] <- r_147 - r_148 - r_149 + r_150
            v.[84] <- r_134 + r_138 + r_142 - r_144 - r_145 - r_146 - r_147 + r_149
            v.[85] <- r_141 - r_142 - r_143 + r_144
            v.[86] <- -r_140 - r_141 + r_143
            v.[87] <- r_140
            v.[88] <- r_137 - r_138 - r_139 + r_145
            v.[89] <- -r_136 - r_137 + r_139
            v.[90] <- r_136
            v.[91] <- r_133 - r_134 - r_135 + r_146
            v.[92] <- -r_132 - r_133 + r_135
            v.[93] <- r_132
            v.[94] <- r_104 - r_130 - r_131 + r_151
            v.[95] <- -r_104 - r_129 + r_131
            v.[96] <- r_129
            v.[97] <- r_41 + r_43 + r_45 + r_49 - r_55 + r_66 + r_68 + r_70 + r_74 - r_80 + r_106 + r_107 + r_125 - r_127 - r_128 + r_129 + r_152
            v.[98] <- r_124 - r_125 - r_126 + r_127
            v.[99] <- r_111 + r_115 + r_119 - r_121 - r_122 - r_123 - r_124 + r_126
            v.[100] <- r_118 - r_119 - r_120 + r_121
        do
            v.[101] <- -r_117 - r_118 + r_120
            v.[102] <- r_117
            v.[103] <- r_114 - r_115 - r_116 + r_122
            v.[104] <- -r_113 - r_114 + r_116
            v.[105] <- r_113
            v.[106] <- r_110 - r_111 - r_112 + r_123
            v.[107] <- -r_109 - r_110 + r_112
            v.[108] <- r_109
            v.[109] <- r_105 - r_107 - r_108 + r_128
            v.[110] <- -r_105 - r_106 + r_108
            v.[111] <- r_106
            v.[112] <- -r_42 + r_44 - r_46 + r_48 - r_50 + r_52 - r_67 + r_69 - r_71 + r_73 - r_75 + r_77 + r_101 - r_102 - r_103 - r_104 - r_105 + r_108 + r_131 + r_154
            v.[113] <- r_94 - r_100 - r_101 + r_102
            v.[114] <- -r_94 + r_98 - r_99 + r_100
            v.[115] <- r_96 - r_97 - r_98 + r_99
            v.[116] <- -r_92 - r_96 + r_97
            v.[117] <- -r_95 - r_96 + r_97
            v.[118] <- r_95
            v.[119] <- r_95
            v.[120] <- -r_93 - r_94 + r_100
            v.[121] <- r_93
            v.[122] <- -r_56 + r_58 - r_81 + r_83 - r_91 - r_92 + r_157
            v.[123] <- r_92
            v.[124] <- r_24 + r_92
            v.[125] <- r_35 - r_89 - r_90 + r_159
            v.[126] <- -r_35 - r_88 + r_90
            v.[127] <- r_88
            v.[128] <- r_0 + r_4 + r_8 + r_10 + r_12 + r_14 + r_16 + r_20 + r_38 + r_63 + r_64 + r_82 - r_84 - r_85 - r_86 - r_87 + r_88
            v.[129] <- r_81 - r_82 - r_83 + r_84
            v.[130] <- r_68 + r_72 + r_76 - r_78 - r_79 - r_80 - r_81 + r_83
            v.[131] <- r_75 - r_76 - r_77 + r_78
            v.[132] <- -r_74 - r_75 + r_77
            v.[133] <- r_74
            v.[134] <- r_71 - r_72 - r_73 + r_79
            v.[135] <- -r_70 - r_71 + r_73
            v.[136] <- r_70
            v.[137] <- r_67 - r_68 - r_69 + r_80
            v.[138] <- -r_66 - r_67 + r_69
            v.[139] <- r_66
            v.[140] <- r_36 - r_64 - r_65 + r_85
            v.[141] <- -r_36 - r_63 + r_65
            v.[142] <- r_63
            v.[143] <- r_0 + r_4 + r_8 + r_12 + r_16 + r_18 + r_20 + r_22 + r_38 + r_39 + r_57 - r_59 - r_60 - r_61 - r_62 + r_63 + r_88
            v.[144] <- r_56 - r_57 - r_58 + r_59
            v.[145] <- r_43 + r_47 + r_51 - r_53 - r_54 - r_55 - r_56 + r_58
            v.[146] <- r_50 - r_51 - r_52 + r_53
            v.[147] <- -r_49 - r_50 + r_52
            v.[148] <- r_49
            v.[149] <- r_46 - r_47 - r_48 + r_54
            v.[150] <- -r_45 - r_46 + r_48
            v.[151] <- r_45
            v.[152] <- r_42 - r_43 - r_44 + r_55
            v.[153] <- -r_41 - r_42 + r_44
            v.[154] <- r_41
            v.[155] <- r_37 - r_39 - r_40 + r_60
            v.[156] <- -r_37 - r_38 + r_40
            v.[157] <- r_38
            v.[158] <- -r_1 + r_3 - r_5 + r_7 - r_9 + r_11 - r_13 + r_15 - r_17 + r_19 - r_21 + r_23 + r_33 - r_34 - r_35 - r_36 - r_37 + r_40 + r_65 + r_90
            v.[159] <- r_25 - r_32 - r_33 + r_34
            v.[160] <- -r_25 + r_30 - r_31 + r_32
            v.[161] <- r_27 - r_29 - r_30 + r_31
            v.[162] <- -r_27 - r_28 + r_29
            v.[163] <- r_28
            v.[164] <- -r_26 - r_27 + r_29
            v.[165] <- r_26
            v.[166] <- r_26
            v.[167] <- -r_24 - r_25 + r_32
            v.[168] <- r_24
            v.[169] <- r_21 - r_22 - r_23 + r_61
            v.[170] <- -r_20 - r_21 + r_23
            v.[171] <- r_20
            v.[172] <- r_17 - r_18 - r_19 + r_62
            v.[173] <- -r_16 - r_17 + r_19
            v.[174] <- r_16
            v.[175] <- r_13 - r_14 - r_15 + r_86
            v.[176] <- -r_12 - r_13 + r_15
            v.[177] <- r_12
            v.[178] <- r_9 - r_10 - r_11 + r_87
            v.[179] <- -r_8 - r_9 + r_11
            v.[180] <- r_8
            v.[181] <- r_5 - r_6 - r_7 + r_160
            v.[182] <- -r_4 - r_5 + r_7
            v.[183] <- r_4
            v.[184] <- r_1 - r_2 - r_3 + r_161
            v.[185] <- -r_0 - r_1 + r_3
            v.[186] <- r_0
        v

    let DNA_init = 
       let x0 = oscillating_ics // Initial point

       let f (t:double) (x: Vector) = // OscillatingDNA's right part
              oscillating_derivs t x
       init 0.0 x0 f { defaults() with MaxStep = 500.0 }

    let DNA_solve (rm : state) count =
        let t = Array.zeroCreate count
        let x1 = Array.zeroCreate count
        let x2 = Array.zeroCreate count
        let x3 = Array.zeroCreate count

        let mutable r = rm
        for i = 0 to count - 1 do
            Array.set t i r.t
            Array.set x1 i (r.x.[76] + r.x.[128] + r.x.[143])
            Array.set x2 i (r.x.[44] + r.x.[82] + r.x.[97])
            Array.set x3 i (r.x.[40] + r.x.[50] + r.x.[56])
            r <- advance r

        (t,x1,x2,x3,r)



