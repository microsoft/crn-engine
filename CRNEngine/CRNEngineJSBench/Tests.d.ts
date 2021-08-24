// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

declare var Papa: any;
declare class Tests {
    private static program_simulate_oslo_am;
    private static program_simulate_oslo_am_sundials;
    private static program_simulate_oslo_am_functional;
    private static program_simulate_oslo_am_sundials_functional;
    private static program_simulate_oslo_waves;
    private static program_simulate_sundials_waves;
    private static program_infer_oslo_am;
    private static program_infer_sundials_am;
    private static observation_am_noised;
    private static ParseFile;
    private static ConvertTable;
    private static run_crn_oslo_simulate;
    private static run_crn_sundials_simulate;
    private static run_crm_inference;
    private static run_crm_inference_10000;
    static Simulate_Oslo_AM(): void;
    static Simulate_Oslo_Waves(): void;
    static Simulate_Oslo_AM_functional(): void;
    static Simulate_Sundials_AM(): void;
    static Simulate_Sundials_Waves(): void;
    static Simulate_Sundials_AM_functional(): void;
    static Infer_Oslo_AM_1000(): void;
    static Infer_Sundials_AM_1000(): void;
    static Infer_Oslo_AM_10000(): void;
    static Infer_Sundials_AM_10000(): void;
}
