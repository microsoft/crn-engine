// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.Io

#if JavaScript
//This probably won't work in JavaScript...
#else
//takes a filename and produces a string of the file contents
val load_file : string -> string 
val write_file : string -> string -> unit
val set_working_dir : string -> string
val load_data : string -> Crn_settings<'a> -> Crn_settings<'a>
#endif