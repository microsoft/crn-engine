// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//F# 4.0

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Microsoft.Research.CRNEngine.Tests.Array

let inline checkNonNull argName arg = 
    match box arg with 
    | null -> nullArg argName 
    | _ -> ()

[<CompiledName("Last")>]
let inline last (array : 'T[]) =
    checkNonNull "array" array
    if array.Length = 0 then invalidArg "array" "The input array was empty." //LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    array.[array.Length-1]