// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine
[<JavaScript>] 
///Note: structure and svg ought to be option types. 
///However, because of https://github.com/intellifactory/websharper/issues/621, serialization will fail. 
///When we switch to W# 4, they should be reverted to option types.
type Attributes = { 
  name:string;
  structure:string;
  svg:string;
}