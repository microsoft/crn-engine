// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace global

//these types are dummy types to enable F# portable library compilation of WebSharper intended code

open System

[<AttributeUsage(AttributeTargets.All)>]
type JavaScriptAttribute() = inherit Attribute()
[<Sealed; AttributeUsage(AttributeTargets.Constructor|||AttributeTargets.Method|||AttributeTargets.Property)>]
type InlineJavaScriptAttribute() =
    inherit Attribute()
    /// Constructs a new inlining annotation from a code template.
    new (template: string) = InlineJavaScriptAttribute()
type DirectJavaScriptAttribute() =
    inherit Attribute()
    /// Constructs a new inlining annotation from a code template.
    new (template: string) = DirectJavaScriptAttribute()
[<AttributeUsage(AttributeTargets.All)>]
type NamedUnionCasesAttribute() = inherit Attribute()
[<AttributeUsage(AttributeTargets.All)>]
type ConstantAttribute(n:string) = inherit Attribute()
