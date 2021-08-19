namespace global

#if JavaScript

//These abbreviations map to the real WebSharper types.

type JavaScriptAttribute = WebSharper.JavaScriptAttribute

#else

//these types are dummy types to enable F# portable library compilation of WebSharper intended code

open System

[<AttributeUsage(AttributeTargets.All)>]
type JavaScriptAttribute() = inherit Attribute()

[<Sealed; AttributeUsage(AttributeTargets.Constructor|||AttributeTargets.Method|||AttributeTargets.Property)>]
type InlineJavaScriptAttribute() =
    inherit Attribute()
    /// Constructs a new inlining annotation from a code template.
    new (template: string) = InlineJavaScriptAttribute()
    
#endif