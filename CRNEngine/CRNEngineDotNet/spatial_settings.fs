// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.CRNEngine

open Operators
open System.Diagnostics
[<JavaScript>]
type Boundary = 
  | [<WebSharper.Constant "Periodic">] Periodic 
  | [<WebSharper.Constant "ZeroFlux">] ZeroFlux   // ZeroFlux currently not implemented

[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Spatial_settings<'s> when 's:equality =
  { 
    parameters  : Parameter list;
    diffusibles : ('s*Value) list;
    dimensions  : int;
    boundary    : Boundary;
    xmax        : float;
    nx          : int;
    dt          : float;
    default_diffusion : float;
    random      : float;  
  }
  static member defaults = { 
    parameters  = [];
    diffusibles = [];
    dimensions  = 1;
    boundary    = Periodic;
    random      = 0.0;
    xmax        = 1.0;
    nx          = 20;
    default_diffusion = 0.0;
    dt          = 0.0;
  }
  static member map (f:'s -> 'b) (s:Spatial_settings<'s>) = {
    parameters = s.parameters;
    diffusibles = s.diffusibles |> List.map (fun (k,v) -> f k, v);
    dimensions = s.dimensions;
    boundary = s.boundary;
    xmax = s.xmax;
    nx = s.nx;
    dt = s.dt;
    default_diffusion = s.default_diffusion;
    random = s.random;
  }
  static member diffusibles_to_string ps diff = 
    diff 
    |> List.map (fun (k,v) -> sprintf "%s=%s" (ps k) (Expression.to_string id v))
    |> String.concat "; "
  static member to_string (ps:'s -> string) (s:Spatial_settings<'s>) = 
    let defaults = Spatial_settings<'s>.defaults
    [ 
      (if s.dimensions = defaults.dimensions then None else Some (sprintf "dimensions = %d" s.dimensions))
      (if s.boundary = defaults.boundary     then None else Some (sprintf "boundary = %s" (s.boundary.ToString())))
      (if s.diffusibles.IsEmpty              then None else Some (sprintf "diffusibles = [%s]" (Spatial_settings<'s>.diffusibles_to_string ps s.diffusibles)))
      (if s.default_diffusion = defaults.default_diffusion then None else Some (sprintf "default_diffusion = %s" (s.default_diffusion.ToString())))
      (if s.xmax = defaults.xmax             then None else Some (sprintf "xmax = %s" (s.xmax.ToString())))
      (if s.nx = defaults.nx                 then None else Some (sprintf "nx = %d" s.nx))
      (if s.dt = defaults.dt                 then None else Some (sprintf "dt = %s" (s.dt.ToString())))
      (if s.random = defaults.random         then None else Some (sprintf "random = %s" (s.random.ToString())))
    ]
    |> List.choose id
    |> String.concat "; "
    |> sprintf "{ %s }" 
  static member parse_defaults (defaults:Spatial_settings<'s>) (parse_species:Parser.t<'s>) : Parser.t<Spatial_settings<'s>> = 
    let pint = Parser.pint32 .>> Parser.spaces
    let pfloat = Parser.pfloat .>> Parser.spaces
    let diffParser (ps:Parser.t<'s>) : Parser.t<('s*Value) list> = 
      Parser.list_of ( ps .>>. (Parser.skw "=" >>. Expression.parse Parser.name)) 
    Parser.record defaults [ 
      "diffusibles" , diffParser parse_species |>> fun d (s:Spatial_settings<'s>) -> { s with diffusibles = d }
      "default_diffusion", pfloat       |>> fun d (s:Spatial_settings<'s>) -> { s with default_diffusion = d }
      "dimensions"  , pint              |>> fun d (s:Spatial_settings<'s>) -> { s with dimensions  = d }
      "random"      , pfloat            |>> fun r (s:Spatial_settings<'s>) -> { s with random      = r }
      "xmax"        , pfloat            |>> fun x (s:Spatial_settings<'s>) -> { s with xmax        = x }
      "nx"          , pint              |>> fun n (s:Spatial_settings<'s>) -> { s with nx          = n }
      "dt"          , pfloat            |>> fun d (s:Spatial_settings<'s>) -> { s with dt          = d }
      "boundary"    , Parser.choice [ 
                        Parser.kw "Periodic" |>> fun _ (s:Spatial_settings<'s>) -> { s with boundary = Periodic };
                        Parser.kw "ZeroFlux" |>> fun _ (s:Spatial_settings<'s>) -> { s with boundary = ZeroFlux }; 
                      ]
    ]
  static member parse = Spatial_settings<'s>.parse_defaults Spatial_settings<'s>.defaults
