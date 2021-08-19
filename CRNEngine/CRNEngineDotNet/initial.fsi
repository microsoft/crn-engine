[<JavaScript>]
module Microsoft.Research.CRNEngine.Initial



type t<'species,'value> = {
  species: 'species;
  value: 'value;
  constant: bool; 
  time: 'value;
  spatial : Spatial_initial.t option;
}

type gui = t<string,string>

val mapSpecies            : ('a -> 'b) -> t<'a, 'v> -> t<'b, 'v>
val mapValues             : ('v -> 'w) -> t<'a, 'v> -> t<'a, 'w>
val create                : bool -> 'value -> 'species -> 'value -> Spatial_initial.t option -> t<'species,'value>
val create_gui            : bool -> string -> string -> string -> Spatial_initial.t option -> t<string,string>
val collectSpecies        : ('a -> 'b list) -> t<'a, 'v> -> t<'b, 'v> list
val to_string             : ('species -> string) -> ('value -> string) -> ('value -> bool) -> t<'species,'value> -> string
val display               : ('species -> string) -> ('value -> string) -> ('value -> bool) -> t<'species,'value> -> string
val to_gui                : ('species -> string) -> ('value -> string) -> t<'species,'value> -> t<string,string>
val from_gui              : (string -> 'species) -> (string -> 'value) -> gui -> t<'species,'value>
val eval                  : ('value -> 'output) -> t<'species,'value> -> t<'species,'output>
/// Sum up the initial conditions to define initial values for each population, and separate timed events for downstream consumption. This initialization method is generic in both the species 's of the population and the resulting format 'a in which populations are stored (e.g. the 2d spatial simulator stores populations along a grid (cartesian plane), so 'a = float [][]; non-spatial CRNs only store the concentration of a species, so 'a = float).
val to_initialpops_events_poly<'s, 'a> : env          : Environment.t 
                                      -> initial_time : float 
                                      -> is           : t<'s,Expression.t<string>> list 
                                      -> accumulator  : (t<'s,float> seq -> 'a)
                                      -> Populations.t<'s,'a> * Event.t<'s,float,'a> list when 's : equality
val to_initialpops_events<'s> : env:Environment.t 
                                -> initial_time:float 
                                -> is:t<'s,Expression.t<string>> list 
                                -> Populations.t<'s,float> * Event.t<'s,float,float> list when 's : equality
val parse                 : Parser.t<'species> -> Parser.t<'value> -> 'value -> Parser.t<t<'species,'value>>