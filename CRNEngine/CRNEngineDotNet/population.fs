namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Population<'species,'value> = 
  {
    constant: bool; // Population never changes.
    value: 'value;
    species: 'species;
    initial: bool; // Present at start of simulation. Can include species generated from infinite rate reactions.
    input: bool; // Specified by the user.
    spatial_initial: Microsoft.Research.CRNEngine.Spatial_initial.t option
    max: int option;
  }
  static member create (c:bool,v:'value,s:'species,i:bool,input:bool,m:int option) = 
    {constant = c; value = v; species = s; initial = i; input = input; max = m; spatial_initial = None}
  static member create (c:bool,v:'value,s:'species) = Population.create(c,v,s,true,true,None)
  member p.to_string (species_to_string:'species -> string) (value_to_string:'value -> string) =
    (if p.constant then "constant " else "") + (value_to_string p.value) + " * " + (species_to_string p.species) 
