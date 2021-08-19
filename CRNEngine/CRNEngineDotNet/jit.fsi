[<JavaScript>] 
module Microsoft.Research.CRNEngine.Jit

type reaction<'s> when 's : equality = Reaction<'s, Expression.t<string>, Expression.t< Key<'s >>>

// JIT simulation data
type t<'s> when 's : equality     // JIT internal data structures
type cancel = bool ref            // cancel button
type row    = float Row         // data points at some time t
type output = row -> unit         // data sink
type newplottable = {
  name:string;
  structural:string option;
  svg:string option;
}
type outputplottable = newplottable -> unit // add a new plottable

// utility methods
val empty   : printSpecies      : ('s -> newplottable)     (* a function to print species. It's only used to print column 
                                                       names in the results (of type Table) at the end of the 
                                                       simulation. (FP) this should also provide GUI decorations *)
              -> equalsSpecies   : ('s -> 's -> bool ) (* this is used to check if two species are equivalent. This accomodates 
                                                          equivalence up to rotational symmetry in DSD for example. *)
              -> matchesSpecies : ('s -> 's -> bool) (* species equivalence; it's used to check if a new species
                                                        matches a plot. This is necessary in DSD for example, where a 
                                                        plot sum(<_ A B> can match freshly generated species in JIT. *)
              -> t<'s> when 's : equality 
val create : simulation        : Simulation_settings<Expression.t<Inlined<'s>>>
             -> stochastic     : Stochastic_settings
             -> rates          : Map<string, Expression.t<Inlined<'s>>>
             -> parameters     : Parameter list
             -> explored       : 's list
             -> initials       : Initial<'s, Expression.t<string>> list
             -> reactions      : Reaction<'s, float, Expression.t<Inlined<'s>>> list
             -> printSpecies   : ('s -> newplottable) // see comment above
             -> equalsSpecies   : ('s -> 's -> bool ) // see comment above
             -> matchesSpecies : ('s -> 's -> bool)  // see comment above
             -> t<'s> when 's : comparison

val fromCRN : Crn -> t<Species>

val get_species : t<'s> -> Initial<'s, Expression.t<string>> list
val get_reactions : t<'s> -> Reaction<'s, float, Expression.t<Inlined<'s>>> list

// core JIT simulation function
val simulate_callback : cancel 
                        -> output 
                        -> outputplottable
                        -> t<'s> 
                        -> Calculus<'s> 
                        -> Unit

// main entry point
val simulate : jit               : t<'s>                 (* a JIT data structure *)
               -> calculus       : Calculus<'s>        (* a calculus to find new reactions *)
               -> finalJit       : t<'s>                 (* the final JIT data structure *)
                  * results      : Table<float>        (* a table with JIT data points for old and new species *)

val to_ctmc : jit:t<'s> -> calc:Calculus<'s> -> (ctmc_result<'s>*('s->Attributes))
val to_crn : jit:t<'s> -> original:Crn -> Crn