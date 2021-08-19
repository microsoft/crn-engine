[<JavaScript>] 
module Microsoft.Research.CRNEngine.Reaction

type t<'s,'v,'e> when 's:equality = {
  reactants:Mset.t<'s>;
  products:Mset.t<'s>;
  catalysts:Mset.t<'s>;
  rate: Rate.t<'v,'e>;
  reverseRate: Rate.t<'v,'e> option; 
}
type gui = t<string,string,string>                

val create : Mset.t<'s> -> Mset.t<'s> -> Rate.t<'v,'e> -> Rate.t<'v,'e> option -> Mset.t<'s> -> t<'s,'v,'e>
val create_massaction : list<'s> -> list<'s> -> 'v -> 'v option -> list<'s> -> t<'s,'v,'e>
val create_functional : list<'s> -> list<'s> -> 'e -> 'e option -> list<'s> -> t<'s,'v,'e>
val map : ('s1 -> 's2) -> (Rate.t<'v1,'e1> -> Rate.t<'v2,'e2>) -> t<'s1,'v1,'e1> -> t<'s2,'v2,'e2>
val from_gui : (string -> 's) -> (string -> 'v) -> (string -> 'e) -> gui -> t<'s,'v,'e>
val to_gui : ('s -> string) -> ('v -> string) -> ('e -> string) -> t<'s,'v,'e> -> gui
val allSpecies : t<'s,'v,'e> -> 's list
val to_string : ('s -> string) -> ('v -> string) -> ('e -> string) -> t<'s,'v,'e> -> string
val rate_units : t<'s,'v,'e> -> string -> string -> (string*string)
val scale : (float -> int -> 'v -> 'v) -> float -> t<'s,'v,'e> -> t<'s,'v,'e>
val normalise : t<'s,'v,'e> -> t<'s,'v,'e> * t<'s,'v,'e> option
val normalise_list : t<'s,'v,'e> list -> t<'s,'v,'e> list
val numInReactants : 's -> t<'s,'v,'e> -> int
val numInProducts : 's -> t<'s,'v,'e> -> int
val getStoich : bool -> 's -> t<'s,'v,'e> -> float
val reverse : t<'s,'v,'e> -> t<'s,'v,'e> option
val parse : Parser.t<'s> -> Parser.t<'v> -> Parser.t<'e> -> 'v -> Parser.t<t<'s,'v,'e>>

//NB the code below is for specific instantiations of the reaction type
val get_sim_reactions_products : float 
                                  -> Populations.t<Species.t,'a> 
                                  -> paramEnv : Environment.t 
                                  -> ratesEnv : Map<string,Expression.t<Key.inlined_t<Species.t>>>
                                  -> t<Species.t,Expression.t<string>,Expression.t<Key.t<Species.t>>> list 
                                  -> (t<int,float,Lambda.t<Key.inlined_t<int>>> * Mset.t<int> * Rate.t<float,Expression.t<Key.inlined_t<int>>> ) list