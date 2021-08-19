module Microsoft.Research.CRNEngine.Mset

type entry<'a> = {
  element:'a;
  multiplicity:int
}
type 'a t = entry<'a> list //List is assumed to be without duplicates. Would have liked to hide this but it is too simple to hide, just an "abbreviation"

val empty             : 'a t
val from_list         : 'a list -> 'a t when 'a:equality
val from_mlist        : entry<'a> list -> 'a t when 'a:equality
val fold_left         : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left_m       : ('a -> entry<'b> -> 'a) -> 'a -> 'b t -> 'a
val iter              : ('a -> unit) -> 'a t -> unit
val iterm             : (entry<'a> -> unit) -> 'a t -> unit
val to_list           : 'a t -> 'a list (* returns the elements repeated with multiplicity *)
val to_map            : 'a t -> Map<'a,int>
val to_mlist          : 'a t -> entry<'a> list
val add               : ('a -> 'a -> bool) -> 'a t -> entry<'a> -> 'a t
val union             : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
val union_all         : ('a -> 'a -> bool) -> 'a t list -> 'a t
val intersection      : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
val is_perm           : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val get_mult          : ('a -> 'a -> bool) -> 'a t -> 'a -> int
val elements          : 'a t -> 'a list (* returns the elements without multiplicity *)
val nonzero_elements  : 'a t -> 'a list (* returns the elements without multiplicity *)
val size              : 'a t -> int (* multiplicities counted *)
val nr_uniques        : 'a t -> int (* nr of unique elements *)
val to_string         : ('a -> string) -> string -> 'a t -> string
val to_string_m       : (entry<'a> -> string) -> string -> 'a t -> string
val is_empty          : 'a t -> bool 
val map               : ('a -> 'b) -> 'a t -> 'b t
val mapm              : (entry<'a> -> entry<'b>) -> 'a t -> 'b t
val filter            : ('a -> bool) -> 'a t -> 'a t
val partition         : ('a -> bool) -> 'a t -> ('a t * 'a t)
val collect           : ('a -> 'b) -> 'a t -> 'b list
val collectm          : (entry<'a> -> 'b) -> 'a t -> 'b list
val minus             : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t (* [x] minus [y] = [x natminus y] *)
val difference        : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t (* [x] minus [y] = [x - y], so can result in negative multiplicities *)

val parse             : Parser.t<'a> -> Parser.t<'a t> when 'a:equality