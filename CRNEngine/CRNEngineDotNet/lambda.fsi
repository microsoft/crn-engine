[<JavaScript>] 
module Microsoft.Research.CRNEngine.Lambda

type 'key t when 'key:equality = ('key -> float) -> float

val map : ('a -> 'b) -> 'a t -> 'b t
val reset : 'a t -> ( 'a -> 'key) -> 'key t //special case of mapLambda