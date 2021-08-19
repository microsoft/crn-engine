module Microsoft.Research.CRNEngine.Calculus

type value = Expression.t<string>
type key<'k> = Key.t<'k>
type reaction<'s> when 's : equality = Reaction<'s,value,Expression.t<key<'s>>> 

type t<'s> when 's : equality = { react : 's list -> 's -> reaction<'s> list }