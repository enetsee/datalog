type +'a t

include Functor.S1 with type 'a t := 'a t
include Foldable.S1 with type 'a t := 'a t
include Pretty.S1 with type 'a t := 'a t

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val to_list : 'a t -> 'a list
val from_list : 'a list -> 'a t option
val from_list_exn : 'a list -> 'a t
val last : 'a t -> 'a
