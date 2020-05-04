module type S = sig
  type t

  val atoms : t -> 'a Atom.t list
end

module type S1 = sig
  type 'a t

  val atoms : ('a -> 'a Atom.t list) -> 'a t -> 'a Atom.t list
end
