module type S = sig
  type t
  type atom

  val atoms_of : t -> atom list
end
