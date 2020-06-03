module type S = sig
  type t

  val effects_of : t -> Eff.Set.t
end
