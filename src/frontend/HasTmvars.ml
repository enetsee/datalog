module type S = sig
  type t

  val tmvars : t -> Core.Tmvar.t list
end

module type S1 = sig
  type 'a t

  val tmvars : ('a -> Core.Tmvar.t list) -> 'a t -> Core.Tmvar.t list
end
