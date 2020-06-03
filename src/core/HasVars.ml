module type S = sig
  type t

  val vars_of : t -> Tmvar.t list
end

module type S1 = sig
  type 'a t

  val vars : ('a -> Tmvar.t list) -> 'a t -> Tmvar.t list
end
