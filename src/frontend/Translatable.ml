module type S = sig
  type t
  type elem

  val to_core : t -> (elem, State.t, Err.t) Core.Logger.t
end

module type S1 = sig
  type 'a t
  type elem
  type subelem

  val to_core
    :  ('a -> (subelem, State.t, Err.t) Core.Logger.t)
    -> 'a t
    -> (elem, State.t, Err.t) Core.Logger.t
end
