module type S = sig
  type t
  type elem

  val to_core : t -> elem Logger.t
end

module type S1 = sig
  type 'a t
  type elem
  type subelem

  val to_core : ('a -> subelem Logger.t) -> 'a t -> elem Logger.t
end
