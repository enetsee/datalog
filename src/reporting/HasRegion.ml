module type S = sig
  type t

  val region_of : t -> Region.t
end
