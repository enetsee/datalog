open Core_kernel

module type S = sig
  type t [@@deriving compare]

  module Map : Map.S with type Key.t := t
  module Set : Set.S with type Elt.t := t
end
