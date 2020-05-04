open Core_kernel

module type S = sig
  include Monad.S2

  val get : ('st, 'st) t
  val put : 'st -> (unit, 'st) t
  val state : ('st -> 'a * 'st) -> ('a, 'st) t
  val modify : ('st -> 'st) -> (unit, 'st) t
end
