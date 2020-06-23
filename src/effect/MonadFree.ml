open Core_kernel
open Lib

module type S1 = sig
  module F : Functor.S1
  include Monad.S

  val liftF : 'a F.t -> 'a t
  val effect : 'a t F.t -> 'a t
  val run : 'a t -> pure:('a -> 'r) -> eff:('r F.t -> 'r) -> 'r
end
