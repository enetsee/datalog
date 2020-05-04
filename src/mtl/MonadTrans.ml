open Core_kernel

module type S1 = sig
  module M : Monad.S
  include Monad.S

  val lift : 'a M.t -> 'a t
end
