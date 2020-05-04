open Core_kernel

(** The `Reader` monad presents a computation, which can read values from a 
    shared environment, pass values from function to function, and execute 
    sub-computations in a modified environment. 
*)
module type S = sig
  module M : Monad.S
  include Monad.S2
  include Applicative.S2 with type ('a, 'env) t := ('a, 'env) t

  val lift : 'a M.t -> ('a, 'env) t
  val ask : ('st, 'st) t
  val local : 'st -> (unit, 'st) t
  val reader : 'st -> (unit, 'st) t
  val run : ('a, 'env) t -> env:'env -> 'a M.t
end
