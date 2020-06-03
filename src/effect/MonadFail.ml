open Core_kernel
open Lib

module type S1 = sig
  module Err : Tycon.S0
  include Monad.S

  val fail : Err.t -> 'a t
end
