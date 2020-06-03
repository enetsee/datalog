open Core_kernel
open Lib

module type Minimal1 = sig
  module Env : Tycon.S0
  include Monad.S

  val reader : (Env.t -> 'a) -> 'a t
  val local : 'a t -> f:(Env.t -> Env.t) -> 'a t
end

module type S1 = sig
  include Minimal1

  val ask : Env.t t
end

module Make (X : Minimal1) :
  S1 with module Env := X.Env and type 'a t := 'a X.t = struct
  include X

  let ask = reader Fn.id
end

module MakeEnv (Env : Tycon.S0) (X : Minimal1 with module Env := Env) :
  S1 with module Env := Env and type 'a t := 'a X.t = struct
  include X

  let ask = reader Fn.id
end
