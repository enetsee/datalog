open Core_kernel
open Lib

module type S1 = sig
  module Env : Tycon.S0
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val run : 'a t -> env:Env.t -> 'a
  val reader : (Env.t -> 'a) -> 'a t
  val local : 'a t -> f:(Env.t -> Env.t) -> 'a t
  val ask : Env.t t
end

module Make_with_env (Env : Tycon.S0) : S1 with module Env := Env = struct
  module Minimal = struct
    type 'a t = { apply : 'o. ('a -> 'o) -> Env.t -> 'o }

    let map { apply = g } ~f = { apply = (fun k -> g @@ fun a -> k @@ f a) }
    let map = `Custom map
    let return a = { apply = (fun k _ -> k a) }

    let bind { apply = g } ~f =
      { apply = (fun k env -> g (fun a -> (f a).apply k env) env) }
    ;;

    let apply mf mt = bind mf ~f:(fun f -> bind mt ~f:(fun t -> return @@ f t))
  end

  include Minimal
  include Monad.Make (Minimal)
  include Applicative.Make (Minimal)

  let run { apply = g } ~env = g Fn.id env
  let reader f = { apply = (fun k env -> k @@ f env) }
  let ask = reader Fn.id
  let local { apply = g } ~f = { apply = (fun k env -> g k @@ f env) }
end
