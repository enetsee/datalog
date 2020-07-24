open Core_kernel
open Lib

(** The `Reader` monad presents a computation, which can read values from a 
    shared environment, pass values from function to function, and execute 
    sub-computations in a modified environment. 
*)
module type S = sig
  module M : Monad.S
  include Monad.S2
  include Applicative.S2 with type ('a, 'env) t := ('a, 'env) t

  val lift : 'a M.t -> ('a, 'env) t
  val run : ('a, 'env) t -> env:'env -> 'a M.t
  val reader : ('env -> 'a) -> ('a, 'env) t
  val local : ('a, 'env) t -> f:('env -> 'env) -> ('a, 'env) t
  val ask : ('env, 'env) t
end

module Make (M : Monad.S) : S with module M := M = struct
  module Minimal = struct
    type ('a, 'env) t = { apply : 'b. ('a -> 'b M.t) -> 'env -> 'b M.t }

    let map { apply = g } ~f = { apply = (fun k -> g @@ fun a -> k @@ f a) }
    let map = `Custom map
    let return a = { apply = (fun k _ -> k a) }

    let bind { apply = g } ~f =
      { apply = (fun k env -> g (fun a -> (f a).apply k env) env) }
    ;;

    let apply mf mt = bind mf ~f:(fun f -> bind mt ~f:(fun t -> return @@ f t))
  end

  include Minimal
  include Monad.Make2 (Minimal)
  include Applicative.Make2 (Minimal)

  let run { apply = g } ~env = g M.return env
  let reader f = { apply = (fun k env -> k @@ f env) }
  let ask = { apply = (fun k env -> k @@ env) }
  let local { apply = g } ~f = { apply = (fun k env -> g k @@ f env) }
  let lift m = { apply = (fun f _ -> M.bind m ~f) }
end

module type S_with_env = sig
  module Env : Tycon.S0
  module M : Monad.S
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val lift : 'a M.t -> 'a t
  val run : 'a t -> env:Env.t -> 'a M.t
  val reader : (Env.t -> 'a) -> 'a t
  val local : 'a t -> f:(Env.t -> Env.t) -> 'a t
  val ask : Env.t t
end

module Make_with_env (Env : Tycon.S0) (M : Monad.S) :
  S_with_env with module Env := Env and module M := M = struct
  module Minimal = struct
    type 'a t = { apply : 'b. ('a -> 'b M.t) -> Env.t -> 'b M.t }

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

  let run { apply = g } ~env = g M.return env
  let reader f = { apply = (fun k env -> k @@ f env) }
  let ask = reader Fn.id
  let local { apply = g } ~f = { apply = (fun k env -> g k @@ f env) }
  let lift m = { apply = (fun f _ -> M.bind m ~f) }
end
