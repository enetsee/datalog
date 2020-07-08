open Core_kernel
open Lib

module type S1 = sig
  module Err : Tycon.S0
  module Topic : Monoid.S0
  module State : Tycon.S0
  module Env : Tycon.S0
  include Monad.S
  include Applicative.S with type 'a t := 'a t
  include MonadFail.S1 with type 'a t := 'a t and module Err := Err
  include MonadReader.S1 with type 'a t := 'a t and module Env := Env
  include MonadWriter.S1 with type 'a t := 'a t and module Topic := Topic
  include MonadState.S1 with type 'a t := 'a t and module State := State

  val run
    :  'a t
    -> env:Env.t
    -> st:State.t
    -> ('a, Err.t) result * Topic.t * State.t

  val eval : 'a t -> env:Env.t -> st:State.t -> ('a, Err.t) result
end

module Make
    (Err : Tycon.S0)
    (Topic : Monoid.S0)
    (State : Tycon.S0)
    (Env : Tycon.S0) :
  S1
    with module Err := Err
     and module Topic := Topic
     and module State := State
     and module Env := Env = struct
  module Minimal = struct
    type 'a t =
      { apply :
          'o. Env.t -> State.t
          -> k:(('a, Err.t) result -> Topic.t -> State.t -> 'o) -> 'o
      }

    let run { apply } ~env ~st = apply env st ~k:(fun r w s -> r, w, s)

    let eval t ~env ~st =
      let res, _, _ = run ~env ~st t in
      res
    ;;

    let map { apply = g } ~f =
      { apply = (fun env st ~k -> g env st ~k:(fun a -> k @@ Result.map ~f a)) }
    ;;

    let map = `Custom map
    let return a = { apply = (fun _ st ~k -> k (Ok a) Topic.mempty st) }

    let bind { apply = g } ~f =
      { apply =
          (fun env st ~k ->
            g env st ~k:(fun a_res w st' ->
                match Result.map ~f a_res with
                | Error err -> k (Error err) w st'
                | Ok { apply } ->
                  apply env st' ~k:(fun b_res w' st'' ->
                      k b_res Topic.(append w w') st'')))
      }
    ;;

    let apply mf mt = bind mf ~f:(fun f -> bind mt ~f:(fun t -> return @@ f t))
  end

  module Effects = struct
    include Minimal
    include Monad.Make (Minimal)
    include Applicative.Make (Minimal)

    let writer (a, warn) = { apply = (fun _ st ~k -> k (Ok a) warn st) }

    let listen { apply = f } =
      { apply =
          (fun env st ~k ->
            f env st ~k:(fun a_res w st' ->
                k Result.(map ~f:(fun a -> a, w) a_res) w st'))
      }
    ;;

    let pass { apply = f } =
      { apply =
          (fun env st ~k ->
            f env st ~k:(fun a_res w st' ->
                match a_res with
                | Error err -> k (Error err) w st'
                | Ok (a, p) -> k (Ok a) (p w) st'))
      }
    ;;

    let reader f = { apply = (fun env st ~k -> k (Ok (f env)) Topic.mempty st) }
    let ask = reader Fn.id
    let local { apply = g } ~f = { apply = (fun env st ~k -> g (f env) st ~k) }

    let state f =
      { apply =
          (fun _ st ~k ->
            let a, st' = f st in
            k (Ok a) Topic.mempty st')
      }
    ;;

    let fail err = { apply = (fun _ st ~k -> k (Error err) Topic.mempty st) }
  end

  include Effects
  include MonadWriter.MakeTopic (Topic) (Effects)
  include MonadState.MakeState (State) (Effects)
end
