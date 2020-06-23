open Core_kernel
open Lib

module Make (F : Functor.S1) : MonadFree.S1 with module F := F = struct
  module Minimal = struct
    type 'a t =
      | Pure of 'a
      | Eff of 'a t F.t

    let effect x = Eff x
    let pure x = Pure x

    let rec map_ t ~f =
      match t with
      | Pure x -> Pure (f x)
      | Eff e -> Eff (F.map ~f:(map_ ~f) e)
    ;;

    let map = `Custom map_
    let return = pure

    let rec bind t ~f =
      match t with
      | Pure x -> f x
      | Eff e -> Eff (F.map ~f:(bind ~f) e)
    ;;

    let rec apply f t =
      match f, t with
      | Pure g, Pure x -> Pure (g x)
      | Pure f, Eff e -> Eff (F.map ~f:(map_ ~f) e)
      | Eff e, _ -> Eff (F.map ~f:(fun f -> apply f t) e)
    ;;
  end

  include Minimal
  include Monad.Make (Minimal)
  include Applicative.Make (Minimal)

  let rec run t ~pure ~eff =
    match t with
    | Pure x -> pure x
    | Eff e -> eff @@ F.map ~f:(run ~pure ~eff) e
  ;;

  let liftF e = effect @@ F.map ~f:(fun x -> Pure x) e
end

module CPS = struct
  module Make (F : Functor.S1) : MonadFree.S1 with module F := F = struct
    module Minimal = struct
      type 'a t = { run : 'r. ('a -> 'r) -> ('r F.t -> 'r) -> 'r }

      let effect x =
        { run = (fun kp kf -> kf @@ F.map x ~f:(fun { run } -> run kp kf)) }
      ;;

      let pure x = { run = (fun kp _ -> kp x) }
      let return = pure

      let bind { run } ~f =
        { run = (fun kp kf -> run (fun x -> (f x).run kp kf) kf) }
      ;;

      let map_ { run } ~f = { run = (fun kp kf -> run (fun x -> kp @@ f x) kf) }
      let map = `Custom map_

      let apply f t =
        { run = (fun kp kf -> f.run (fun f -> t.run (fun x -> kp @@ f x) kf) kf)
        }
      ;;
    end

    include Minimal
    include Monad.Make (Minimal)
    include Applicative.Make (Minimal)

    let join mmx = { run = (fun kp kf -> mmx.run (fun mx -> mx.run kp kf) kf) }
    let run { run } ~pure ~eff = run pure eff
    let liftF e = effect @@ F.map ~f:pure e
  end
end
