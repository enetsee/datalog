open Core_kernel
open Lib

module Atomic = struct
  include Set.Make (Int)

  let from_MV mv =
    of_list
    @@ List.filter ~f:(fun x -> x > -1)
    @@ List.mapi ~f:(fun idx ->
         function
         | Mode.Req -> idx
         | _ -> -1)
    @@ ModeVector.to_list mv
  ;;

  let to_MV t ~arity =
    ModeVector.from_list
    @@ List.init arity ~f:(fun i -> if mem t i then Mode.Req else Mode.Opt)
  ;;

  let pp ppf xs = Fmt.(hbox @@ braces @@ list ~sep:comma int) ppf (elements xs)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)

  let leq x y = is_subset x ~of_:y
end

(* -- Constraints ----------------------------------------------------------- *)

include Set.Make (Atomic)

let ill = empty
let trivial = singleton @@ Atomic.empty

let pp ppf xs =
  Fmt.(hvbox @@ braces @@ list ~sep:comma Atomic.pp) ppf (elements xs)
;;

let from_MVs mvs = of_list @@ List.map ~f:Atomic.from_MV mvs
let to_MVs t ~arity = List.map ~f:(Atomic.to_MV ~arity) @@ elements t

(* -- Lattice --------------------------------------------------------------- *)
let top = ill
let bottom = trivial

let leq xs ys =
  List.(
    elements xs >>= fun x -> elements ys >>= fun y -> return @@ Atomic.leq x y)
  |> List.for_all ~f:Fn.id
;;

let join xs ys =
  List.(
    elements xs >>= fun x -> elements ys >>= fun y -> return @@ Atomic.union x y)
  |> of_list
;;

(* unused and wrong! *)
let meet xs ys =
  List.(
    elements xs >>= fun x -> elements ys >>= fun y -> return @@ Atomic.inter x y)
  |> of_list
;;
