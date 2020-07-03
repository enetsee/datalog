open Core_kernel
open Lib

(** An atomic constraint is an alternate representation of a mode vector. 
    Rather than a list of modes, an atomic constraint is the set of argument 
    indices with a `+` mode.
*)
module Atomic = struct
  include Set.Make (Int)

  let from_MV mv =
    of_list
    @@ List.filter ~f:(fun x -> x > -1)
    @@ List.mapi ~f:(fun idx ->
         function
         | Mode.Req -> idx
         | _ -> -1)
    @@ Mode.Vector.to_list mv
  ;;

  let to_MV t ~arity =
    Mode.Vector.from_list
    @@ List.init arity ~f:(fun i -> if mem t i then Mode.Req else Mode.Opt)
  ;;

  let fully_bound n = of_list @@ List.init n ~f:Fn.id
  let pp ppf xs = Fmt.(hbox @@ braces @@ list ~sep:comma int) ppf (elements xs)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)

  include PartialOrd.Make (struct
    type nonrec t = t

    let leq x y = is_subset x ~of_:y
  end)
end

(* -- Dataflow Constraints -------------------------------------------------- *)

(** A `Constraint` is then a set of `Atomic` constraints. 
    `Constraint.t` forms a bounded lattice with `ill` and `trivial` as bottom 
    and top elements resepectively. *)
include Set.Make (Atomic)

let ill = empty
let trivial = singleton @@ Atomic.empty
let fully_bound n = singleton @@ Atomic.fully_bound n

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf xs =
    Fmt.(hvbox @@ braces @@ list ~sep:comma Atomic.pp) ppf (elements xs)
  ;;

  let pp = `NoPrec pp
end)

let from_MVs mvs = of_list @@ List.map ~f:Atomic.from_MV mvs
let to_MVs t ~arity = List.map ~f:(Atomic.to_MV ~arity) @@ elements t

(* -- Lattice --------------------------------------------------------------- *)
let top = ill
let bottom = trivial

let leq t1 t2 =
  List.(
    elements t1 >>= fun x -> elements t2 >>= fun y -> return @@ Atomic.leq x y)
  |> List.for_all ~f:Fn.id
;;

(** Given a list of atomic constraints, eliminate those atoms which are
    supersets of some other atom.

    Example: given a list of atoms  {{1},{1,2}} we eliminate {1,2} since it is a 
    superset of {1}
*)
let min_of in_ =
  let xs, orig =
    match in_ with
    | `ListIn xs -> xs, of_list xs
    | `SetIn orig -> to_list orig, orig
  in
  of_list
  @@ List.filter ~f:(fun x -> not @@ exists ~f:(fun y -> Atomic.lt y x) orig) xs
;;

(** Join: A (x) B === min (A U B) *)
let join t1 t2 = min_of @@ `SetIn (union t1 t2)

(** Meet: A (+) B ===  min { a U b | a in A, b in B } *)
let meet t1 t2 =
  min_of
  @@ `ListIn
       List.(
         elements t1
         >>= fun x -> elements t2 >>= fun y -> return @@ Atomic.union x y)
;;

let join_list ts = List.fold_left ~init:ill ~f:join ts
let meet_list ts = List.fold_left ~init:trivial ~f:meet ts
