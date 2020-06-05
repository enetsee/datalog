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

(** A `Constraint` is then a set of `Atomic` constraints. 
    `Constraint.t` forms a bounded lattice with `ill` and `trivial` as bottom 
    and top elements resepectively. *)
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

let leq t1 t2 =
  List.(
    elements t1 >>= fun x -> elements t2 >>= fun y -> return @@ Atomic.leq x y)
  |> List.for_all ~f:Fn.id
;;

(** Given a list of atomic constraints, eliminate those atoms which are
    supersets of some other atom 

    Example: 

    given a list of atoms  {{1},{1,2}}
    we eliminate {1,2} since it is a superset of {1}


*)
let elim_supersets xs =
  let rec aux accu = function
    | next :: rest ->
      let next' =
        List.filter next ~f:(fun atom ->
            not @@ List.exists accu ~f:(fun atom' -> Atomic.leq atom' atom))
      in
      aux (next' @ accu) rest
    | [] -> accu
  in
  aux []
  @@ List.group ~break:(fun xs ys -> Set.length xs <> Set.length ys)
  @@ List.sort
       ~compare:(fun xs ys -> Int.compare (Set.length xs) (Set.length ys))
       xs
;;

let join t1 t2 = of_list @@ elim_supersets @@ to_list @@ union t1 t2

(* unused and wrong! *)
let meet t1 t2 = union t1 t2
