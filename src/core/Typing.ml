open Core_kernel
open Lib

module Minimal = struct
  (** A typing is a set of non-degenerate type tuple constraints *)
  include Set.Make (TTC)

  let arity_of t = Option.map ~f:TTC.arity_of @@ List.hd @@ to_list t

  let pp ppf t =
    Fmt.(hvbox @@ braces @@ list ~sep:comma TTC.pp) ppf @@ to_list t
  ;;

  let pp = `NoPrec pp

  let of_list ttcs =
    of_list @@ List.filter ttcs ~f:(fun x -> not @@ TTC.is_degenerate x)
  ;;

  let singleton ttc = of_list [ ttc ]

  let of_schema tys =
    singleton @@ TTC.{ tys; equiv = Partition.trivial @@ List.length tys }
  ;;

  let bottom = empty
  let top n = singleton @@ TTC.top n
  let join t1 t2 = union t1 t2

  let product t1 t2 =
    let xs, ys = to_list t1, to_list t2 in
    of_list @@ List.(xs >>= fun x -> ys >>= fun y -> return @@ TTC.product x y)
  ;;

  let project t ~flds = of_list @@ List.map ~f:(TTC.project ~flds) @@ to_list t
end

include Minimal
include Pretty.Make0 (Minimal)
