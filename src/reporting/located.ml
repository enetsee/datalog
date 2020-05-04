open Lib

module Minimal = struct
  type 'a t =
    { elem : 'a
    ; region : Region.t
    }
  [@@deriving map]

  let empty elem = { elem; region = Region.empty }

  let merge elem { region = start_; _ } { region = end_; _ } =
    { elem; region = Region.merge start_ end_ }
  ;;

  let pp_prec prec pp_a ppf { elem; _ } = pp_a prec ppf elem
  let pp = `WithPrec pp_prec
  let map t ~f = map f t
  let equal eq_a { elem = a; _ } { elem = b; _ } = eq_a a b
  let compare cmp_a { elem = a; _ } { elem = b; _ } = cmp_a a b
end

include Minimal
include Pretty.Make1 (Minimal)
include Functor.Make1 (Minimal)
