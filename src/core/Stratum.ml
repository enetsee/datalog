open Lib

module X = struct
  type 'a t = { stratum : 'a Clause.t list }

  let pp pp_a ppf { stratum } =
    Fmt.(hovbox @@ list ~sep:cut (Clause.pp pp_a)) ppf stratum
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)
