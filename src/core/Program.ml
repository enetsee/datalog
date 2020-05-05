open Lib

module X = struct
  type 'a t =
    { strata : 'a Stratum.t list
    ; queries : 'a Pred.t list
    }

  let pp pp_a ppf { strata; queries } =
    Fmt.(
      hovbox
      @@ pair
           ~sep:cut
           (list ~sep:cut @@ Stratum.pp pp_a)
           (list ~sep:cut @@ Pred.pp pp_a))
      ppf
      (strata, queries)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)
