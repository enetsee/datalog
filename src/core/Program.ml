open Lib


module Unstratified = struct

  module X = struct
    type 'a t =
      { clauses : 'a Clause.t list
      ; queries : 'a Pred.t list
      ; cnstrts : Constraint.t PredSymbol.Map.t 
      }

    let pp pp_a ppf { clauses; queries; cnstrts } =
      Fmt.(
        vbox
        @@ pair
             ~sep:cut
             (prefix (any "Clauses") @@ prefix cut @@ list ~sep:cut @@ Clause.pp pp_a)
        @@ pair ~sep:cut     
          (prefix (any "Queries") @@ prefix cut @@ list ~sep:cut @@ Pred.pp pp_a)
          (prefix (any "Constraints") @@ prefix cut @@ list ~sep:cut @@ hbox @@ pair ~sep:(any " => ") PredSymbol.pp Constraint.pp)
        )
        ppf
        (clauses, (queries,PredSymbol.Map.to_alist cnstrts))
    ;;

    let pp = `NoPrec pp
  end

  include X
  include Pretty.Make1 (X)

end



module Stratified = struct
  module X = struct
    type 'a t =
      { strata : 'a Stratum.t list
      ; queries : 'a Pred.t list
      }

    let pp pp_a ppf { strata; queries } =
      Fmt.(
        vbox
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
end
