open Core_kernel
open Lib

module Raw = struct
  type t =
    { clauses : Clause.Raw.t list
    ; queries : Pred.t list
    ; cnstrts : Constraint.t PredSymbol.Map.t
    }

  (** All predicates in a program *)
  let preds_of { clauses; _ } =
    List.dedup_and_sort ~compare:Pred.compare
    @@ List.concat_map ~f:Clause.Raw.preds_of clauses
  ;;

  let clauses_of { clauses; _ } = clauses
  let queries_of { queries; _ } = queries
  let constraints_of { cnstrts; _ } = cnstrts

  (** Intensional predicates, i.e. those appearing in the head of a clause *)
  let intensionals { clauses; _ } =
    Pred.Set.of_list
    @@ List.map
         ~f:(fun Clause.Raw.{ head = Lit.Raw.{ pred; _ }; _ } -> pred)
         clauses
  ;;

  (** Intensional predicates, i.e. those appearing only in the body of a clause *)
  let extensionals prog =
    Pred.Set.diff (Pred.Set.of_list @@ preds_of prog) (intensionals prog)
  ;;


    

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { clauses; queries; cnstrts } =
      Fmt.(
        vbox
        @@ pair
             ~sep:cut
             (prefix (any "Clauses")
             @@ prefix cut
             @@ list ~sep:cut
             @@ Clause.Raw.pp)
        @@ pair
             ~sep:cut
             (prefix (any "Queries") @@ prefix cut @@ list ~sep:cut @@ Pred.pp)
             (prefix (any "Constraints")
             @@ prefix cut
             @@ list ~sep:cut
             @@ hbox
             @@ pair ~sep:(any " => ") PredSymbol.pp Constraint.pp))
        ppf
        (clauses, (queries, PredSymbol.Map.to_alist cnstrts))
    ;;

    let pp = `NoPrec pp
  end)
end

(* 
module Stratified = struct
  module X = struct
    type t =
      { strata : Clause.Raw.t list list
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
end *)
