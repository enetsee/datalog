open Core_kernel
open Lib

module type S = sig
  module Lit : Lit.S
  module Clause : Clause.S with module Lit := Lit

  type t =
    { clauses : Clause.t list
    ; queries : Pred.t list
    ; cnstrts : Constraint.t PredSymbol.Map.t
    }

  include HasPreds.S with type t := t
  include Pretty.S0 with type t := t

  val clauses_of : t -> Clause.t list
  val queries_of : t -> Pred.t list
  val constraints_of : t -> Constraint.t PredSymbol.Map.t
  val intensionals : t -> Pred.Set.t
  val extensionals : t -> Pred.Set.t
end

module Make (Lit : Lit.S) (Clause : Clause.S with module Lit := Lit) :
  S with module Lit := Lit and module Clause := Clause = struct
  type t =
    { clauses : Clause.t list
    ; queries : Pred.t list
    ; cnstrts : Constraint.t PredSymbol.Map.t
    }

  (** All predicates in a program *)
  let preds_of { clauses; _ } =
    List.dedup_and_sort ~compare:Pred.compare
    @@ List.concat_map ~f:Clause.preds_of clauses
  ;;

  let clauses_of { clauses; _ } = clauses
  let queries_of { queries; _ } = queries
  let constraints_of { cnstrts; _ } = cnstrts

  (** Intensional predicates, i.e. those appearing in the head of a clause *)
  let intensionals { clauses; _ } =
    Pred.Set.of_list @@ List.map ~f:(fun cl -> Clause.head_pred_of cl) clauses
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
             (prefix (any "Clauses") @@ prefix cut @@ list ~sep:cut @@ Clause.pp)
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
