open Core_kernel
open Lib

module type S = sig
  module Lit : Lit.S
  module Clause : Clause.S with module Lit := Lit

  type t

  include Pretty.S0 with type t := t
  include HasPreds.S with type t := t

  val sorted : t -> t
  val clauses_of : t -> Clause.t list
  val intensionals : t -> Pred.Set.t
  val extensionals : t -> Pred.Set.t
end

module Unstratified = struct
  module type S = sig
    module Lit : Lit.S
    module Clause : Clause.S with module Lit := Lit

    type t = { clauses : Clause.t list } [@@deriving compare, eq]

    val program : Clause.t list -> t

    include S with module Lit := Lit and module Clause := Clause and type t := t
  end

  module Make (Lit : Lit.S) (Clause : Clause.S with module Lit := Lit) :
    S with module Lit := Lit and module Clause := Clause = struct
    type t = { clauses : Clause.t list } [@@deriving compare, eq]

    (** Make a program  with standard ordering of queries and clauses 
    *)
    let program clauses = { clauses }

    let sorted { clauses } =
      { clauses = List.sort ~compare:Clause.compare clauses }
    ;;

    (** All predicates in a program *)
    let preds_of { clauses; _ } =
      List.dedup_and_sort ~compare:Pred.compare
      @@ List.concat_map ~f:Clause.preds_of clauses
    ;;

    let clauses_of { clauses; _ } = clauses

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

      let pp ppf { clauses } = Fmt.(vbox @@ list ~sep:cut Clause.pp) ppf clauses
      let pp = `NoPrec pp
    end)
  end
end

module Raw = Unstratified.Make (Lit.Raw) (Clause.Raw)

module Adorned = struct
  include Unstratified.Make (Lit.Adorned) (Clause.Adorned)

  (* let well_moded { clauses; _ } ~cstrs =
       List.for_all clauses ~f:Clause.Adorned.(well_moded ~cstrs)
     ;; *)
end

module Stratified : sig
  type t = { strata : Clause.Adorned.t list list }
  [@@deriving compare, eq, sexp]

  include
    S
      with module Lit := Lit.Adorned
       and module Clause := Clause.Adorned
       and type t := t

  val strata_of : t -> Clause.Adorned.t list list
end = struct
  (** A _stratified_ datalog program `D` is a partition of `D` into a _sequence 
    of adorned programs_ (strata) `D1,...,Dn` such that, given a function 
    `s : Predicate -> Int` mapping predicates to their stratum, we have:

    - For each predicate `P`, all the clauses in `P` defining `P` are in the 
      same stratum

    - For each clause in `D` of the form `r(u) :- ... , r'(v), ... ` then if 
      `r'` is an intensional predicate, `s(r') <= s(r)` i.e. predicates 
      appearing as positive literals in the body of a clause must must be in the 
      same stratum or strata before the predicate in the head of the clause.

    - For each clause in `D` of the form `r(u) :- ... , not r'(v), ... ` then if 
      `r'` is an intensional predicate, `s(r') < s(r)` i.e. predicates appearing 
      as negative literals in the body of a clause must must be strata strictly 
      before the predicate in the head of the clause.

    A datalog program is stratifiable if its _precedence graph_ has no cycles 
    containing a negative edge
*)
  type t = { strata : Clause.Adorned.t list list }
  [@@deriving compare, eq, sexp]

  let sorted { strata } =
    { strata = List.(map ~f:(sort ~compare:Clause.Adorned.compare) strata) }
  ;;

  let strata_of { strata; _ } = strata
  let clauses_of { strata; _ } = List.concat strata

  let preds_of t =
    List.dedup_and_sort ~compare:Pred.compare
    @@ List.concat_map ~f:Clause.Adorned.preds_of
    @@ clauses_of t
  ;;

  (** Intensional predicates, i.e. those appearing in the head of a clause *)
  let intensionals t =
    Pred.Set.of_list
    @@ List.map ~f:(fun cl -> Clause.Adorned.head_pred_of cl)
    @@ clauses_of t
  ;;

  (** Intensional predicates, i.e. those appearing only in the body of a clause *)
  let extensionals prog =
    Pred.Set.diff (Pred.Set.of_list @@ preds_of prog) (intensionals prog)
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { strata; _ } =
      Fmt.(
        vbox
        @@ list ~sep:(suffix cut cut)
        @@ pair ~sep:cut (prefix (any "stratum ") int)
        @@ list ~sep:cut Clause.Adorned.pp)
        ppf
        (List.mapi ~f:(fun i cls -> i, cls) strata)
    ;;

    let pp = `NoPrec pp
  end)
end
