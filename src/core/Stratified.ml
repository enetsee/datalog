open Core_kernel
open Lib

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
type t =
  { strata : Adorned.Clause.t list list
  ; queries : Pred.t list
  }
[@@deriving sexp]

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { strata; _ } =
    Fmt.(vbox @@ list ~sep:cut @@ list ~sep:cut Adorned.Clause.pp) ppf strata
  ;;

  let pp = `NoPrec pp
end)
