open Core_kernel
open Core
open Lib

module X = struct
  (** A workitem represents a unique predicate / binding pattern pair for whic
        we must generate adorned clauses *)
  type t = Pred.t * Binding.t [@@deriving compare, sexp]

  let pp ppf (p, bp) =
    Fmt.(hbox @@ pair Pred.pp @@ braces @@ Binding.pp) ppf (p, bp)
  ;;

  let pp = `NoPrec pp
  let from_literal Lit.Adorned.{ pred; bpatt; _ } = pred, bpatt

  let from_clause Clause.Adorned.{ body; _ } =
    List.dedup_and_sort ~compare @@ List.map ~f:from_literal body
  ;;
end

include X
include Pretty.Make0 (X)
module Set = Set.Make (X)
