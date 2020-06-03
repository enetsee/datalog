open Core_kernel
open Lib
open Reporting

module Raw = struct
  module Minimal = struct
    type t =
      { head : Lit.Raw.t
      ; body : Lit.Raw.t list
      ; region : Region.t [@compare.ignore]
      }
    [@@deriving compare, sexp]

    let head_of { head; _ } = head
    let body_of { body; _ } = body

    (** The predicates in the head and body of the clause *)
    let preds_of { body; head = Lit.Raw.{ pred = hdpred; _ }; _ } =
      let bodypreds = List.map ~f:(fun Lit.Raw.{ pred; _ } -> pred) body in
      List.dedup_and_sort ~compare:Pred.compare (hdpred :: bodypreds)
    ;;

    (** All terms contained in the head and body of the clause *)
    let terms_of { head; body; _ } =
      Lit.Raw.terms_of head @ List.concat_map ~f:Lit.Raw.terms_of body
    ;;

    (** All term variables of a clause; assumes clause is range-restricted *)
    let vars_of { body; _ } = List.concat_map ~f:Lit.Raw.vars_of body

    let pp ppf { head; body; _ } =
      Fmt.(
        hovbox
        @@ pair ~sep:(any " :-@, ") Lit.Raw.pp
        @@ hovbox
        @@ list ~sep:comma Lit.Raw.pp)
        ppf
        (head, body)
    ;;

    let pp = `NoPrec pp
  end

  include Minimal
  include Pretty.Make0 (Minimal)
  module Map = Map.Make (Minimal)
end
