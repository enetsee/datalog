open Core_kernel
open Lib
open Reporting

module type S = sig
  module Lit : Lit.S

  (** A `Clause` is made up of: 
      - a head `Lit`eral, the _conclusion_ of the clause; and 
      - a (non-empty) list of body `Lit`erals, each a _hypothesis_ or _premise_ 
        of the clause. 
  *)
  type t =
    { head : Lit.t
    ; body : Lit.t list
    ; region : Region.t [@compare.ignore]
    }
  [@@deriving compare, sexp]

  include HasRegion.S with type t := t
  include HasTerms.S with type t := t
  include HasPreds.S with type t := t
  include HasVars.S with type t := t
  include Pretty.S0 with type t := t

  val clause : ?region:Region.t -> Lit.t -> Lit.t list -> t
  val head_of : t -> Lit.t
  val head_pred_of : t -> Pred.t
  val body_of : t -> Lit.t list

  module Map : Map.S with type Key.t := t
end

module Make (Lit : Lit.S) : S with module Lit := Lit = struct
  module Minimal = struct
    type t =
      { head : Lit.t
      ; body : Lit.t list
      ; region : Region.t [@compare.ignore]
      }
    [@@deriving compare, sexp]

    let clause ?(region = Region.empty) head body = { head; body; region }
    let head_of { head; _ } = head
    let head_pred_of { head; _ } = Lit.pred_of head
    let body_of { body; _ } = body
    let region_of { region; _ } = region

    (** The predicates in the head and body of the clause *)
    let preds_of { body; head; _ } =
      let hdpred = Lit.pred_of head
      and bodypreds = List.map ~f:Lit.pred_of body in
      List.dedup_and_sort ~compare:Pred.compare (hdpred :: bodypreds)
    ;;

    (** All terms contained in the head and body of the clause *)
    let terms_of { head; body; _ } =
      Lit.terms_of head @ List.concat_map ~f:Lit.terms_of body
    ;;

    (** All term variables of a clause; assumes clause is range-restricted *)
    let vars_of { body; _ } = List.concat_map ~f:Lit.vars_of body

    let pp ppf { head; body; _ } =
      Fmt.(
        hovbox
        @@ pair ~sep:(any " :-@, ") Lit.pp
        @@ hovbox
        @@ list ~sep:comma Lit.pp)
        ppf
        (head, body)
    ;;

    let pp = `NoPrec pp
  end

  include Minimal
  include Pretty.Make0 (Minimal)
  module Map = Map.Make (Minimal)
end
