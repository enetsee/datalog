open Lib
open Reporting
open Core_kernel

module type S = sig
  type t [@@deriving compare, sexp, eq]

  include Pretty.S0 with type t := t
  include HasVars.S with type t := t
  include HasTerms.S with type t := t
  include HasRegion.S with type t := t

  val pred_of : t -> Pred.t
  val pol_of : t -> Polarity.t
  val neg : t -> t

  module Set : Set.S with type Elt.t := t
end

module Raw : sig
  exception MismatchArity of Name.t * int * int

  type t =
    { pol : Polarity.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t [@compare.ignore] [@equal.ignore]
    }

  include S with type t := t

  val lit : ?pol:Polarity.t -> ?region:Region.t -> Pred.t -> Term.t list -> t
end = struct
  exception MismatchArity of Name.t * int * int

  module Minimal = struct
    type t =
      { pol : Polarity.t
      ; pred : Pred.t
      ; terms : Term.t list
      ; region : Region.t [@compare.ignore] [@equal.ignore]
      }
    [@@deriving compare, sexp, eq]

    let pp ppf { pol; pred; terms; _ } =
      Fmt.(
        hbox
        @@ pair Polarity.pp
        @@ pair Pred.pp
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (pol, (pred, terms))
    ;;

    let pp = `NoPrec pp
  end

  include Minimal

  let lit ?(pol = Polarity.Pos) ?(region = Region.empty) pred terms =
    let arity = Pred.arity_of pred
    and actual = List.length terms in
    if arity = actual
    then { pol; pred; terms; region }
    else raise (MismatchArity (Pred.name_of pred, arity, actual))
  ;;

  (* -- Lit implementation -------------------------------------------------- *)
  let pol_of { pol; _ } = pol
  let pred_of { pred; _ } = pred
  let neg t = { t with pol = Polarity.toggle t.pol }

  (* -- HasTerms implementation --------------------------------------------- *)
  let terms_of { terms; _ } = terms

  (* -- HasVars implementation ---------------------------------------------- *)
  let vars_of { terms; _ } = List.concat_map ~f:Term.vars_of terms

  (* -- HasRegion implementation -------------------------------------------- *)
  let region_of { region; _ } = region

  (* -- Pretty implementation ----------------------------------------------- *)
  include Pretty.Make0 (Minimal)
  module Set = Set.Make (Minimal)
end

module Adorned = struct
  module Minimal = struct
    type t =
      { pol : Polarity.t
      ; pred : Pred.t
      ; bpatt : Binding.t
      ; terms : Term.t list
      ; region : Region.t [@compare.ignore] [@equal.ignore]
      }
    [@@deriving compare, sexp, eq]

    let pp ppf { pol; pred; bpatt; terms; _ } =
      Fmt.(
        hbox
        @@ pair Polarity.pp
        @@ pair
             (pair Pred.pp
             @@ hbox
             @@ prefix (any "<")
             @@ suffix (any ">")
             @@ Binding.pp)
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (pol, ((pred, bpatt), terms))
    ;;

    let pp = `NoPrec pp
  end

  include Minimal

  let from_raw Raw.{ pol; pred; terms; region } ~bpatt =
    { pol; pred; bpatt; terms; region }
  ;;

  let bpatt_of { bpatt; _ } = bpatt

  (** A literal is well moded when its terms are compatible 
      at least one possible mode constraint *)

  (* let well_moded { bpatt; _ } ~cnstr = Binding.well_moded bpatt ~cnstr *)

  (* -- Lit implementation -------------------------------------------------- *)
  let pol_of { pol; _ } = pol
  let pred_of { pred; _ } = pred
  let neg t = { t with pol = Polarity.toggle t.pol }

  (* -- HasTerms implementation --------------------------------------------- *)
  let terms_of { terms; _ } = terms

  (* -- HasVars implementation ---------------------------------------------- *)
  let vars_of { terms; _ } = List.concat_map ~f:Term.vars_of terms

  (* -- HasRegion implementation -------------------------------------------- *)
  let region_of { region; _ } = region

  (* -- Pretty implementation ----------------------------------------------- *)
  include Pretty.Make0 (Minimal)
  module Set = Set.Make (Minimal)
end
