open Core_kernel
open Lib
open Reporting

module Lit : sig
  type t =
    { pol : Polarity.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t [@compare.ignore]
    }

  include Lit.S with type t := t

  val lit : ?pol:Polarity.t -> ?region:Region.t -> Pred.t -> Term.t list -> t
end = struct
  type t =
    { pol : Polarity.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t [@compare.ignore]
    }
  [@@deriving compare, sexp]

  let lit ?(pol = Polarity.Pos) ?(region = Region.empty) pred terms =
    { pol; pred; terms; region }
  ;;

  (* -- Lit implementation -------------------------------------------------- *)
  let pol_of { pol; _ } = pol
  let pred_of { pred; _ } = pred
  let neg t = { t with pol = Polarity.toggle t.pol }

  (* -- HasEffects implementation ------------------------------------------- *)
  let effects_of { pred; _ } = Pred.effects_of pred

  (* -- HasTerms implementation --------------------------------------------- *)
  let terms_of { terms; _ } = terms

  (* -- HasVars implementation ---------------------------------------------- *)
  let vars_of { terms; _ } = List.concat_map ~f:Term.vars_of terms

  (* -- HasRegion implementation -------------------------------------------- *)
  let region_of { region; _ } = region

  (* -- Pretty implementation ----------------------------------------------- *)
  include Pretty.Make0 (struct
    type nonrec t = t

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
  end)
end

module Clause = Clause.Make (Lit)
module Program = Program.Make (Lit) (Clause)
module Dependency = Dependency.Make (Lit) (Clause) (Program)
