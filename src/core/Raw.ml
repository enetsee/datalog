open Core_kernel
open Lib
open Reporting

module Lit : sig
  exception MismatchArity of Pred.Name.t * int * int

  type t =
    { pol : Polarity.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t [@compare.ignore] [@equal.ignore]
    }

  include Lit.S with type t := t

  val lit : ?pol:Polarity.t -> ?region:Region.t -> Pred.t -> Term.t list -> t
end = struct
  exception MismatchArity of Pred.Name.t * int * int

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

  (* -- HasEffects implementation ------------------------------------------- *)
  let effects_of { pred; _ } = Pred.effects_of pred

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

module Clause = Clause.Make (Lit)
module Program = Program.Make (Lit) (Clause)
module Dependency = Dependency.Make (Lit) (Clause) (Program)
