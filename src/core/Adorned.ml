open Core_kernel
open Lib
open Reporting

module Lit = struct
  module Minimal = struct
    type t =
      { pol : Polarity.t
      ; pred : Pred.t
      ; bpatt : BindingPatt.t
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
             @@ BindingPatt.pp)
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (pol, ((pred, bpatt), terms))
    ;;

    let pp = `NoPrec pp
  end

  include Minimal

  let from_raw Raw.Lit.{ pol; pred; terms; region } ~bpatt =
    { pol; pred; bpatt; terms; region }
  ;;

  let bpatt_of { bpatt; _ } = bpatt

  (** A literal is well moded when its terms are compatible 
      at least one possible mode constraint *)
  let well_moded { bpatt; _ } ~cnstr = BindingPatt.well_moded bpatt ~cnstr

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

module Clause = struct
  include Clause.Make (Lit)

  let well_moded { body; _ } ~cstrs =
    List.for_all body ~f:(fun lit ->
        let cnstr =
          Option.value ~default:Constraint.trivial
          @@ Pred.Map.find cstrs
          @@ Lit.pred_of lit
        in
        Lit.well_moded lit ~cnstr)
  ;;
end

module Program = struct
  include Program.Make (Lit) (Clause)

  let well_moded { clauses; _ } ~cstrs =
    List.for_all clauses ~f:Clause.(well_moded ~cstrs)
  ;;
end

module Dependency = Dependency.Make (Lit) (Clause) (Program)
