open Core_kernel
open Lib
open Reporting

type t =
  { dest : Dataflow.Dest.t
  ; tmvar : Tmvar.t
  ; region : Region.t [@compare.ignore]
  }
[@@deriving compare, eq]

let violation ?(region = Region.empty) dest tmvar = { dest; tmvar; region }

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { dest; tmvar; _ } =
    Fmt.(hbox @@ pair ~sep:sp Dataflow.Dest.pp (quote Tmvar.pp))
      ppf
      (dest, tmvar)
  ;;

  let pp = `NoPrec pp
end)

(** The (potential) range-restriction violations of a clause are the 
    variables appearing in the head of the clause which do not appear in any 
    _positive_ literal in the body of the clause *)
let of_clause Clause.Raw.{ head; body; _ } =
  let head_pred = Lit.Raw.pred_of head
  and body_vars =
    Tmvar.Set.of_list
    @@ List.concat_map body ~f:(fun (Lit.Raw.{ pol; _ } as lit) ->
           if Polarity.isPos pol then Lit.Raw.vars_of lit else [])
  in
  List.filter_mapi (Lit.Raw.terms_of head) ~f:(fun idx ->
    function
    | Term.TVar (v, region) when Tmvar.Set.(not @@ mem body_vars v) ->
      let dest = Dataflow.Dest.DPred (head_pred, idx) in
      Some (violation ~region dest v)
    | _ -> None)
;;
