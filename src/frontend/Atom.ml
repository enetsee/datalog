open Core_kernel
open Lib
open Reporting

module X = struct
  type 'a t =
    { predSym : Core.PredSymbol.t Located.t
    ; terms : 'a Located.t list
    ; nature : Core.ForeignFunc.t option [@compare.ignore]
    }
  [@@deriving compare]

  let equal eq_a { predSym = p1; terms = t1; _ } { predSym = p2; terms = t2; _ }
    =
    Located.equal Core.PredSymbol.equal p1 p2
    && List.equal (Located.equal eq_a) t1 t2
  ;;

  let atom predSym terms = { predSym; terms; nature = None }

  let pp pp_a ppf { predSym; terms; _ } =
    Fmt.(
      hovbox
      @@ pair (Located.pp Core.PredSymbol.pp)
      @@ parens
      @@ list ~sep:comma (Located.pp pp_a))
      ppf
      (predSym, terms)
  ;;

  let pp = `NoPrec pp
  let map t ~f = { t with terms = List.map ~f:(Located.map ~f) t.terms }
end

include X
include Pretty.Make1 (X)
include Functor.Make1 (X)

(* -- Query ----------------------------------------------------------------- *)

let tmvars (tmvars_a : 'a -> Core.Tmvar.t list) { terms; _ } =
  List.concat_map terms ~f:(fun Located.{ elem; _ } -> tmvars_a elem)
;;

(* -- Transformation -------------------------------------------------------- *)

let ffn_clash t ~ffns =
  Logger.(
    match Core.PredSymbol.Map.find ffns t.predSym.elem with
    | Some _ ->
      warn @@ predsym_clash_with_ffn t.predSym.region >>= fun () -> return t
    | _ -> return t)
;;

let set_foreign_func t ~ffns =
  Option.value_map ~default:t ~f:(fun ffn -> { t with nature = Some ffn })
  @@ Core.PredSymbol.Map.find ffns t.predSym.elem
;;
