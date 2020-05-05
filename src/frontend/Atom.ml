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

(* -- Core translation ------------------------------------------------------ *)

(** Translate to core representation of `Nature` guarding against mismatched
    arity *)
let translate_nature nature nargs region =
  Logger.(
    match nature with
    | Some Core.ForeignFunc.{ arity; _ } when arity <> nargs ->
      fail @@ ffn_wrong_arity ~actual:nargs ~expect:arity region
    | Some ff -> return @@ Core.Nature.ExtraLogical ff
    | None -> return Core.Nature.Logical)
;;

(** Translate an `Atom` to it's corresponding representation in the `Core`
    language i.e. a `Literal` 
*)
let to_core term_to_core { predSym = Located.{ elem; region }; terms; nature } =
  Logger.(
    List.map ~f:Fn.(compose term_to_core Located.elem_of) terms
    |> all
    >>= fun terms ->
    let arity = List.length terms in
    translate_nature nature arity region
    >>= fun nature ->
    Core.(
      let predSym = elem
      and annot = Annotation.base region in
      let pred = Pred.{ annot; predSym; arity; nature } in
      return Literal.{ annot; polarity = Pos; pred; terms }))
;;
