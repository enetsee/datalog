open Core_kernel
open Reporting
open Lib

type t =
  | SClause of
      { head : Head.Term.t
      ; body : Body.t
      ; region : Region.t
      }
  | SFact of Head.Symbol.t
  | STy of
      { name : Core.Name.t Located.t
      ; defn : Core.Ty.t Located.t
      ; region : Region.t
      }
  | SData of
      { name : Core.Name.t Located.t
      ; attrs : (string * Core.Ty.t Located.t) list
      ; region : Region.t
      }
  | SParam of
      { name : Core.Name.t Located.t
      ; ty : Core.Ty.t Located.t
      ; region : Region.t
      }
  | SExport of Core.Name.t Located.t

(* -- Helpers --------------------------------------------------------------- *)
let clause ?region head body =
  let region =
    Option.value
      region
      ~default:Region.(merge Head.Term.(region head) Body.(region body))
  in
  SClause { head; body; region }
;;

let fact head = SFact head
let export pred = SExport pred

let param ?region name ty =
  let region =
    Option.value
      region
      ~default:Region.(merge Located.(region_of name) Located.(region_of ty))
  in
  SParam { name; ty; region }
;;

let data name attrs =
  let region =
    match List.last attrs with
    | Some (_, ty) ->
      Region.merge Located.(region_of name) Located.(region_of ty)
    | _ -> Located.(region_of name)
  in
  SData { name; attrs; region }
;;

let tydefn ?region name defn =
  let region =
    Option.value
      region
      ~default:Region.(merge Located.(region_of name) Located.(region_of defn))
  in
  STy { name; defn; region }
;;

(* -- Transformation -------------------------------------------------------- *)

let normalize = function
  | SClause ({ body; _ } as t) ->
    List.map ~f:(fun body -> SClause { t with body }) @@ Body.normalize body
  | t -> [ t ]
;;

(* -- HasCoreRepr implementation -------------------------------------------- *)

type repr =
  | RCls of Core.Clause.Raw.t
  | RKnw of Core.Knowledge.t
  | RTy of Core.Name.t * Core.Ty.t
  | RData of Core.Name.t * Core.Typing.t
  | RParam of Core.Name.t * Core.Ty.t
  | RExport of Core.Name.t

let to_core = function
  | SClause { head; body; region } ->
    Result.combine
      Head.Term.(to_core head)
      Body.(to_core body)
      ~ok:(fun head body -> RCls Core.Clause.Raw.{ head; body; region })
      ~err:Fn.const
  | SFact head -> Result.map ~f:(fun k -> RKnw k) @@ Head.Symbol.to_core head
  | STy { name; defn; _ } ->
    Result.return @@ RTy (Located.(elem_of name), Located.elem_of defn)
  | SData { name; attrs; _ } ->
    let name = Located.elem_of name
    and typing =
      Core.Typing.of_schema
      @@ List.map attrs ~f:(fun (_, ty) -> Located.elem_of ty)
    in
    Result.return (RData (name, typing))
  | SParam { name; ty; _ } ->
    Result.return @@ RParam (Located.(elem_of name), Located.elem_of ty)
  | SExport pred -> Result.return @@ RExport Located.(elem_of pred)
;;

(* -- Pretty implementation ----------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp_attr ppf (v, ty) =
    Fmt.(hbox @@ pair ~sep:(any " : ") string (Located.pp Core.Ty.pp))
      ppf
      (v, ty)
  ;;

  let pp ppf = function
    | SClause { head; body; _ } ->
      Fmt.(
        hovbox
        @@ suffix (any ".")
        @@ pair ~sep:(any " :-@; ") Head.Term.pp Body.pp)
        ppf
        (head, body)
    | SFact head -> Fmt.(hovbox @@ suffix (any ".") @@ Head.Symbol.pp) ppf head
    | STy { name; defn; _ } ->
      Fmt.(
        hbox
        @@ pair ~sep:(any " <: ") (any "type @" ++ Located.pp Core.Name.pp)
        @@ Located.pp Core.Ty.pp)
        ppf
        (name, defn)
    | SData { name; attrs; _ } ->
      Fmt.(
        hovbox
        @@ prefix (any "data@;")
        @@ pair
             (Located.pp Core.Name.pp)
             (hovbox @@ parens @@ list ~sep:comma pp_attr))
        ppf
        (name, attrs)
    | SParam { name; ty; _ } ->
      Fmt.(
        hovbox
        @@ pair ~sep:(any "@;:@;") (any "param $" ++ Located.pp Core.Name.pp)
        @@ Located.pp Core.Ty.pp)
        ppf
        (name, ty)
    | SExport pred ->
      Fmt.(prefix (any "export ") @@ Located.pp Core.Name.pp) ppf pred
  ;;

  let pp = `NoPrec pp
end)
