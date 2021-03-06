open Core_kernel
open Lib
open Core

exception NotAPred of Name.t
exception UnboundPred of Name.t
exception UnboundParam of Name.t
exception UnboundData of Name.t

type pred_info =
  { typing : Type.Typing.t
  ; nature : Nature.t
  ; cstr : Constraint.t
  }
[@@deriving eq, compare]

let empty_info =
  { typing = Type.Typing.empty
  ; nature = Nature.Logical
  ; cstr = Constraint.trivial
  }
;;

type binding =
  | BParam of Type.Ty.t
  | BData of Type.TTC.t
  | BPred of pred_info
[@@deriving eq, compare]

type t = { bindings : binding Name.Map.t } [@@deriving eq, compare]

include Pretty.Make0 (struct
  type nonrec t = t

  let arr = Fmt.any "@;:@;"
  let pp_param_binding = Fmt.(hbox @@ pair ~sep:arr Name.pp Type.Ty.pp)
  let pp_data_binding = Fmt.(hbox @@ pair ~sep:arr Name.pp Type.TTC.pp)

  let pp_pred_binding ppf (name, { typing; nature; cstr }) =
    match nature, cstr with
    | Nature.Logical, _ when Constraint.is_trivial cstr ->
      Fmt.(hovbox @@ pair ~sep:arr Name.pp Type.Typing.pp) ppf (name, typing)
    | Logical, _ ->
      Fmt.(hovbox @@ pair ~sep:arr (pair Name.pp Constraint.pp) Type.Typing.pp)
        ppf
        ((name, cstr), typing)
    | Extralogical effs, _ when Constraint.is_trivial cstr ->
      Fmt.(
        hovbox
        @@ pair
             ~sep:(any "@;:@;")
             (pair Name.pp @@ brackets @@ list ~sep:comma Eff.pp)
             Type.Typing.pp)
        ppf
        ((name, effs), typing)
    | Extralogical effs, _ ->
      Fmt.(
        hovbox
        @@ pair
             ~sep:(any "@;:@;")
             (pair Name.pp
             @@ pair (brackets @@ list ~sep:comma Eff.pp) Constraint.pp)
             Type.Typing.pp)
        ppf
        ((name, (effs, cstr)), typing)
  ;;

  let collect { bindings } =
    let rec aux (params, data, preds) = function
      | (nm, BParam ty) :: rest -> aux ((nm, ty) :: params, data, preds) rest
      | (nm, BData ttc) :: rest -> aux (params, (nm, ttc) :: data, preds) rest
      | (nm, BPred pinfo) :: rest ->
        aux (params, data, (nm, pinfo) :: preds) rest
      | _ -> params, data, preds
    in
    aux ([], [], []) @@ Map.to_alist bindings
  ;;

  let pp_params ppf =
    Fmt.(
      vbox
      @@ prefix (any "parameters@;@.")
      @@ suffix cut
      @@ vbox
      @@ list ~sep:cut pp_param_binding)
      ppf
  ;;

  let pp_data ppf =
    Fmt.(
      vbox
      @@ prefix (any "@.data@;@.")
      @@ suffix cut
      @@ vbox
      @@ list ~sep:cut pp_data_binding)
      ppf
  ;;

  let pp_relations ppf =
    Fmt.(
      vbox
      @@ prefix (any "@.relations@;@.")
      @@ suffix cut
      @@ vbox
      @@ list ~sep:cut pp_pred_binding)
      ppf
  ;;

  let pp ppf t =
    let params, data, preds = collect t in
    Fmt.(vbox @@ pair ~sep:cut pp_params @@ pair ~sep:cut pp_data pp_relations)
      ppf
      (params, (data, preds))
  ;;

  let pp = `NoPrec pp
end)

let empty = { bindings = Name.Map.empty }

let add_ { bindings } ~name ~binding =
  { bindings = Name.Map.add_exn bindings ~key:name ~data:binding }
;;

let add_param t ~name ~ty = add_ t ~name ~binding:(BParam ty)
let add_data t ~name ~ttc = add_ t ~name ~binding:(BData ttc)
let add_pred t ~name ~info = add_ t ~name ~binding:(BPred info)

let add_params t ~params =
  List.fold_left ~init:t params ~f:(fun t (name, ty) -> add_param t ~name ~ty)
;;

let add_datas t ~datas =
  List.fold_left ~init:t datas ~f:(fun t (name, ttc) -> add_data t ~name ~ttc)
;;

let add_preds t ~preds =
  List.fold_left ~init:t preds ~f:(fun t (name, info) -> add_pred t ~name ~info)
;;

(** initialize a typing environment with:
  - declared parameters 
  - declared data 
  - predicates with optional type info 
*)
let typing_env ~params ~datas ~preds =
  add_preds
    ~preds:
      (List.map preds ~f:(fun (nm, info_opt) ->
           nm, Option.value ~default:empty_info info_opt))
  @@ add_datas ~datas
  @@ add_params ~params empty
;;

let update_pred { bindings } ~name ~info =
  { bindings = Name.Map.update bindings name ~f:Fn.(const info) }
;;

let update_pred_typing_exn { bindings } ~name ~typing =
  { bindings =
      Name.Map.update bindings name ~f:(function
          | Some (BPred info) -> BPred { info with typing }
          | Some _ -> raise @@ NotAPred name
          | _ -> raise @@ UnboundPred name)
  }
;;

let update_pred_constraint_exn { bindings } ~name ~cstr =
  { bindings =
      Name.Map.update bindings name ~f:(function
          | Some (BPred info) -> BPred { info with cstr }
          | Some _ -> raise @@ NotAPred name
          | _ -> raise @@ UnboundPred name)
  }
;;

let find_name { bindings } ~name = Name.Map.find bindings name

let find_param t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BParam ty -> Some ty
    | _ -> None)
;;

let find_data t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BData ttc -> Some ttc
    | _ -> None)
;;

let find_pred t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BPred info -> Some info
    | _ -> None)
;;

let find_pred_constraint t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BPred { cstr; _ } -> Some cstr
    | _ -> None)
;;

let find_pred_constraint_exn t ~name =
  match find_pred_constraint t ~name with
  | Some cstr -> cstr
  | _ -> raise @@ UnboundPred name
;;

let find_pred_typing t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BPred { typing; _ } -> Some typing
    | _ -> None)
;;

let find_pred_nature t ~name =
  Option.(
    find_name t ~name
    >>= function
    | BPred { nature; _ } -> Some nature
    | _ -> None)
;;

let find_pred_effects t ~name =
  Option.(
    find_pred_nature t ~name
    >>= function
    | Nature.Extralogical effs -> Some (Eff.Set.of_list effs)
    | _ -> Some Eff.Set.empty)
;;

let find_pred_effects_exn t ~name =
  match find_pred_effects t ~name with
  | Some eff -> eff
  | _ -> raise @@ UnboundPred name
;;
