open Core_kernel

exception NotAPred of Name.t
exception UnboundPred of Name.t
exception UnboundParam of Name.t
exception UnboundData of Name.t

type pred_info =
  { typing : Typing.t
  ; nature : Nature.t
  ; cstr : Constraint.t
  }

type binding =
  | BParam of Ty.t
  | BData of TTC.t
  | BPred of pred_info

type t = binding Name.Map.t

let empty = Name.Map.empty
let add_ t ~name ~binding = Name.Map.add_exn t ~key:name ~data:binding
let add_param t ~name ~ty = add_ t ~name ~binding:(BParam ty)
let add_data t ~name ~ttc = add_ t ~name ~binding:(BData ttc)
let add_pred t ~name ~info = add_ t ~name ~binding:(BPred info)
let update_pred t ~name ~info = Name.Map.update t name ~f:Fn.(const info)

let update_pred_typing_exn t ~name ~typing =
  Name.Map.update t name ~f:(function
      | Some (BPred info) -> BPred { info with typing }
      | Some _ -> raise @@ NotAPred name
      | _ -> raise @@ UnboundPred name)
;;

let update_pred_constraint_exn t ~name ~cstr =
  Name.Map.update t name ~f:(function
      | Some (BPred info) -> BPred { info with cstr }
      | Some _ -> raise @@ NotAPred name
      | _ -> raise @@ UnboundPred name)
;;

let find_ bindings ~name = Name.Map.find bindings name

let find_param t ~name =
  Option.(
    find_ t ~name
    >>= function
    | BParam ty -> Some ty
    | _ -> None)
;;

let find_data t ~name =
  Option.(
    find_ t ~name
    >>= function
    | BData ttc -> Some ttc
    | _ -> None)
;;

let find_pred t ~name =
  Option.(
    find_ t ~name
    >>= function
    | BPred info -> Some info
    | _ -> None)
;;

let find_pred_constraint t ~name =
  Option.(
    find_ t ~name
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
    find_ t ~name
    >>= function
    | BPred { typing; _ } -> Some typing
    | _ -> None)
;;

let find_pred_nature t ~name =
  Option.(
    find_ t ~name
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
