open Core_kernel
open Lib

(** A typing is a set of non-degenerate type tuple constraints *)
include Set.Make (TTC)

let arity_of t = Option.map ~f:TTC.arity_of @@ List.hd @@ to_list t

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf t =
    Fmt.(hvbox @@ braces @@ list ~sep:comma TTC.pp) ppf @@ to_list t
  ;;

  let pp = `NoPrec pp
end)

let of_list ttcs =
  of_list @@ List.filter ttcs ~f:(fun x -> not @@ TTC.is_degenerate x)
;;

let singleton ttc = of_list [ ttc ]

let of_schema tys =
  singleton @@ TTC.{ tys; equiv = Partition.trivial @@ List.length tys }
;;

let bottom = empty

let top n = singleton @@ TTC.top n

(**  T1 /\\ T2 = { t1 /\\ t2 | t1 in T1, t2 in T2, not degenerate t1 /\\ t2 } *)
let meet t1 t2 ~trg =
  let meet = TTC.meet ~trg 
  and xs, ys= to_list t1 , to_list t2 in
  of_list
  @@ List.( xs >>= fun x -> ys >>= fun y -> return @@ meet x y)
;;

let join t1 t2 = union t1 t2


let product t1 t2 =
  let xs, ys= to_list t1 , to_list t2 in
  of_list
  @@ List.(
       xs >>= fun x -> ys >>= fun y -> return @@ TTC.product x y)
;;

let project t ~flds = of_list @@ List.map ~f:(TTC.project ~flds) @@ to_list t

let interpret rel ~trg ~edb ~stratum =
  let algebra = function
    | Relation.F.RPred pred ->
      Option.(
        value ~default:bottom
        @@ first_some Pred.Map.(find edb pred) Pred.Map.(find stratum pred))
    | RComp ty -> Option.value_map ~default:bottom ~f:top @@ arity_of ty
    | RProj (flds, ty) -> project ty ~flds
    | RRestr (cstr, ty) ->
      meet ~trg ty
      @@ Option.value_map (arity_of ty) ~default:bottom ~f:(fun n ->
             singleton @@ TTC.with_constraint ~cstr @@ TTC.top n)
    | RUnion (ty1, ty2) -> join ty1 ty2
    | RInter (ty1, ty2) -> meet ~trg ty1 ty2
    | RProd (ty1, ty2) -> product ty1 ty2
  in
  Relation.cata algebra rel
;;

let groupBy t ~proj ~cmp =
  let accu, cur_key, elems =
    List.fold_left
      ~f:(fun (accu, cur_key, elems) elem ->
        let key = proj elem in
        match cur_key with
        | None -> accu, Some key, [ elem ]
        | Some key' when cmp key key' = 0 -> accu, cur_key, elem :: elems
        | Some key' -> (key', elems) :: accu, Some key, [ elem ])
      ~init:([], None, [])
    @@ List.sort ~compare:(fun x y -> cmp (proj x) (proj y)) t
  in
  match cur_key with
  | Some key -> (key, elems) :: accu
  | _ -> accu
;;

let fix step ~init ~eq =
  let rec aux prev =
    let next = step prev in
    if eq prev next then next else aux next
  in
  aux init
;;

let type_of_stratum stratum ~trg ~edb =
  (* TODO: use depedencies to reduce work done *)
  let rels =
    List.map ~f:(fun (pred, cls) -> pred, Relation.of_clauses cls)
    @@ groupBy ~proj:Clause.Adorned.head_pred_of ~cmp:Pred.compare stratum
  in
  let init =
    Pred.Map.of_alist_exn
    @@ List.map rels ~f:(fun (pred, _) -> pred, top @@ Pred.arity_of pred)
  in
  let step typings =
    List.fold_left rels ~init:typings ~f:(fun stratum (pred, rel) ->
        let ty = interpret ~trg rel ~edb ~stratum in
        Pred.Map.update stratum pred ~f:Fn.(const ty))
  in
  let eq = Pred.Map.equal equal in
  fix step ~init ~eq
;;

let merge edb typings =
  List.fold_left (Pred.Map.to_alist typings) ~init:edb ~f:(fun acc (k, v) ->
      Pred.Map.update acc k ~f:Fn.(const v))
;;

let type_of strata ~trg ~edb =
  let f = type_of_stratum ~trg in
  List.fold_left strata ~init:edb ~f:(fun edb stratum ->
      merge edb @@ f stratum ~edb)
;;
