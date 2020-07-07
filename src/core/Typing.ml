open Core_kernel
open Lib

module Minimal = struct
  (** A typing is a set of non-degenerate type tuple constraints *)
  include Set.Make (TTC)

  let arity_of t = Option.map ~f:TTC.arity_of @@ List.hd @@ to_list t

  let pp ppf t =
    Fmt.(hvbox @@ braces @@ list ~sep:comma TTC.pp) ppf @@ to_list t
  ;;

  let pp = `NoPrec pp

  let of_list ttcs =
    of_list @@ List.filter ttcs ~f:(fun x -> not @@ TTC.is_degenerate x)
  ;;

  let singleton ttc = of_list [ ttc ]

  let of_schema tys =
    singleton @@ TTC.{ tys; equiv = Partition.trivial @@ List.length tys }
  ;;

  let bottom = empty
  let top n = singleton @@ TTC.top n
  let join t1 t2 = union t1 t2

  let product t1 t2 =
    let xs, ys = to_list t1, to_list t2 in
    of_list @@ List.(xs >>= fun x -> ys >>= fun y -> return @@ TTC.product x y)
  ;;

  let project t ~flds = of_list @@ List.map ~f:(TTC.project ~flds) @@ to_list t
end

include Minimal
include Pretty.Make0 (Minimal)

module type MonadTyping = sig
  include Ty.MonadTy

  val get_typing_of : Name.t -> Minimal.t t
  val set_typing_of : Name.t -> Minimal.t -> unit t
end

module Make (M : MonadTyping) = struct
  module TTCM = TTC.Make (M)

  (**  T1 /\\ T2 = { t1 /\\ t2 | t1 in T1, t2 in T2, not degenerate t1 /\\ t2 } *)
  let meet t1 t2 =
    let xs, ys = to_list t1, to_list t2 in
    M.map ~f:of_list
    @@ M.all
    @@ List.(xs >>= fun x -> ys >>= fun y -> return @@ TTCM.meet x y)
  ;;

  let restrict t ~equiv =
    meet t
    @@ Option.value_map (arity_of t) ~default:bottom ~f:(fun n ->
           singleton @@ TTC.restrict ~equiv @@ TTC.top n)
  ;;

  let specialize t ~ty_idxs =
    M.map ~f:of_list
    @@ M.all
    @@ List.map ~f:(TTCM.specialize ~ty_idxs)
    @@ to_list t
  ;;

  (** Abstract interpretation of a relation to obtain its typing *)
  let interpret rel =
    let rec aux t ~k =
      match Relation.proj t with
      | Relation.F.RPred (pr, ty_idxs) ->
        k @@ M.(get_typing_of Pred.(name_of pr) >>= specialize ~ty_idxs)
      | RProj (flds, rel) ->
        aux rel ~k:(fun tym -> k @@ M.map ~f:(project ~flds) tym)
      | RRestr (equiv, rel) ->
        aux rel ~k:(fun tym -> k @@ M.bind ~f:(restrict ~equiv) tym)
      | RUnion (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 -> k @@ M.map2 ~f:join tym1 tym2))
      | RProd (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 -> k @@ M.map2 ~f:product tym1 tym2))
      | RInter (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 ->
                k @@ M.(tym1 >>= fun ty1 -> tym2 >>= meet ty1)))
      | RComp rel -> k @@ M.return @@ top @@ Relation.arity_of rel
    in
    aux ~k:Fn.id rel
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

  let typing_of_stratum stratum =
    let deps = Dependency.Adorned.from_clauses stratum in
    let init =
      List.map ~f:(fun (pred, cls) -> pred, Relation.of_clauses cls)
      @@ groupBy ~proj:Clause.Adorned.head_pred_of ~cmp:Pred.compare stratum
    in
    let rel_lut = Pred.Map.of_alist_exn init
    and dep_lut =
      Pred.Map.of_alist_exn
      @@ List.map
           ~f:(fun (pred, _) -> pred, Dependency.Adorned.deps_of deps pred)
           init
    in
    let next pred =
      List.map ~f:Pred.Map.(fun pr -> pr, find_exn rel_lut pr)
      @@ Pred.Map.find_exn dep_lut pred
    in
    let rec aux ws =
      M.(
        match ws with
        | [] -> return ()
        | (pred, rel) :: rest ->
          let nm = Pred.name_of pred in
          get_typing_of nm
          >>= fun typing_in ->
          interpret rel
          >>= fun typing_out ->
          if equal typing_in typing_out
          then aux rest
          else
            set_typing_of nm typing_out
            >>= fun _ -> aux (rest @ ((pred, rel) :: next pred)))
    in
    aux init
  ;;

  let typing_of Program.Stratified.{ strata; _ } =
    M.map ~f:ignore @@ M.all @@ List.map ~f:typing_of_stratum strata
  ;;
end
