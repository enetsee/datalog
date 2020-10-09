open Core_kernel

type t = Ty.Set.t Ty.Map.t

let empty = Ty.Map.empty
let subtypes_of trg ~ty = Ty.Map.find trg ty

(** Given a map of (subty,ty) pairs, find its transitive closure as a map
      from a type to all of its subtypes *)
let from_list xs =
  let step init =
    List.fold_left ~init ~f:(fun accu ty ->
        let subs =
          Ty.Set.union_list
          @@ List.map ~f:(fun ty' ->
                 if Ty.equal ty ty'
                 then Ty.Set.(singleton ty')
                 else
                   Option.value ~default:Ty.Set.(singleton ty')
                   @@ Ty.Map.find accu ty')
          @@ Ty.Set.to_list
          @@ Ty.Map.find_exn accu ty
        in
        Ty.Map.update accu ty ~f:Fn.(const subs))
    @@ Ty.Map.keys init
  in
  let rec fix prev =
    let next = step prev in
    if Ty.Map.equal Ty.Set.equal next prev then next else fix next
  in
  fix
  @@ Ty.Map.of_alist_exn
  @@ List.map ~f:(function
         | (ty, _) :: _ as xs -> ty, Ty.Set.of_list @@ (ty :: List.map ~f:snd xs)
         | _ -> failwith "impossible")
  @@ List.group ~break:(fun (ty1, _) (ty2, _) -> not @@ Ty.equal ty1 ty2)
  @@ List.dedup_and_sort
       ~compare:(Tuple2.compare ~cmp1:Ty.compare ~cmp2:Ty.compare)
  @@ List.concat_map
       ~f:(fun (nm, ty) ->
         let subty = Ty.Named nm in
         [ subty, subty; ty, subty ])
       xs
;;
