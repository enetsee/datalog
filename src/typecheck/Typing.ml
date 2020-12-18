open Core_kernel

module type MonadTyping = sig
  include Ty.MonadTy
  include Relation.MonadRelation with type 'a t := 'a t

  val get_typing_of : Core.Name.t -> Core.Typing.t t
  val set_typing_of : Core.Name.t -> Core.Typing.t -> unit t
end

module Make (M : MonadTyping) = struct
  module TTCM = TTC.Make (M)
  module RelM = Relation.Make (M)

  (**  T1 /\\ T2 = { t1 /\\ t2 | t1 in T1, t2 in T2, not degenerate t1 /\\ t2 } *)
  let meet t1 t2 =
    let xs, ys = Core.Typing.(to_list t1, to_list t2) in
    M.map ~f:Core.Typing.of_list
    @@ M.all
    @@ List.(xs >>= fun x -> ys >>= fun y -> return @@ TTCM.meet x y)
  ;;

  let restrict t ~equiv =
    Core.(
      meet t
      @@ Option.value_map
           Typing.(arity_of t)
           ~default:Typing.bottom
           ~f:(fun n -> Typing.singleton @@ TTC.restrict ~equiv @@ TTC.top n))
  ;;

  let specialize t ~ty_idxs =
    M.map ~f:Core.Typing.of_list
    @@ M.all
    @@ List.map ~f:(TTCM.specialize ~ty_idxs)
    @@ Core.Typing.to_list t
  ;;

  (** Abstract interpretation of a relation to obtain its typing *)
  let interpret rel =
    let rec aux t ~k =
      match Relation.Algebra.proj t with
      | Relation.Algebra.F.RPred (pr, ty_idxs) ->
        k @@ M.(get_typing_of Core.Pred.(name_of pr) >>= specialize ~ty_idxs)
      | RProj (flds, rel) ->
        aux rel ~k:(fun tym -> k @@ M.map ~f:Core.Typing.(project ~flds) tym)
      | RRestr (equiv, rel) ->
        aux rel ~k:(fun tym -> k @@ M.bind ~f:(restrict ~equiv) tym)
      | RUnion (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 -> k @@ M.map2 ~f:Core.Typing.join tym1 tym2))
      | RProd (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 ->
                k @@ M.map2 ~f:Core.Typing.product tym1 tym2))
      | RInter (rel1, rel2) ->
        aux rel1 ~k:(fun tym1 ->
            aux rel2 ~k:(fun tym2 ->
                k @@ M.(tym1 >>= fun ty1 -> tym2 >>= meet ty1)))
      | RComp rel ->
        k @@ M.return @@ Core.Typing.top @@ Relation.Algebra.arity_of rel
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
    M.(
      let deps = Dependency.Adorned.from_clauses stratum in
      groupBy
        ~proj:Core.Clause.Adorned.head_pred_of
        ~cmp:Core.Pred.compare
        stratum
      |> List.map ~f:(fun (pred, cls) ->
             map ~f:(fun rel -> pred, rel) @@ RelM.of_clauses cls)
      |> all
      >>= fun init ->
      let rel_lut = Core.Pred.Map.of_alist_exn init
      and dep_lut =
        Core.Pred.Map.of_alist_exn
        @@ List.map
             ~f:(fun (pred, _) -> pred, Dependency.Adorned.deps_of deps pred)
             init
      in
      let next pred =
        List.map ~f:Core.Pred.Map.(fun pr -> pr, find_exn rel_lut pr)
        @@ Core.Pred.Map.find_exn dep_lut pred
      in
      let rec aux ws =
        M.(
          match ws with
          | [] -> return ()
          | (pred, rel) :: rest ->
            let nm = Core.Pred.name_of pred in
            get_typing_of nm
            >>= fun typing_in ->
            interpret rel
            >>= fun typing_out ->
            if Core.Typing.equal typing_in typing_out
            then aux rest
            else
              set_typing_of nm typing_out
              >>= fun _ -> aux (rest @ ((pred, rel) :: next pred)))
      in
      aux init)
  ;;

  let typing_of_knowledge_base kb =
    M.(
      RelM.of_knowledge_base kb
      >>= fun rels ->
      List.map
        ~f:(fun (pr, rel) -> map ~f:(fun ty -> pr, ty) @@ interpret rel)
        rels
      |> all
      >>= fun _ -> return ())
  ;;

  let typing_of_program Core.Program.Stratified.{ strata; _ } =
    M.map ~f:ignore @@ M.all @@ List.map ~f:typing_of_stratum strata
  ;;
end
