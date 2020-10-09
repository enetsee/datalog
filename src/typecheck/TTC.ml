open Core_kernel

module Make (M : Ty.MonadTy) = struct
  module MTy = Ty.Make (M)

  (** Find the meet of all elements of an equivalence class  *)
  let meet_class (eq, tys) =
    M.(
      MTy.meets tys
      >>= function
      | None -> failwith "impossible"
      | Some ty ->
        return @@ List.map ~f:(fun idx -> idx, ty) @@ Int.Set.elements eq)
  ;;

  (** Group types into their equivalence classes *)
  let by_class equiv tys =
    fst
    @@ List.fold_left
         tys
         ~init:(List.map ~f:(fun eq -> eq, []) @@ Partition.elements equiv, 0)
         ~f:(fun (eqs, idx) ty ->
           let eqs =
             List.map eqs ~f:(fun (eq, tys) ->
                 if Int.Set.mem eq idx then eq, ty :: tys else eq, tys)
           in
           eqs, idx + 1)
  ;;

  let meet_within equiv tys =
    M.map ~f:(fun xss ->
        List.map ~f:snd
        @@ List.dedup_and_sort ~compare:(fun (x, _) (y, _) -> Int.compare x y)
        @@ List.concat xss)
    @@ M.all
    @@ List.map ~f:meet_class
    @@ by_class equiv tys
  ;;

  (** 
The meet of two TTCs is the pairwise meet of each type further further
constrained to be the meet of the resulting types within each equivalence
class.

(s1,...,sn | p) /\ (t1,...,tn | q) := (s1 /\ t1,...,sn /\ tn) (|) (p /\ q)
(u1,...,un) (|) r := (u1',...,un') where ui'= {uj | i ∼ j}
*)
  let meet Core.TTC.{ tys = ts1; equiv = p1 } Core.TTC.{ tys = ts2; equiv = p2 }
    =
    let equiv = Partition.(join p1 p2) in
    M.(
      List.map2_exn ~f:MTy.meet ts1 ts2
      |> all
      >>= meet_within equiv
      |> map ~f:(fun tys -> Core.TTC.{ tys; equiv }))
  ;;

  (** Given a list of types with their indicies, specialize the TTC based
  on this information and the by finding the meet of the types within
  their equivalence classes *)
  let specialize (Core.TTC.{ tys; equiv } as t) ~ty_idxs =
    let lut = Int.Map.of_alist_exn ty_idxs in
    let tys' =
      List.mapi tys ~f:(fun idx ty ->
          Option.value ~default:ty @@ Int.Map.find lut idx)
    in
    meet t { tys = tys'; equiv }
  ;;
end
