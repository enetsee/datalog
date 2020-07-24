open Core_kernel
open Lib

(** A type tuple constraint (TTC) tracks both the type of fields of a relation 
    as well as equalities between fields.

    A TTC of arity n consists of an n-tuple of types (t1,...,tn), plus a 
    partition p of {1,...,n}. The partition plays the role of an equality type 
    in formal logic, stipulating which components of (t1,...,tn) must be equal.
*)
type t =
  { tys : Ty.t list
  ; equiv : Partition.t
  }
[@@deriving eq, compare, sexp]

let ttc ?equiv tys =
  let equiv =
    Option.value ~default:Partition.(trivial @@ List.length tys) equiv
  in
  { tys; equiv }
;;

let arity_of { tys; _ } = List.length tys

let restrict t ~equiv:(i, j) =
  { t with equiv = Partition.update_exn (i, j) t.equiv }
;;

(** For given arity `n`, ( _|_, ... , _|_ | {{1..n}} ) *)
let bottom n =
  { tys = List.init n ~f:Fn.(const Ty.bottom)
  ; equiv = Partition.singleton @@ Int.Set.of_list @@ List.init n ~f:Fn.id
  }
;;

(** For a given arity `n`, ( T,...,T | {{1},...,{n}} ) *)
let top n =
  { tys = List.init n ~f:Fn.(const Ty.top)
  ; equiv = Partition.of_list @@ List.init n ~f:Int.Set.singleton
  }
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { tys; equiv } =
    match
      List.filter ~f:(fun elem -> Int.Set.length elem > 1)
      @@ Partition.to_list equiv
    with
    | [] -> Fmt.(parens @@ hovbox @@ list ~sep:comma Ty.pp) ppf tys
    | elems ->
      Fmt.(
        parens
        @@ hovbox
        @@ pair ~sep:(always " @;|@; ") (list ~sep:comma Ty.pp)
        @@ braces
        @@ list ~sep:comma
        @@ braces
        @@ list ~sep:comma int)
        ppf
        (tys, List.map ~f:Int.Set.(to_list) elems)
  ;;

  let pp = `NoPrec pp
end)

(** Any TTC that contains the type _|_ represents an empty relation, since the 
    interpretation of _|_ is always empty. We call such TTCs degenerate. 
*)
let is_degenerate { tys; _ } = List.exists ~f:Ty.(equal Ty.bottom) tys

(** Cartesian product of two TTCs simply appends tuples and takes the union
    of equivalence classes after shifting the second partition to the correct
    poisitons in the cross product.

    (t_1,...,t_k | p) (x) (t_k+1,...,t_n | q) = (t_1, ... , t_n | p U q)
  *)
let product { tys = ts1; equiv = p1 } { tys = ts2; equiv = p2 } =
  let arity1 = List.length ts1 in
  { tys = ts1 @ ts2
  ; equiv =
      Partition.(union p1 @@ map ~f:(Int.Set.map ~f:(fun x -> x + arity1)) p2)
  }
;;

(** Projection: given an ordered list of tuple indices, return the TTC with 
    a tuple of corresponding types and update the equivalence classes if 
    types change index
*)
let project { tys; equiv } ~flds =
  let lut =
    Int.Map.of_alist_exn
    @@ List.mapi flds ~f:(fun new_idx old_idx -> old_idx, new_idx)
  and tylut =
    Int.Map.of_alist_exn @@ List.mapi tys ~f:(fun idx ty -> idx, ty)
  in
  let tys = List.map flds ~f:Int.Map.(find_exn tylut)
  and equiv =
    Partition.of_list
    @@ List.filter_map ~f:(fun elts ->
           match
             List.filter_map ~f:Int.Map.(find lut) @@ Int.Set.to_list elts
           with
           | [] -> None
           | elts -> Some (Int.Set.of_list elts))
    @@ Partition.to_list equiv
  in
  { tys; equiv }
;;

module Make (M : Ty.TyM) = struct
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
  let meet { tys = ts1; equiv = p1 } { tys = ts2; equiv = p2 } =
    let equiv = Partition.(join p1 p2) in
    M.(
      List.map2_exn ~f:MTy.meet ts1 ts2
      |> all
      >>= meet_within equiv
      |> map ~f:(fun tys -> { tys; equiv }))
  ;;

  (** Given a list of types with their indicies, specialize the TTC based
    on this information and the by finding the meet of the types within
    their equivalence classes *)
  let specialize ({ tys; equiv } as t) ~ty_idxs =
    let lut = Int.Map.of_alist_exn ty_idxs in
    let tys' =
      List.mapi tys ~f:(fun idx ty ->
          Option.value ~default:ty @@ Int.Map.find lut idx)
    in
    meet t { tys = tys'; equiv }
  ;;
end
