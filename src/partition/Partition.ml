open Core_kernel
open Lib

exception NotInPartition of (int * int)

module Minimal = struct
  include Set.Make (Int.Set)

  let pp ppf xs =
    Fmt.(hvbox @@ braces @@ list ~sep:comma @@ braces @@ list ~sep:comma int)
      ppf
    @@ List.map ~f:Int.Set.elements
    @@ elements xs
  ;;

  let pp = `NoPrec pp
  let trivial n = of_list @@ List.init n ~f:Int.Set.singleton

  (** Update a partition such that the sets containing i and j are combined *)
  let update (i, j) t =
    let rec aux has_i has_j accu = function
      | [] ->
        Option.map2 has_i has_j ~f:(fun x y ->
            of_list @@ (Int.Set.union x y :: accu))
      | next :: rest when Int.Set.mem next i -> aux (Some next) has_j accu rest
      | next :: rest when Int.Set.mem next j -> aux has_i (Some next) accu rest
      | next :: rest -> aux has_i has_j (next :: accu) rest
    in
    aux None None [] @@ elements t
  ;;

  let update_exn eq t =
    match update eq t with
    | Some t -> t
    | _ -> raise @@ NotInPartition eq
  ;;

  (** A partition `p`  of a set X is finer than `q` if every element of `p` 
        is a subset of some element of `q`. 
    *)
  let is_finer p ~than:q =
    for_all p ~f:(fun e -> exists q ~f:(fun of_ -> Int.Set.is_subset e ~of_))
  ;;

  (** The set of all partitions is partially ordered by refinement *)
  let leq p q = is_finer p ~than:q

  (** 
    The greatest lower bound of P and Q is a partition which 
    is finer than P and Q. This is the set of non-empty 
    intersections of the elements of the product of P and Q.
    
    P /\ Q = { p `inter` q | p in P, q in Q, p `inter` q /= 0 }
  *)
  let meet p q =
    of_list
    @@ List.(
         elements p
         >>= fun pe ->
         elements q
         >>= fun qe ->
         let i = Int.Set.inter pe qe in
         if Int.Set.is_empty i then [] else [ i ])
  ;;

  let min_of in_ =
    let xs, orig =
      match in_ with
      | `ListIn xs -> xs, of_list xs
      | `SetIn orig -> elements orig, orig
    in
    of_list
    @@ List.filter xs ~f:(fun x ->
           not
           @@ exists orig ~f:(fun y ->
                  Int.Set.((not @@ equal x y) && is_subset x ~of_:y)))
  ;;

  (** The least upper bound of P and Q is a partition of which both P and Q are
      refinements.

      This can be found by repeatedly taking the union of the the Cartesian 
      product of P and Q such that the intersection is not empty whilst
      alternating between the two partitions.

      Example: 

      P = {{1},{2},{3,4}}
      Q = {{1,3},{2},{4}}
      P \/ Q  = {{1,3,4},{2}}

            1           1
                         \
      P = 2   3   Q = 2   3
             /            
            4           4

                 1
      P \/ Q =   |\
               2 | 3
                 |/
                 4
  *)
  let join t1 t2 =
    let step accu qs =
      min_of
      @@ `ListIn
           (List.concat_map ~f:(fun p ->
                List.filter_map
                  ~f:(fun q ->
                    if Int.Set.(is_empty @@ inter p q)
                    then None
                    else Some Int.Set.(union p q))
                  qs)
           @@ elements accu)
    in
    let rec aux accu = function
      | [], _ -> accu
      | ps, qs ->
        let accu' = step accu qs in
        if equal accu accu' then accu else aux accu' (qs, ps)
    in
    let ps, qs = elements t1, elements t2 in
    aux (step t1 qs) (ps, qs)
  ;;

  (** Given an partition over a tuple n, extract equivalences classes for the 
      projected indices.

      Example: given {{0,1},{2,3}}, projecting on {2,3} should give {0,1}


  *)
  let project t ~flds =
    let lut =
      Int.Map.of_alist_exn
      @@ List.mapi ~f:(fun new_idx old_idx -> old_idx, new_idx)
      @@ Int.Set.elements flds
    in
    of_list
    @@ List.filter_map ~f:(fun elt ->
           let elt' =
             Int.Set.of_list
             @@ List.filter_map ~f:Int.Map.(find lut)
             @@ Int.Set.elements elt
           in
           if Int.Set.is_empty elt' then None else Some elt')
    @@ elements t
  ;;
end

include Minimal
include PartialOrd.Make (Minimal)
include Pretty.Make0 (Minimal)
