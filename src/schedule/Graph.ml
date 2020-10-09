open Core_kernel
open Core
open Lib

module Minimal = struct
  (** A schedule graph encodes the ordering of body literals *)
  type t =
    { head_vars : (int * Var.t) list
    ; graph : (Vertex.t * Edge.t list) Tree.t
    }
  [@@deriving compare, eq]

  (* -- Pretty implementation ------------------------------------------------- *)

  let pp_lbl ppf (vtx, edges) =
    Fmt.(hbox @@ pair ~sep:sp Vertex.pp (braces @@ list ~sep:comma Edge.pp))
      ppf
      (vtx, edges)
  ;;

  let pp_head_vars ppf vs =
    Fmt.(hbox @@ braces @@ list ~sep:comma @@ pair ~sep:(any "@") int Var.pp)
      ppf
      vs
  ;;

  let pp ppf { head_vars; graph } =
    Fmt.(vbox @@ pair ~sep:cut pp_head_vars (Tree.pp pp_lbl))
      ppf
      (head_vars, graph)
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)

(* == Constraint extraction ================================================= *)

(** Convert a debt to a atomic constraint *)
let atomic_constraint_of_debt debt ~head_vars =
  Constraint.Atomic.of_list
  @@ List.filter_map
       ~f:(fun (idx, v) -> if Var.Set.mem debt v then Some idx else None)
       head_vars
;;

(** Convert a debt to a singleton constraint set *)
let constraint_of_debt debt ~head_vars =
  Constraint.(singleton @@ atomic_constraint_of_debt ~head_vars debt)
;;

(** Extract the possible mode patterns for the variables in the head of the 
      clause.
      
      Note that we only cosider the debt at _terminal_ vertices i.e. those for 
      which all literals have been scheduled 
  *)
let extract { head_vars; graph } =
  Constraint.join_list
  @@ List.filter_map ~f:(fun (Vertex.{ available; debt; _ }, _) ->
         if List.is_empty available
         then Some (constraint_of_debt ~head_vars debt)
         else None)
  @@ Tree.leaves graph
;;

(* == Ordering extraction =================================================== *)

(** Enumerate all permutations of literals in a path segment *)
let permutations t =
  let neq x y = not @@ Lit.Raw.equal x y in
  let rec aux = function
    | [] -> [ [] ]
    | xs ->
      List.concat_map xs ~f:(fun x ->
          List.map ~f:(fun xs -> x :: xs) @@ aux @@ List.filter ~f:(neq x) xs)
  in
  aux t
;;

(** Expand a list of path segments into all of path permutation *)
let expand pss =
  List.fold_left ~init:[ [] ] pss ~f:(fun paths litset ->
      let segments = permutations @@ Set.elements litset in
      List.(paths >>= fun path -> segments >>= fun seg -> return (path @ seg)))
;;

(** Helper to convert a binding pattern to a `Var.Set.t` *)
let bound_by { head_vars; _ } ~bpatt =
  Var.Set.of_list
  @@ List.filter_mapi ~f:(fun idx ->
       function
       | Binding.Bound ->
         Some (snd @@ List.find_exn head_vars ~f:(fun (i, _) -> i = idx))
       | _ -> None)
  @@ Binding.to_list bpatt
;;

(** Flatten the tree representation of the schedule graph collecting 
      on terminal paths which are consistent with the binding pattern
  *)
let consistent_paths ({ graph; _ } as t) ~bpatt =
  (* Check consistency by subset rather than via `Binding.consistent`
     to avoid repeatedly converting `debt` to atomic constraints *)
  let bound = bound_by t ~bpatt in
  let rec flatten
      ( Edge.{ scheduled; _ }
      , Tree.{ lbl = Vertex.{ available; debt; _ }, edges_out; children } )
    =
    match children with
    | [] when List.is_empty available && Var.Set.is_subset debt ~of_:bound ->
      [ [ scheduled ] ]
    | [] -> []
    | ts ->
      List.zip_exn edges_out ts
      |> List.concat_map ~f:flatten
      |> List.map ~f:(fun xs -> scheduled :: xs)
  in
  flatten (Edge.empty, graph)
;;

(** Obtain the unique orderings of body literals encoded in the minimum obligation
    graph that are consistent with the binding pattern *)
let orderings t ~bpatt =
  List.(
    dedup_and_sort ~compare:(compare Lit.Raw.compare)
    @@ concat_map ~f:expand
    @@ consistent_paths t ~bpatt)
;;
