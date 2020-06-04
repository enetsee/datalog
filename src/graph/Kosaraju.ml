open Core_kernel

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let depth_first_order (graph : int list array) : int array =
  let size = Array.length graph in
  let marked = Array.create ~len:size false in
  let stack = Array.create ~len:size ~-1 in
  let pos = ref 0 in
  let push i =
    stack.(!pos) <- i;
    incr pos
  in
  let rec aux node =
    if not marked.(node)
    then (
      marked.(node) <- true;
      List.iter ~f:aux graph.(node);
      push node)
  in
  for i = 0 to size - 1 do
    aux i
  done;
  stack
;;

let transpose graph =
  let size = Array.length graph in
  let transposed = Array.create ~len:size [] in
  let add src dst = transposed.(src) <- dst :: transposed.(src) in
  Array.iteri
    ~f:(fun src dsts -> List.iter ~f:(fun dst -> add dst src) dsts)
    graph;
  transposed
;;

let mark order graph =
  let size = Array.length graph in
  let graph = transpose graph in
  let marked = Array.create ~len:size false in
  let id = Array.create ~len:size ~-1 in
  let count = ref 0 in
  let rec aux node =
    if not marked.(node)
    then (
      marked.(node) <- true;
      id.(node) <- !count;
      List.iter ~f:aux graph.(node))
  in
  for i = size - 1 downto 0 do
    let node = order.(i) in
    if not marked.(node)
    then (
      aux order.(i);
      incr count)
  done;
  id, !count
;;

let kosaraju graph =
  let dfo = depth_first_order graph in
  let components, ncomponents = mark dfo graph in
  ncomponents, components
;;

type component_graph =
  { sorted_connected_components : int list array
  ; component_edges : int list array
  }

let component_graph graph =
  let ncomponents, components = kosaraju graph in
  let id_scc = Array.create ~len:ncomponents [] in
  let component_graph = Array.create ~len:ncomponents Int.Set.empty in
  let add_component_dep node set =
    let node_deps = graph.(node) in
    List.fold_left
      ~f:(fun set dep -> Int.Set.add set components.(dep))
      ~init:set
      node_deps
  in
  Array.iteri
    ~f:(fun node component ->
      id_scc.(component) <- node :: id_scc.(component);
      component_graph.(component)
        <- add_component_dep node component_graph.(component))
    components;
  { sorted_connected_components = id_scc
  ; component_edges = Array.map ~f:Int.Set.elements component_graph
  }
;;
