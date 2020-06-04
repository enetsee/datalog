open Core_kernel
include Scc_intf

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
module Make (Elt : Identifiable.S) : S with module Elt := Elt = struct
  exception NotFound of Elt.t

  type directed_graph = Elt.Set.t Elt.Map.t

  type component =
    | Has_loop of Elt.t list
    | No_loop of Elt.t

  type numbering =
    { back : int Elt.Map.t
    ; forth : Elt.t array
    }

  let number graph =
    let size = Elt.Map.length graph in
    let bindings = Elt.Map.to_alist graph in
    let a = Array.of_list bindings in
    let forth = Array.map ~f:fst a in
    let back =
      let back = ref Elt.Map.empty in
      for i = 0 to size - 1 do
        back := Elt.Map.update !back forth.(i) ~f:Fn.(const i)
      done;
      !back
    in
    let integer_graph =
      Array.init size ~f:(fun i ->
          let _, dests = a.(i) in
          Elt.Set.fold
            ~f:(fun acc dest ->
              let v =
                try Elt.Map.find_exn back dest with
                | _ -> raise @@ NotFound dest
              in
              v :: acc)
            dests
            ~init:[])
    in
    { back; forth }, integer_graph
  ;;

  let component_graph graph =
    let numbering, integer_graph = number graph in
    let Kosaraju.{ sorted_connected_components; component_edges } =
      Kosaraju.component_graph integer_graph
    in
    Array.mapi
      ~f:(fun component nodes ->
        match nodes with
        | [] -> assert false
        | [ node ] ->
          ( (if List.exists ~f:Int.(equal node) integer_graph.(node)
            then Has_loop [ numbering.forth.(node) ]
            else No_loop numbering.forth.(node))
          , component_edges.(component) )
        | _ :: _ ->
          ( Has_loop (List.map ~f:(fun node -> numbering.forth.(node)) nodes)
          , component_edges.(component) ))
      sorted_connected_components
  ;;

  let topological graph = Array.map ~f:fst @@ component_graph graph
end
