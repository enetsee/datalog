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
module type S = sig
  module Elt : Identifiable.S

  exception NotFound of Elt.t

  type directed_graph = Elt.Set.t Elt.Map.t

  type component =
    | Has_loop of Elt.t list
    | No_loop of Elt.t

  val topological : directed_graph -> component array
  val component_graph : directed_graph -> (component * int list) array
end

module type Scc = sig
  module type S = S

  module Make (Elt : Identifiable.S) : S with module Elt := Elt
end
