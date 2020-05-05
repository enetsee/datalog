open Core_kernel

(* A wrapper around StateT Result to hide complexity and provide some helpers *)

(** TODO: flatten this down and possibly add reader? *)
module M = Mtl.StateT.Make2 (Result)

include M
