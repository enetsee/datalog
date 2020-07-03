open Core_kernel
open Lib

(** A `Predicate` is identified by its name and its arity i.e. the number of 
    `Term`s it takes as arguments. A `Predicate` is said to be _intentional_ if 
    it appears in the conclusion of some clause. If a `Predicate` appears only 
    as a premise it is said to be _extensional_ and may be defined as 
    `Knowledge`.
*)
module X = struct
  type t =
    { name : Name.t
    ; arity : int
    }
  [@@deriving eq, compare, hash, sexp]

  let arity_of { arity; _ } = arity
  let name_of { name; _ } = name
  let pp ppf { name; _ } = Name.pp ppf name
  let pp = `NoPrec pp
  let pred name ~arity = { name; arity }
end

include X
include Pretty.Make0 (X)
module Set = Set.Make (X)
module Map = Map.Make (X)
