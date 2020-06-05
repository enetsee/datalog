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
    { name : PredSymbol.t
    ; arity : int
    ; nature : Nature.t
    }
  [@@deriving eq, compare, sexp]

  let pp ppf { name; _ } = PredSymbol.pp ppf name
  let pp = `NoPrec pp
  let pred ?(nature = Nature.Logical) name arity = { name; arity; nature }
  let logical name arity = { name; arity; nature = Logical }

  let extralogical ?(eff = Eff.Set.empty) name arity =
    { name; arity; nature = Extralogical eff }
  ;;

  let effects_of { nature; _ } = Nature.effects_of nature
end

include X
include Pretty.Make0 (X)
module Set = Set.Make (X)
module Map = Map.Make (X)
