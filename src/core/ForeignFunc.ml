open Core_kernel

type t =
  { arity : int
  ; ff : Term.t list -> (Symbol.t list, string) result [@compare.ignore]
  }
[@@deriving compare]
