open Core_kernel
open Lib

module Prim = struct
  module X = struct
    type t =
      | TySymbol
      | TyReal
      | TyInt
      | TyBool
    [@@deriving eq, compare]

    let pp ppf = function
      | TySymbol -> Fmt.string ppf "Symbol"
      | TyReal -> Fmt.string ppf "Real"
      | TyInt -> Fmt.string ppf "Int"
      | TyBool -> Fmt.string ppf "Bool"
    ;;

    let pp = `NoPrec pp
  end

  include X
  include Pretty.Make0 (X)
end

module Name = struct
  module X = struct
    type t = { name : string } [@@deriving eq, compare, hash, sexp]

    let pp ppf { name } = Fmt.string ppf name
    let pp = `NoPrec pp
  end

  include X
  include Pretty.Make0 (X)
  module Map = Map.Make (X)

  let from_string name = { name }
end

module X = struct
  type t =
    | Prim of Prim.t
    | Named of Name.t
  [@@deriving eq, compare]

  let pp ppf = function
    | Prim p -> Prim.pp ppf p
    | Named n -> Name.pp ppf n
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
