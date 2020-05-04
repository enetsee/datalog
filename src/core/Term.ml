open Core_kernel
open Lib

module X = struct
  type t =
    | TVar of Tmvar.t
    | TSym of Symbol.t
    | TWild
  [@@deriving eq, compare]

  let pp ppf = function
    | TVar tv -> Tmvar.pp ppf tv
    | TSym sym -> Symbol.pp ppf sym
    | TWild -> Fmt.string ppf "_"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Constructors ---------------------------------------------------------- *)

let var' t = TVar t
let var t = var' @@ Tmvar.from_string t
let sym s = TSym s
let wild = TWild

(* -- Query ----------------------------------------------------------------- *)

let tmvars = function
  | TVar tv -> [ tv ]
  | _ -> []
;;
