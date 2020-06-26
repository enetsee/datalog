open Core_kernel
open Lib
open Reporting

(** A `Term` is either:
    - A term `Symbol` or constant;
    - A term variable `Tmvar`; or
    - A term wildcard, corresponding to an existential variable. 
  *)
type t =
  | TVar of Tmvar.t * (Region.t[@compare.ignore] [@equal.ignore])
  | TSym of Symbol.t * (Region.t[@compare.ignore] [@equal.ignore])
  | TWild of (Region.t[@compare.ignore] [@equal.ignore])
[@@deriving compare, sexp, eq]

exception NotAVariable of t
exception NotASymbol of t

(* -- Constructors ---------------------------------------------------------- *)

let var' ?(region = Region.empty) t = TVar (t, region)
let var ?(region = Region.empty) t = var' ~region @@ Tmvar.from_string t
let sym ?(region = Region.empty) s = TSym (s, region)
let text ?region str = sym ?region @@ Symbol.from_string str
let real ?region f = sym ?region @@ Symbol.from_float f
let int ?region n = sym ?region @@ Symbol.from_int n
let bool ?region b = sym ?region @@ Symbol.from_bool b
let wild ?(region = Region.empty) () = TWild region

(* -- Query ----------------------------------------------------------------- *)

let is_var = function
  | TVar _ -> true
  | _ -> false
;;

let is_sym = function
  | TSym _ -> true
  | _ -> false
;;

let is_wild = function
  | TWild _ -> true
  | _ -> false
;;

let lower_var = function
  | TVar (v, _) -> Some v
  | _ -> None
;;

let lower_sym = function
  | TSym (s, _) -> Some s
  | _ -> None
;;

let lower_var_exn = function
  | TVar (v, _) -> v
  | t -> raise (NotAVariable t)
;;

let lower_sym_exn = function
  | TSym (s, _) -> s
  | t -> raise (NotASymbol t)
;;

(* -- HasVars implementation ------------------------------------------------ *)

let vars_of = function
  | TVar (t, _) -> [ t ]
  | _ -> []
;;

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | TVar (tv, _) -> Tmvar.pp ppf tv
    | TSym (sym, _) -> Symbol.pp ppf sym
    | TWild _ -> Fmt.string ppf "_"
  ;;

  let pp = `NoPrec pp
end)
