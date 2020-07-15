open Core_kernel
open Lib
open Reporting

(** A `Term` is either:
    - A term variable `Tmvar`; or
    - A term wildcard, corresponding to an existential variable.
    - A term `Symbol` or constant;
    - A term parameter which is a constant that must be provided to execute the 
      program
  *)
type t =
  | TVar of Tmvar.t * (Region.t[@compare.ignore] [@equal.ignore])
  | TWild of Name.t option * (Region.t[@compare.ignore] [@equal.ignore])
  | TSym of Symbol.t * (Region.t[@compare.ignore] [@equal.ignore])  
  | TParam of Name.t * (Region.t[@compare.ignore] [@equal.ignore])
[@@deriving compare, sexp, eq, variants]

exception NotAVariable of t
exception NotASymbol of t
exception NotAParam of t

(* -- Constructors ---------------------------------------------------------- *)

let var' ?(region = Region.empty) t = TVar (t, region)
let var ?(region = Region.empty) t = var' ~region @@ Tmvar.from_string t
let sym ?(region = Region.empty) s = TSym (s, region)
let text ?region str = sym ?region @@ Symbol.text str
let real ?region f = sym ?region @@ Symbol.real f
let int ?region n = sym ?region @@ Symbol.int n
let bool ?region b = sym ?region @@ Symbol.bool b

let date ?region d = sym ?region @@ Symbol.date d

let span ?region unit_of_time = sym ?region @@ Symbol.span unit_of_time


let wild  ?(region = Region.empty) nm_opt = 
    TWild(nm_opt,region)

let param ?(region = Region.empty) name  = 
  TParam(name,region)
(* -- Query ----------------------------------------------------------------- *)

let is_var = function
  | TVar _ -> true
  | _ -> false
;;

let is_wild = function
  | TWild _ -> true
  | _ -> false
;;

let is_sym = function
  | TSym _ -> true
  | _ -> false
;;

let is_param = function
  | TParam _ -> true
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

let lower_param = function
  | TParam (nm, _) -> Some nm
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

let lower_param_exn = function
  | TParam (nm, _) -> nm
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
    | TWild(Some nm,_) -> Fmt.( (any "_") ++ Name.pp) ppf nm
    | TWild _ -> Fmt.string ppf "_"
    | TParam(nm,_) -> Fmt.( (any "#") ++ Name.pp) ppf nm
  ;;

  let pp = `NoPrec pp
end)
