open Core_kernel
open Lib
open Reporting

module X = struct
  (** A `Term` is either:
    - A term `Symbol` or constant;
    - A term variable `Tmvar`; or
    - A term wildcard, corresponding to an existential variable. 
  *)
  type t =
    | TVar of Tmvar.t * (Region.t[@compare.ignore])
    | TSym of Symbol.t * (Region.t[@compare.ignore])
    | TWild of (Region.t[@compare.ignore])
  [@@deriving compare, sexp]

  let pp ppf = function
    | TVar (tv, _) -> Tmvar.pp ppf tv
    | TSym (sym, _) -> Symbol.pp ppf sym
    | TWild _ -> Fmt.string ppf "_"
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Constructors ---------------------------------------------------------- *)

let var' ?(region = Region.empty) t = TVar (t, region)
let var ?(region = Region.empty) t = var' ~region @@ Tmvar.from_string t
let sym ?(region = Region.empty) s = TSym (s, region)
let wild ?(region = Region.empty) () = TWild region

(* -- Query ----------------------------------------------------------------- *)

let is_var = function
  | TVar _ -> true
  | _ -> false
;;

let lower_var = function
  | TVar (v, _) -> Some v
  | _ -> None
;;

let lower_var_exn = function
  | TVar (v, _) -> v
  | _ -> failwith "Term.lower_var_exn: not a variable"
;;

let vars_of = function
  | TVar (t, _) -> [ t ]
  | _ -> []
;;
