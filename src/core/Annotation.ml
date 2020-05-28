open Core_kernel
open Reporting

(* TODO: can we use constraints to statically enforce stages here?  *)

type 'a t =
  | ABase of (Region.t[@compare.ignore])
  | ADelta of 'a t
  | ADependency of 'a t
  | AAdornment of 'a t
  | ARename of int * 'a t
  | AProvenance of 'a t
[@@deriving compare]

let base region = ABase region
