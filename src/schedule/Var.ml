open Core_kernel
open Core
open Lib

module Minimal = struct
  (** A `Var` is either a named variable or a wildcard; we use this 
        representation to deal with situations where a constraint may indexes
        a wildcard variable. When constructing a schedule graph, any test
        to determine if the cost of scheduling a literal are `affordable` will
        fail since only named variables are present in the head or become
        bound during construction.
    *)
  type t =
    | Named of Tmvar.t
    | Wild of int
  [@@deriving eq, compare, sexp]

  let pp ppf = function
    | Named v -> Tmvar.pp ppf v
    | Wild _ -> Fmt.char ppf '_'
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)

module Set = struct
  include Set.Make (Minimal)

  include PartialOrd.Make (struct
    type nonrec t = t

    let leq x y = is_subset x ~of_:y
  end)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf xs =
      Fmt.(hovbox @@ braces @@ list ~sep:comma pp) ppf @@ elements xs
    ;;

    let pp = `NoPrec pp
  end)
end
