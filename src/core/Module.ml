open Core_kernel
open Lib

module Minimal = struct
  type t =
    { clauses : Clause.Raw.t list
    ; knowledge : Knowledge.t list
    ; tydefns : (Name.t * Ty.t) list
    ; data : (Name.t * (Name.t * Ty.t) list) list
    ; params : (Name.t * Ty.t) list
    ; exports : Name.t list
    }
  [@@deriving eq, compare]

  let empty =
    { clauses = []
    ; knowledge = []
    ; tydefns = []
    ; data = []
    ; params = []
    ; exports = []
    }
  ;;

  let pp_clauses = Fmt.(list ~sep:cut Clause.Raw.pp)
  let pp_knowledge = Fmt.(list ~sep:cut Knowledge.pp)
  let pp_exports = Fmt.(list ~sep:cut @@ (any "export " ++ Name.pp))

  let pp_params =
    Fmt.(
      list ~sep:cut
      @@ hbox
      @@ pair ~sep:(any "@;:@;") (any "param $" ++ Name.pp) Ty.pp)
  ;;

  let pp_data =
    Fmt.(
      list ~sep:cut
      @@ pair (prefix (any "data ") Name.pp)
      @@ parens
      @@ list ~sep:comma
      @@ pair ~sep:(any "@;:@;") Name.pp Ty.pp)
  ;;

  let pp_tydefns =
    Fmt.(
      list ~sep:cut
      @@ pair ~sep:(any "@;extends@;") (prefix (any "type @") Name.pp) Ty.pp)
  ;;

  let pp ppf { clauses; knowledge; tydefns; data; params; exports } =
    Fmt.(
      vbox
      @@ pair ~sep:cut (suffix cut pp_tydefns)
      @@ pair ~sep:cut (suffix cut pp_params)
      @@ pair ~sep:cut (suffix cut pp_data)
      @@ pair ~sep:cut (suffix cut pp_knowledge)
      @@ pair ~sep:cut (suffix cut pp_clauses) pp_exports)
      ppf
      (tydefns, (params, (data, (knowledge, (clauses, exports)))))
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)
