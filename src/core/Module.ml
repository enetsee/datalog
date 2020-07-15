open Core_kernel
open Lib
open Reporting

module Minimal = struct
  type t =
    { program : Program.Raw.t
    ; knowledge : Knowledge.Base.t
    ; tydefs : (Name.t * Ty.t) list
    ; data : (Name.t * (Name.t * Ty.t) list) list
    ; params : (Name.t * Ty.t) list
    ; exports : Name.t Located.t list
    }
  [@@deriving eq, compare]

  let empty =
    { program = Program.Raw.program []
    ; knowledge = Knowledge.Base.empty
    ; tydefs = []
    ; data = []
    ; params = []
    ; exports = []
    }
  ;;

  let pp_exports = Fmt.(list ~sep:cut @@ (any "export " ++ Located.pp Name.pp))

  let pp_params =
    Fmt.(
      list ~sep:cut
      @@ hbox
      @@ pair ~sep:(any "@;:@;") (any "param $" ++ Name.pp) Ty.pp)
  ;;

  let pp_data =
    Fmt.(
      list ~sep:cut
      @@ hovbox
      @@ pair (prefix (any "data ") Name.pp)
      @@ parens
      @@ list ~sep:comma
      @@ pair ~sep:(any "@;:@;") Name.pp Ty.pp)
  ;;

  let pp_tydefs =
    Fmt.(
      list ~sep:cut
      @@ hbox
      @@ pair ~sep:(any "@;extends@;") (prefix (any "type @") Name.pp) Ty.pp)
  ;;

  let pp ppf { program; knowledge; tydefs; data; params; exports } =
    Fmt.(
      vbox
      @@ pair ~sep:cut (suffix cut pp_tydefs)
      @@ pair ~sep:cut (suffix cut pp_params)
      @@ pair ~sep:cut (suffix cut pp_data)
      @@ pair ~sep:cut (suffix cut Knowledge.Base.pp)
      @@ pair ~sep:cut (suffix cut Program.Raw.pp) pp_exports)
      ppf
      (tydefs, (params, (data, (knowledge, (program, exports)))))
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)
