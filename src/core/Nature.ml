type t =
  | Logical
  | ExtraLogical of ForeignFunc.t
[@@deriving compare]
