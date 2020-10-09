open Lib

type t = Subty of Ty.t

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | Subty ty -> Fmt.(prefix (any "<:@;") Ty.pp) ppf ty
  ;;

  let pp = `NoPrec pp
end)
