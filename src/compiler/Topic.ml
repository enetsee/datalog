open Lib

module X = struct
  type t = Warn.t list

  let mempty = []
  let append x y = x @ y
  let pp ppf xs = Fmt.(vbox @@ list ~sep:cut Warn.pp) ppf xs
  let pp = `NoPrec pp
end

include X
include Monoid.Make0 (X)
include Pretty.Make0 (X)
