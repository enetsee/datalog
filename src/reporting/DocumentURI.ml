open Lib

module Minimal = struct
  type t = { uri : string }

  let pp ppf { uri } = Fmt.string ppf uri
  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)
