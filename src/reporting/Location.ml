open Lib

module Minimal = struct
  type t =
    { doc : DocumentURI.t
    ; region : Region.t
    }

  let pp ppf { doc; region } =
    Fmt.(hovbox @@ pair ~sep:sp DocumentURI.pp Region.pp) ppf (doc, region)
  ;;

  let pp = `NoPrec pp
end

include Minimal
include Pretty.Make0 (Minimal)
