open Core_kernel
open Lib

module X = struct
  type 'a t =
    { head : 'a Literal.t
    ; body : 'a Literal.t NonEmpty.t
    ; annot : 'a Annotation.t
    }

  let pp pp_a ppf { head; body; _ } =
    Fmt.(
      hovbox
      @@ pair ~sep:(any " :-@,") (Literal.pp pp_a)
      @@ hovbox
      @@ list ~sep:comma (Literal.pp pp_a))
      ppf
      (head, NonEmpty.to_list body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make1 (X)
