open Lib

module type S = sig
  include Pretty.S0

  val precedence : t -> int
end

module Never : S = struct
  type t = Never of t [@@warning "-37"]

  let precedence _t = failwith "impossible"

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp_ _ _ = failwith "impossible"
    let pp = `NoPrec pp_
  end)
end

module Unary : sig
  type t = Neg

  include S with type t := t
end = struct
  type t = Neg

  let pp ppf _ = Fmt.(string ++ sp) ppf "not"
  let precedence _ = 3

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module Binary : sig
  type t =
    | Conj
    | Disj

  include S with type t := t
end = struct
  type t =
    | Conj
    | Disj

  let pp ppf = function
    | Conj -> Fmt.char ppf ','
    | Disj -> Fmt.char ppf ';'
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)

  let precedence = function
    | Conj -> 2
    | Disj -> 1
  ;;
end
