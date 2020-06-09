include PartialOrd_intf

module Make (X : Minimal) : S with type t := X.t = struct
  include X

  let geq t1 t2 = leq t2 t1
  let eq t1 t2 = leq t1 t2 && leq t2 t1
  let neq t1 t2 = not @@ eq t1 t2
  let lt t1 t2 = leq t1 t2 && neq t1 t2
  let gt t1 t2 = leq t2 t1 && neq t1 t2

  let compare_opt t1 t2 =
    if eq t1 t2
    then Some 0
    else if leq t1 t2
    then Some ~-1
    else if geq t1 t2
    then Some 1
    else None
  ;;
end

module MakeTuple2 (X : Minimal) (Y : Minimal) : S with type t := X.t * Y.t =
Make (struct
  type t = X.t * Y.t

  let leq (x1, y1) (x2, y2) = if X.leq x1 x2 then true else Y.leq y1 y2
end)
