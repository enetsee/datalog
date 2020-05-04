include Pretty_intf

module Make0 (X : Minimal0) : S0 with type t := X.t = struct
  let pp =
    match X.pp with
    | `NoPrec f -> f
    | `WithPrec f -> f 0
  ;;

  let pp_prec =
    match X.pp with
    | `NoPrec f -> fun _ -> f
    | `WithPrec f -> f
  ;;

  let to_string t = Fmt.to_to_string pp t
end

module Make1 (X : Minimal1) : S1 with type 'a t := 'a X.t = struct
  let pp pp_a ppf t =
    match X.pp with
    | `NoPrec f -> f pp_a ppf t
    | `WithPrec f -> f 0 (fun _ -> pp_a) ppf t
  ;;

  let pp_prec prec pp_a ppf t =
    match X.pp with
    | `NoPrec f -> f (pp_a 0) ppf t
    | `WithPrec f -> f prec pp_a ppf t
  ;;

  let to_string t ~pp_a = Fmt.to_to_string (pp pp_a) t
end

module Make2 (X : Minimal2) : S2 with type ('a, 'b) t := ('a, 'b) X.t = struct
  let pp pp_a pp_b ppf t =
    match X.pp with
    | `NoPrec f -> f pp_a pp_b ppf t
    | `WithPrec f -> f 0 (fun _ -> pp_a) (fun _ -> pp_b) ppf t
  ;;

  let pp_prec prec pp_a pp_b ppf t =
    match X.pp with
    | `NoPrec f -> f (pp_a 0) (pp_b 0) ppf t
    | `WithPrec f -> f prec pp_a pp_b ppf t
  ;;

  let to_string t ~pp_a ~pp_b = Fmt.to_to_string (pp pp_a pp_b) t
end
