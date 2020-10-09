open Core_kernel
open Core
open Lib
open Reporting

exception ClauseHasNoBody of Region.t
exception NoClauses

module F = struct
  type 'a t =
    | RPred of Pred.t * (int * Ty.t) list
    | RUnion of 'a * 'a
    | RInter of 'a * 'a
    | RProd of 'a * 'a
    | RComp of 'a
    | RProj of int list * 'a
    | RRestr of (int * int) * 'a
  [@@deriving map, compare, eq]

  let pred ?(ty_idxs = []) pred = RPred (pred, ty_idxs)
  let union a b = RUnion (a, b)
  let inter a b = RInter (a, b)
  let product a b = RProd (a, b)
  let comp a = RComp a
  let project t ~flds = RProj (flds, t)
  let restrict t ~equiv = RRestr (equiv, t)

  include Functor.Make1 (struct
    type nonrec 'a t = 'a t

    let map t ~f = map f t
  end)

  include Pretty.Make1 (struct
    type nonrec 'a t = 'a t

    let pp_binop prec pp_a prec' sep =
      let g = Fmt.(hovbox @@ pair ~sep (pp_a prec') (pp_a prec')) in
      if prec' < prec then Fmt.parens g else g
    ;;

    let pp_projset = Fmt.(braces @@ list ~sep:comma int)
    let pp_eqs = Fmt.(hbox @@ parens @@ pair ~sep:(any "@;~@;") int int)

    let pp_pred ppf pr = function
      | [] -> Pred.pp ppf pr
      | ty_idxs ->
        Fmt.(
          hbox
          @@ pair Pred.pp
          @@ parens
          @@ list ~sep:comma
          @@ pair ~sep:(any "@;:@;") int Ty.pp)
          ppf
          (pr, ty_idxs)
    ;;

    let pp prec pp_a ppf = function
      | RPred (pr, ty_idxs) -> pp_pred ppf pr ty_idxs
      | RUnion (a, b) -> (pp_binop prec pp_a 1 Fmt.(any "@;\\/@;")) ppf (a, b)
      | RInter (a, b) -> (pp_binop prec pp_a 2 Fmt.(any "@;/\\@;")) ppf (a, b)
      | RProd (a, b) -> (pp_binop prec pp_a 3 Fmt.(any "@;*@;")) ppf (a, b)
      | RComp a ->
        let prec' = 4 in
        let fmt =
          let g = Fmt.(hbox @@ prefix (any "not@;") @@ pp_a prec') in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf a
      | RProj (s, a) ->
        let prec' = 4 in
        let fmt =
          let g =
            Fmt.(hbox @@ prefix (any "pi") @@ pair pp_projset @@ pp_a prec')
          in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (s, a)
      | RRestr (eq, a) ->
        let prec' = 4 in
        let fmt =
          let g =
            Fmt.(hbox @@ prefix (any "sig") @@ pair pp_eqs @@ pp_a prec')
          in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (eq, a)
    ;;

    let pp = `WithPrec pp
  end)

  module Traversable = struct
    module Make (M : sig
      include Monad.S
      include Applicative.S with type 'a t := 'a t
    end) =
    struct
      let traverse t ~f =
        match t with
        | RPred (pr, ty_idxs) -> M.return @@ RPred (pr, ty_idxs)
        | RUnion (a, b) -> M.map2 ~f:union (f a) (f b)
        | RInter (a, b) -> M.map2 ~f:inter (f a) (f b)
        | RProd (a, b) -> M.map2 ~f:product (f a) (f b)
        | RComp a -> M.map ~f:comp @@ f a
        | RProj (flds, a) -> M.map ~f:(project ~flds) @@ f a
        | RRestr (equiv, a) -> M.map ~f:(restrict ~equiv) @@ f a
      ;;
    end
  end
end

include Fix.Make (F)

(* -- Standard interfaces --------------------------------------------------- *)
let rec compare x y = F.compare compare (proj x) (proj y)
let rec equal x y = F.equal equal (proj x) (proj y)

include Pretty.Make0 (struct
  type nonrec t = t

  let rec pp prec ppf t = F.pp_prec prec pp ppf @@ proj t
  let pp = `WithPrec pp
end)

(* -- Helpers --------------------------------------------------------------- *)

let pred ?ty_idxs pred = embed @@ F.pred pred ?ty_idxs
let union t1 t2 = embed @@ F.union t1 t2
let inter t1 t2 = embed @@ F.inter t1 t2
let product t1 t2 = embed @@ F.product t1 t2
let comp t = embed @@ F.comp t
let project t ~flds = embed @@ F.project t ~flds
let restrict t ~equiv = embed @@ F.restrict t ~equiv

let arity_of t =
  let algebra = function
    | F.RPred (p, _) -> Pred.arity_of p
    | RProj (flds, _) -> List.length flds
    | RRestr (_, n) -> n
    | RComp n -> n
    | RUnion (n, _) -> n
    | RInter (n, _) -> n
    | RProd (m, n) -> m + n
  in
  cata algebra t
;;
