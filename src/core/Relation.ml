open Core_kernel
open Lib

module F = struct
  type 'a t =
    | RPred of Pred.t
    | RUnion of 'a * 'a
    | RInter of 'a * 'a
    | RProd of 'a * 'a
    | RComp of 'a
    | RProj of Int.Set.t * 'a
    | RRestr of (int * int) * 'a
  [@@deriving map, compare, eq]

  let pred pred = RPred pred
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

    let pp_projset ppf s =
      Fmt.(braces @@ list ~sep:comma int) ppf @@ Int.Set.to_list s
    ;;

    let pp_eqs = Fmt.(hbox @@ pair ~sep:(any "@;~@;") int int)

    let pp prec pp_a ppf = function
      | RPred pr -> Pred.pp ppf pr
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
            Fmt.(hbox @@ prefix (any "pi@;") @@ pair pp_projset @@ pp_a prec')
          in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (s, a)
      | RRestr (eq, a) ->
        let prec' = 4 in
        let fmt =
          let g =
            Fmt.(hbox @@ prefix (any "sig@;") @@ pair pp_eqs @@ pp_a prec')
          in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (eq, a)
    ;;

    let pp = `WithPrec pp
  end)
end

include Fix.Make (F)

(* -- Helpers --------------------------------------------------------------- *)

let pred pred = embed @@ F.pred pred
let union t1 t2 = embed @@ F.union t1 t2
let inter t1 t2 = embed @@ F.inter t1 t2
let product t1 t2 = embed @@ F.product t1 t2
let comp t = embed @@ F.comp t
let project t ~flds = embed @@ F.project t ~flds
let restrict t ~equiv = embed @@ F.restrict t ~equiv

let arity_of t =
  let algebra = function
    | F.RPred p -> Pred.arity_of p
    | RProj (flds, _) -> Set.length flds
    | RRestr (_, n) -> n
    | RComp n -> n
    | RUnion (n, _) -> n
    | RInter (n, _) -> n
    | RProd (m, n) -> m + n
  in
  cata algebra t
;;

(* -- Core translation ------------------------------------------------------ *)

(** Translate a body literal to a named relation, applying projection and negation if required,
      and accumulate the variable bound along with their indices and any term variable equivalence
      constraints arising from variables bound earlier in the body *)
let of_lit lit (n, bound, equivs) =
  let pr = Lit.Adorned.pred_of lit
  and vars =
    List.filter_mapi
      Lit.Adorned.(terms_of lit)
      ~f:(fun idx -> function
        | Term.TVar (v, _) -> Some (idx, v)
        | _ -> None)
  in
  let nvars = List.length vars
  and arity = Pred.arity_of pr in
  (* Determine the 'global' index of bound vars in the eventual cross product *)
  let bs = List.mapi ~f:(fun idx (_, v) -> idx + n, v) vars in
  (* For each bound variable, find the index of eariler occurrences *)
  let eqs =
    List.filter_map bs ~f:(fun (n, v) ->
        match List.find bound ~f:(fun (_, w) -> Tmvar.equal v w) with
        | Some (m, _) -> Some (m, n)
        | _ -> None)
  in
  (* update the state:
     - number of variables in the cross product
     - bound variables and their indices
     - equivalence contraints between bound variable indices *)
  let st = n + nvars, bound @ bs, equivs @ eqs in
  (* Construct relation *)
  let r =
    (if Polarity.isPos @@ Lit.Adorned.pol_of lit then Fn.id else comp)
    @@ (if nvars = arity
       then Fn.id
       else project ~flds:(Int.Set.of_list @@ List.map ~f:fst vars))
    @@ pred pr
  in
  st, r
;;

let of_body body =
  let (_, bound, equivs), r =
    match body with
    | x :: xs ->
      List.fold_left
        xs
        ~init:(of_lit x (0, [], []))
        ~f:(fun (st, accu) lit ->
          let st', r = of_lit lit st in
          st', product accu r)
    | _ -> failwith "Not intensional"
  in
  ( bound
  , List.fold_right ~f:(fun equiv accu -> restrict ~equiv accu) ~init:r equivs )
;;

(** Example: 
  
    query(x,y) :- one(x,z,_), two(z,y), three(x,y).
    
    ===
    
    RProj (RProd(RProd( , RPred 'two') , RPred 'three'))
  *)
let of_clause Clause.Adorned.{ head; body; _ } =
  let bound, r = of_body body in
  let head_vars = Lit.Adorned.vars_of head in
  let flds =
    Int.Set.of_list
    @@ List.map head_vars ~f:(fun v ->
           match List.find bound ~f:(fun (_, w) -> Tmvar.equal v w) with
           | Some (idx, _) -> idx
           | _ -> failwith "Not range restricted")
  in
  if arity_of r = List.length head_vars then r else project r ~flds
;;

let of_clauses cls =
  match cls with
  | [] -> failwith "empty"
  | x :: xs ->
    List.fold_left xs ~init:(of_clause x) ~f:(fun accu cl ->
        union accu @@ of_clause cl)
;;

(* -- Standard interfaces --------------------------------------------------- *)
let rec compare x y = F.compare compare (proj x) (proj y)
let rec equal x y = F.equal equal (proj x) (proj y)

include Pretty.Make0 (struct
  type nonrec t = t

  let rec pp prec ppf t = F.pp_prec prec pp ppf @@ proj t
  let pp = `WithPrec pp
end)
