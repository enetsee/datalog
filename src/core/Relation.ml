open Core_kernel
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

(* -- Core translation ------------------------------------------------------ *)

module type RelationM = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val get_param_ty : Name.t -> Ty.t option t
  val err_unbound_param : Name.t -> Region.t -> _ t
end

module Make (M : RelationM) = struct
  let of_knowledge k =
    let pr = Knowledge.pred k in
    M.(
      Knowledge.terms k
      |> List.mapi ~f:(fun idx ->
           function
           | Knowledge.KTerm.KSymbol sym -> return @@ (idx, Symbol.type_of sym)
           | KParam name ->
             get_param_ty name
             >>= (function
             | Some ty -> return (idx, ty)
             | _ -> err_unbound_param name Reporting.Region.empty))
      |> all
      >>= fun ty_idxs -> return @@ pred pr ~ty_idxs)
  ;;

  (** TODO: tidy this up! *)
  let of_knowledge_base kb =
    M.all
    @@ List.map ~f:(function
           | k :: _ as ks ->
             M.(
               List.map ~f:of_knowledge ks
               |> all
               >>= (function
               | r :: rs ->
                 return @@ (Knowledge.pred k, List.fold_left ~f:union ~init:r rs)
               | _ -> failwith "impossible"))
           | _ -> failwith "impossible")
    @@ List.group ~break:(fun k1 k2 ->
           Pred.equal (Knowledge.pred k1) (Knowledge.pred k2))
    @@ List.sort ~compare:(fun k1 k2 ->
           Pred.compare (Knowledge.pred k1) (Knowledge.pred k2))
    @@ Knowledge.Base.to_list kb
  ;;

  (** Translate a body literal to a named relation, applying projection and 
      negation if required, and accumulate the variable bound along with their 
      indices and any term variable equivalence constraints arising from variables 
      bound earlier in the body *)
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
    M.(
      Lit.Adorned.(terms_of lit)
      |> List.filter_mapi ~f:(fun idx ->
           function
           | Term.TSym (sym, _) -> Some (return (idx, Symbol.type_of sym))
           | TParam (nm, region) ->
             Some
               (get_param_ty nm
               >>= function
               | Some ty -> return (idx, ty)
               | _ -> err_unbound_param nm region)
           | _ -> None)
      |> all
      >>= fun ty_idxs ->
      let r =
        (if Polarity.isPos @@ Lit.Adorned.pol_of lit then Fn.id else comp)
        @@ (if nvars = arity
           then Fn.id
           else project ~flds:(List.map ~f:fst vars))
        @@ pred pr ~ty_idxs
      in
      return (st, r))
  ;;

  let of_body body region =
    let mx =
      match body with
      | x :: xs ->
        List.fold_left
          xs
          ~init:(of_lit x (0, [], []))
          ~f:(fun maccu lit ->
            M.(
              maccu
              >>= fun (st, accu) ->
              of_lit lit st >>= fun (st', r) -> return (st', product accu r)))
      | _ -> raise @@ ClauseHasNoBody region
    in
    M.map
      ~f:(fun ((_, bound, equivs), r) ->
        ( bound
        , List.fold_right
            ~f:(fun equiv accu -> restrict ~equiv accu)
            ~init:r
            equivs ))
      mx
  ;;

  (** Translate a datalog clause to the Relational algebra *)
  let of_clause Clause.Adorned.{ head; body; region } =
    let head_vars = Lit.Adorned.vars_of head in
    M.(
      of_body body region
      >>= fun (bound, r) ->
      let flds =
        List.map head_vars ~f:(fun v ->
            match List.find bound ~f:(fun (_, w) -> Tmvar.equal v w) with
            | Some (idx, _) -> idx
            | _ -> failwith "Not range restricted")
      in
      return @@ project r ~flds)
  ;;

  let of_clauses cls =
    match cls with
    | [] -> raise NoClauses
    | x :: xs ->
      List.fold_left xs ~init:(of_clause x) ~f:(fun maccu cl ->
          M.map2 ~f:union maccu @@ of_clause cl)
  ;;
end
