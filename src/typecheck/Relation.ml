open Core_kernel
open Reporting
open Core
open Relation__Algebra

module type MonadRelation = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val get_param_ty : Name.t -> Type.Ty.t option t
  val err_unbound_param : Name.t -> Region.t -> _ t
end

module Make (M : MonadRelation) = struct
  let of_knowledge k =
    let pr = Knowledge.pred k in
    M.(
      Knowledge.terms k
      |> List.mapi ~f:(fun idx -> function
           | Knowledge.KTerm.KSymbol sym -> return @@ (idx, Symbol.type_of sym)
           | KParam name ->
             get_param_ty name
             >>= (function
             | Some ty -> return (idx, ty)
             | _ -> err_unbound_param name Reporting.Region.empty))
      |> all
      >>= fun ty_idxs -> return @@ pred pr ~ty_idxs)
  ;;

  (** 
    Group the 
    TODO: tidy this up! 
  *)
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
        ~f:
          (fun idx -> function
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
      |> List.filter_mapi ~f:(fun idx -> function
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
