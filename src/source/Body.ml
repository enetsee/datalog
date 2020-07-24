open Core_kernel
open Reporting
include Subgoal.Make (OpSet.Body) (Atom.Term)

(* -- Additional helpers -------------------------------------------------- *)

let neg ?(region = Region.empty) t =
  unOp Located.(locate ~region OpSet.Body.Unary.Neg) t
;;

let conj ?(region = Region.empty) a b =
  binOp Located.(locate ~region OpSet.Body.Binary.Conj) a b
;;

let disj ?(region = Region.empty) a b =
  binOp Located.(locate ~region OpSet.Body.Binary.Disj) a b
;;

(* -- Compilation --------------------------------------------------------- *)

(** Split disjunctions *)
let split_disj t =
  let rec aux ~k t =
    match proj t with
    | SBinOp ({ elem = Disj; _ }, s1, s2) ->
      aux s1 ~k:(fun xs -> aux s2 ~k:(fun ys -> k @@ xs @ ys))
    | _ -> k [ t ]
  in
  aux ~k:Fn.id t
;;

(** Eliminate double negation top-down but stop when we hit one *)
let elim_neg t =
  let r_coalgebra t =
    match proj2 t with
    | SUnOp ({ elem = Neg; _ }, SUnOp ({ elem = Neg; _ }, s)) -> Error s
    | _ -> Ok t
  in
  transform_partial r_coalgebra t
;;

(** Push negation towards atoms *)
let push_neg t =
  let coalgebra t =
    match proj2 t with
    | SUnOp ({ elem = Neg; region }, SBinOp ({ elem = Conj; _ }, s1, s2)) ->
      (*  !(s1 /\ s2) <=> !s1 \/ !s2 *)
      disj ~region (elim_neg @@ neg s1 ~region) (elim_neg @@ neg s2 ~region)
    | SUnOp ({ elem = Neg; region }, SBinOp ({ elem = Disj; _ }, s1, s2)) ->
      (* !(s1 \/ s2) <=> !s1 /\ !s2 *)
      conj ~region (elim_neg @@ neg s1 ~region) (elim_neg @@ neg s2 ~region)
    | _ -> elim_neg t
  in
  transform_top_down coalgebra t
;;

(** Move disjuntion upwards *)
let collect_disj t =
  let algebra t =
    match proj2 t with
    | SBinOp
        ( { elem = Conj; region }
        , SBinOp ({ elem = Disj; _ }, s1, s2)
        , SBinOp ({ elem = Disj; _ }, s3, s4) ) ->
      (* (s1 + s2) * (s3 + s4) <=>
            (s1 * s3) + (s1 * s4) + (s2 * s3) + (s2 * s4) *)
      disj ~region (conj ~region s1 s3)
      @@ disj ~region (conj ~region s1 s4)
      @@ disj ~region (conj ~region s2 s3) (conj ~region s2 s4)
    | SBinOp ({ elem = Conj; region }, SBinOp ({ elem = Disj; _ }, s1, s2), s3)
      ->
      (* (s1 + s2) * s3 <=> (s1 * s3)  + (s2 * s3) *)
      disj ~region (conj ~region s1 @@ embed s3) (conj ~region s2 @@ embed s3)
    | SBinOp ({ elem = Conj; region }, s1, SBinOp ({ elem = Disj; _ }, s2, s3))
      ->
      (* s1 * (s2 + s3) <=> (s1 * s2) + (s1 * s3) *)
      disj ~region (conj ~region (embed s1) s2) (conj ~region (embed s1) s3)
    | _ -> t
  in
  transform_bottom_up algebra t
;;

let dnf t = collect_disj @@ push_neg t
let normalize t = split_disj @@ dnf t

module Make (M : SourceM.S) = struct
  module TermM = Atom.Term.Make (M)

  let unop_to_core region op mxs =
    M.(
      mxs
      >>= fun xs ->
      match op, xs with
      | OpSet.Body.Unary.Neg, [ lit ] -> return [ Core.Lit.Raw.neg lit ]
      | _ -> err_clause_neg region)
  ;;

  let binop_to_core region op mxs mys =
    M.(
      match op with
      | OpSet.Body.Binary.Conj -> map2 ~f:List.append mxs mys
      | Disj -> err_clause_disj region)
  ;;

  let to_core subg =
    let algebra subg =
      M.(
        match subg with
        | SubgoalF.SAtom { elem; _ } ->
          map ~f:(fun cl -> [ cl ]) @@ TermM.to_core elem
        | SUnOp ({ elem; region }, mcls) -> unop_to_core region elem mcls
        | SBinOp ({ elem; region }, mxs, mys) ->
          binop_to_core region elem mxs mys
        | SNullOp op -> err_clause_null_op @@ Located.region_of op)
    in
    cata algebra subg
  ;;
end
