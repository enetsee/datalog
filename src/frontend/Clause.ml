open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    { head : Core.Term.t Subgoal.t
    ; body : Core.Term.t Subgoal.t
    }
  [@@deriving eq, compare]

  let clause head body = { head; body }

  let pp ppf { head; body } =
    Fmt.(
      hovbox
      @@ pair
           ~sep:(any " :-@, ")
           (Subgoal.pp Core.Term.pp)
           (Subgoal.pp Core.Term.pp ++ any "."))
      ppf
      (head, body)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Query ----------------------------------------------------------------- *)

let atoms { head; body } = Subgoal.atoms head @ Subgoal.atoms body

let tmvars { head; body } =
  Subgoal.(tmvars Core.Term.tmvars head @ tmvars Core.Term.tmvars body)
;;

(* -- Core translation ------------------------------------------------------ *)

let term_to_core x = Logger.return x

(** Translate head of the clause to a literal guarding against it _not_ 
    being a literal *)
let head_to_core sg =
  Logger.(
    match Subgoal.proj sg with
    | { elem = SAtom a; _ } -> Atom.to_core term_to_core a
    | { region; _ } -> fail @@ head_not_atom region)
;;

(** Flatten subgoal into a conjunction of literals.

    If the previous stages have been applied we should have:
    - negations applied to atoms only 
    - conjunctions of literals only 
    - disjunctions eliminated by splitting rules
    - no nullary operations

    This translation will fail if this is not the case.

    TODO: is this clearer as a paramorphism? How will this work when
    the frontend is generalized over operations?
*)
let body_to_core sg =
  let rec aux sg =
    match Located.elem_of sg with
    (* If the subgoal is an atom return the singleton list containing the
       literal *)
    | Subgoal.Shape.Raw.SAtom a ->
      Logger.map ~f:NonEmpty.singleton @@ Atom.to_core term_to_core a
    (* If this is a unary negation, it should occur being applied to atoms.
       If it _is_ applied to an atom we will have exactly one literal so
       we can simply toggle its polarity. Otherwise, fail.
    *)
    | SUnOp ({ elem = Op.Neg; region }, mxs) ->
      Logger.(
        mxs
        >>= fun xs ->
        (match NonEmpty.to_list xs with
        | [ a ] ->
          return @@ NonEmpty.singleton @@ Core.Literal.toggle_polarity a
        | _ -> fail @@ clause_negation region))
    (* Just in case we have somehow ended up with a non-unary operator, fail *)
    | SUnOp ({ region; _ }, _) -> Logger.(fail @@ clause_unop region)
    (* In the case of conjuction, simply append the literals *)
    | SBinOp ({ elem = Op.Conj; _ }, mxs, mys) ->
      Logger.map2 ~f:NonEmpty.append mxs mys
    (* We should have eliminated disjunctions by now so fail *)
    | SBinOp ({ elem = Op.Disj; region }, mxs, mys) ->
      Logger.(fail @@ clause_disjunction region)
    (* Just in case we have somehow ended up with a non-binary op, fail *)
    | SBinOp ({ region; _ }, _, _) -> Logger.(fail @@ clause_binop region)
    (* Nullary operators cannot be translated *)
    | SNullOp _ -> Logger.(fail clause_nullop)
  in
  Subgoal.cata aux sg
;;

(** Translate frontend-clauses to flattened core representation 
    TODO: the use of `Located.t` is making things messy. Possibly revisit the
    way in which we store source locations.
*)
let to_core { head; body } =
  let head_region = Located.region_of @@ Subgoal.proj head
  and body_region = Located.region_of @@ Subgoal.proj body in
  let annot = Core.Annotation.base @@ Region.merge head_region body_region in
  Logger.map2
    ~f:(fun head body -> Core.Clause.{ head; body; annot })
    (head_to_core head)
    (body_to_core body)
;;
