open Core_kernel
open Reporting
open Lib

(** A subgoal is an expression over atomic formula that forms the head or body 
    of a Datalog clause, query or fact. 

    Subgoal is abstracted over the type of atoms and the set of operations over
    those atoms.

    Subgoals to do not, in general, have a direct representation in the core 
    language and are wrapped in `Head` or `Body` in order to support transaltion
*)
module type S = sig
  (* Module providing atomic formulae over terms *)
  module Atom : Atom.S

  (* Module providing the type of nullary, unary and binary ops over atoms *)
  module Ops : OpSet.S

  (* Shape of subgoals *)
  module SubgoalF : sig
    type 'a t =
      | SAtom of Atom.t Located.t
      | SNullOp of Ops.Nullary.t Located.t
      | SUnOp of Ops.Unary.t Located.t * 'a
      | SBinOp of Ops.Binary.t Located.t * 'a * 'a

    include Pretty.S1 with type 'a t := 'a t
    include Functor.S1 with type 'a t := 'a t
    include Effect.Traversable.S1 with type 'a t := 'a t

    val atom : Atom.t Located.t -> 'a t
    val nullOp : Ops.Nullary.t Located.t -> 'a t
    val unOp : Ops.Unary.t Located.t -> 'a -> 'a t
    val binOp : Ops.Binary.t Located.t -> 'a -> 'a -> 'a t
  end

  (* Subgoals as fixpoint of F-algebra *)
  include Fix.S with module F := SubgoalF

  (* Standard interfaces *)
  include Pretty.S0 with type t := t
  include Core.HasVars.S with type t := t
  include HasAtoms.S with type t := t and type atom := Atom.t

  (* Helpers *)
  val atom : Atom.t Located.t -> t
  val nullOp : Ops.Nullary.t Located.t -> t
  val unOp : Ops.Unary.t Located.t -> t -> t
  val binOp : Ops.Binary.t Located.t -> t -> t -> t
  val region : t -> Region.t
end

module Make (Ops : OpSet.S) (Atom : Atom.S) :
  S with module Ops := Ops and module Atom := Atom = struct
  module SubgoalF = struct
    type 'a t =
      | SAtom of Atom.t Located.t
      | SNullOp of Ops.Nullary.t Located.t
      | SUnOp of Ops.Unary.t Located.t * 'a
      | SBinOp of Ops.Binary.t Located.t * 'a * 'a

    (* -- Helpers ----------------------------------------------------------- *)

    let atom a = SAtom a
    let nullOp op = SNullOp op
    let unOp op a = SUnOp (op, a)
    let binOp op a b = SBinOp (op, a, b)

    (* -- Pretty implementation --------------------------------------------- *)
    let pp_prec prec pp_a ppf = function
      | SAtom { elem; _ } -> Atom.pp_prec prec ppf elem
      | SNullOp { elem; _ } -> Ops.Nullary.pp ppf elem
      | SUnOp (op, f) ->
        let prec' = Ops.Unary.precedence op.elem in
        let fmt =
          let g = Fmt.(hbox @@ pair (Located.pp Ops.Unary.pp) @@ pp_a prec') in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (op, f)
      | SBinOp (op, p, q) ->
        let prec' = Ops.Binary.precedence op.elem in
        let fmt =
          let g =
            Fmt.(
              hovbox
              @@ pair ~sep:sp (pp_a prec')
              @@ pair ~sep:sp (Located.pp Ops.Binary.pp)
              @@ pp_a prec')
          in
          if prec' < prec then Fmt.parens g else g
        in
        fmt ppf (p, (op, q))
    ;;

    include Pretty.Make1 (struct
      type nonrec 'a t = 'a t

      let pp = `WithPrec pp_prec
    end)

    (* -- Functor implementation -------------------------------------------- *)
    include Functor.Make1 (struct
      type nonrec 'a t = 'a t

      let map t ~f =
        match t with
        | SAtom a -> SAtom a
        | SNullOp op -> SNullOp op
        | SUnOp (op, a) -> SUnOp (op, f a)
        | SBinOp (op, a, b) -> SBinOp (op, f a, f b)
      ;;
    end)

    (* -- Traversable implementation ---------------------------------------- *)

    module Traversable (A : Applicative.S) = struct
      let traverse x ~f =
        match x with
        | SAtom n -> A.return @@ SAtom n
        | SNullOp op -> A.return @@ SNullOp op
        | SUnOp (op, t) -> A.map ~f:(fun t' -> SUnOp (op, t')) @@ f t
        | SBinOp (op, s, t) ->
          A.map2 ~f:(fun s' t' -> SBinOp (op, s', t')) (f s) (f t)
      ;;

      let sequence x = traverse x ~f:Fn.id
    end

    module Traversable2 (A : Applicative.S2) = struct
      let traverse x ~f =
        match x with
        | SAtom n -> A.return @@ SAtom n
        | SNullOp op -> A.return @@ SNullOp op
        | SUnOp (op, t) -> A.map ~f:(fun t' -> SUnOp (op, t')) @@ f t
        | SBinOp (op, s, t) ->
          A.map2 ~f:(fun s' t' -> SBinOp (op, s', t')) (f s) (f t)
      ;;

      let sequence x = traverse x ~f:Fn.id
    end

    module Traversable3 (A : Applicative.S3) = struct
      let traverse x ~f =
        match x with
        | SAtom n -> A.return @@ SAtom n
        | SNullOp op -> A.return @@ SNullOp op
        | SUnOp (op, t) -> A.map ~f:(fun t' -> SUnOp (op, t')) @@ f t
        | SBinOp (op, s, t) ->
          A.map2 ~f:(fun s' t' -> SBinOp (op, s', t')) (f s) (f t)
      ;;

      let sequence x = traverse x ~f:Fn.id
    end
  end

  include Fix.Make (SubgoalF)

  (* -- Helpers ------------------------------------------------------------- *)

  let atom a = embed @@ SubgoalF.atom a
  let nullOp op = embed @@ SubgoalF.nullOp op
  let unOp op a = embed @@ SubgoalF.unOp op a
  let binOp op a b = embed @@ SubgoalF.binOp op a b

  let rec leftmost a =
    match proj a with
    | SAtom a -> a.region
    | SNullOp op -> op.region
    | SUnOp (op, _) -> op.region
    | SBinOp (_, l, _) -> leftmost l
  ;;

  let rec rightmost a =
    match proj a with
    | SAtom a -> a.region
    | SNullOp op -> op.region
    | SUnOp (op, _) -> op.region
    | SBinOp (_, _, r) -> rightmost r
  ;;

  let region a =
    let left = leftmost a
    and right = rightmost a in
    Region.merge left right
  ;;

  (* -- HasAtoms implementations -------------------------------------------- *)

  let atoms_of t =
    let aux = function
      | SubgoalF.SAtom a -> [ a.elem ]
      | SNullOp _ -> []
      | SUnOp (_, xs) -> xs
      | SBinOp (_, xs, ys) -> xs @ ys
    in
    cata aux t
  ;;

  (* -- HasVars implementations --------------------------------------------- *)

  let vars_of t =
    let aux = function
      | SubgoalF.SAtom { elem; _ } -> Atom.vars_of elem
      | SNullOp _ -> []
      | SUnOp (_, xs) -> xs
      | SBinOp (_, xs, ys) -> xs @ ys
    in
    cata aux t
  ;;

  (* -- Pretty implementation ----------------------------------------------- *)

  include Pretty.Make0 (struct
    type nonrec t = t

    let rec pp_prec prec ppf t = SubgoalF.pp_prec prec pp_prec ppf @@ proj t
    let pp = `WithPrec pp_prec
  end)
end
