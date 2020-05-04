open Core_kernel
open Reporting
open Lib

(* Subgoal is the workhorse of the front-end and requires quite a bit 
  of transformation to get into normal-form. For this reason, it is
  represented as  the fixpoint of a shape functor and work with 
  recursion schemes.
  
  We also keep track of source locations so actually wrap the shape
  in a `Located.t` and use the fixpoint of that instead. 
*)

module Shape = struct
  module Raw = struct
    type ('a, 'term) t =
      | SAtom of 'term Atom.t
      | SNullOp of Op.t
      | SUnOp of Op.t Located.t * 'a
      | SBinOp of Op.t Located.t * 'a * 'a
    [@@deriving eq, compare]

    let pp_prec prec pp_a pp_b ppf = function
      | SAtom a -> Atom.pp_prec prec pp_b ppf a
      | SNullOp op -> Op.pp ppf op
      | SUnOp (op, f) ->
        let prec' = Op.precedence op.elem in
        let fmt =
          let g = Fmt.(hbox @@ pair (Located.pp Op.pp) @@ pp_a prec') in
          if prec' < prec then Fmt.(parens g) else g
        in
        fmt ppf (op, f)
      | SBinOp (op, p, q) ->
        let prec' = Op.precedence op.elem in
        let fmt =
          let g =
            Fmt.(
              hovbox
              @@ pair ~sep:sp (pp_a prec')
              @@ pair ~sep:sp (Located.pp Op.pp)
              @@ pp_a prec')
          in
          if prec' < prec then Fmt.parens g else g
        in
        fmt ppf (p, (op, q))
    ;;

    include Pretty.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let pp = `WithPrec pp_prec
    end)

    include Functor.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let map t ~f =
        match t with
        | SAtom a -> SAtom a
        | SNullOp op -> SNullOp op
        | SUnOp (op, a) -> SUnOp (op, f a)
        | SBinOp (op, a, b) -> SBinOp (op, f a, f b)
      ;;
    end)

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
  end

  module Located = struct
    type ('a, 'term) t = ('a, 'term) Raw.t Located.t

    let pp_prec prec pp_a pp_b ppf Located.{ elem; _ } =
      Raw.pp_prec prec pp_a pp_b ppf elem
    ;;

    let equal eq_a eq_b Located.{ elem = a; _ } Located.{ elem = b; _ } =
      Raw.equal eq_a eq_b a b
    ;;

    let compare cmp_a cmp_b Located.{ elem = a; _ } Located.{ elem = b; _ } =
      Raw.compare cmp_a cmp_b a b
    ;;

    let atom a ~region = Located.{ elem = Raw.SAtom a; region }
    let unop op a ~region = Located.{ elem = Raw.SUnOp (op, a); region }
    let binop op a b ~region = Located.{ elem = Raw.SBinOp (op, a, b); region }
    let elem Located.{ elem; _ } = elem
    let region Located.{ region; _ } = region

    include Pretty.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let pp = `WithPrec pp_prec
    end)

    include Functor.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let map t ~f = Located.{ t with elem = Raw.map ~f t.elem }
    end)

    module Traversable (A : Applicative.S) = struct
      module T = Raw.Traversable (A)

      let traverse t ~f =
        A.map ~f:(fun elem -> Located.{ t with elem })
        @@ T.traverse ~f
        @@ elem t
      ;;

      let sequence x = traverse x ~f:Fn.id
    end

    module Traversable2 (A : Applicative.S2) = struct
      module T = Raw.Traversable2 (A)

      let traverse t ~f =
        A.map ~f:(fun elem -> Located.{ t with elem })
        @@ T.traverse ~f
        @@ elem t
      ;;

      let sequence x = traverse x ~f:Fn.id
    end
  end
end

include Fix.Make2 (Shape.Located)

let rec equal eq_term a b =
  Shape.Located.equal (equal eq_term) eq_term (proj a) (proj b)
;;

let rec compare cmp_term a b =
  Shape.Located.compare (compare cmp_term) cmp_term (proj a) (proj b)
;;

let rec pp pp_term ppf t = Shape.Located.pp (pp pp_term) pp_term ppf @@ proj t

module Traversable (A : Applicative.S) = struct
  module T = Shape.Located.Traversable (A)

  let rec traverse t ~f =
    A.map ~f:embed @@ T.traverse ~f:(traverse ~f) @@ proj t
  ;;

  let sequence x = traverse x ~f:Fn.id
end

module Traversable2 (A : Applicative.S2) = struct
  module T = Shape.Located.Traversable2 (A)

  let rec traverse t ~f =
    A.map ~f:embed @@ T.traverse ~f:(traverse ~f) @@ proj t
  ;;

  let sequence x = traverse x ~f:Fn.id
end

(* -- Constructor helpers --------------------------------------------------- *)
let region t = (proj t).region
let atom a ~region = embed @@ Shape.Located.atom a ~region
let unop op a ~region = embed @@ Shape.Located.unop op a ~region
let binop op a b ~region = embed @@ Shape.Located.binop op a b ~region
let conj a b ~region = binop { elem = Conj; region } a b ~region
let disj a b ~region = binop { elem = Disj; region } a b ~region
let neg a ~region = unop { elem = Neg; region } a ~region

(* -- Query ----------------------------------------------------------------- *)

let atoms t =
  let aux t =
    match Shape.Located.elem t with
    | Shape.Raw.SAtom a -> [ a ]
    | SNullOp _ -> []
    | SUnOp (_, xs) -> xs
    | SBinOp (_, xs, ys) -> xs @ ys
  in
  cata aux t
;;

let tmvars tmvars_a t =
  let aux t =
    match Shape.Located.elem t with
    | Shape.Raw.SAtom a -> Atom.tmvars tmvars_a a
    | SNullOp _ -> []
    | SUnOp (_, xs) -> xs
    | SBinOp (_, xs, ys) -> xs @ ys
  in
  cata aux t
;;

(* == Transformations ======================================================= *)

(* -- Transformation helpers ------------------------------------------------ *)

(** Apply a bottom-up transformation to all atoms in a subgoal *)
let transform_atom t ~f =
  let rec aux t =
    match proj t with
    | { elem = SAtom a; region } -> atom ~region @@ f a
    | _ -> t
  in
  transform_bottom_up aux t
;;

module Effect (M : sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t
end) =
struct
  module T = Shape.Located.Traversable (M)

  (** Effectful bottom-up transformation of atoms within a subgoal *)
  let transform_atom t ~f =
    let rec aux = function
      | Located.{ elem = Shape.Raw.SAtom a; region } ->
        M.map ~f:(atom ~region) @@ f a
      | s -> M.map ~f:embed @@ T.sequence s
    in
    cata aux t
  ;;
end

module Logged = Effect (Logger)

module Effect2 (M : sig
  include Monad.S2
  include Applicative.S2 with type ('a, 'b) t := ('a, 'b) t
end) =
struct
  module T = Shape.Located.Traversable2 (M)

  (** Effectful bottom-up transformation of atoms within a subgoal *)
  let transform_atom t ~f =
    let rec aux = function
      | Located.{ elem = Shape.Raw.SAtom a; region } ->
        M.map ~f:(atom ~region) @@ f a
      | s -> M.map ~f:embed @@ T.sequence s
    in
    cata aux t
  ;;
end

(* -- Normalization transforms ---------------------------------------------- *)

(** Split disjunctions *)
let split_disj t =
  let rec aux ~k t =
    match Shape.Located.elem @@ proj t with
    | SBinOp ({ elem = Disj; _ }, s1, s2) ->
      aux s1 ~k:(fun xs -> aux s2 ~k:(fun ys -> k @@ xs @ ys))
    | _ -> k [ t ]
  in
  aux ~k:Fn.id t
;;

(** Eliminate double negation top-down but stop when we hit one *)
let elim_neg t =
  let aux t =
    match Shape.Located.elem @@ proj2 t with
    | SUnOp ({ elem = Neg; _ }, { elem = SUnOp ({ elem = Neg; _ }, s); _ }) ->
      Error s
    | _ -> Ok t
  in
  transform_partial aux t
;;

(** Push negation downwards *)
let push_neg t =
  let aux t =
    match Shape.Located.elem @@ proj2 t with
    | SUnOp
        ( { elem = Neg; region }
        , { elem = SBinOp ({ elem = Conj; _ }, s1, s2); _ } ) ->
      (*  !(s1 /\ s2) <=> !s1 \/ !s2 *)
      disj ~region (elim_neg @@ neg s1 ~region) (elim_neg @@ neg s2 ~region)
    | SUnOp
        ( { elem = Neg; region }
        , { elem = SBinOp ({ elem = Disj; _ }, s1, s2); _ } ) ->
      (* !(s1 \/ s2) <=> !s1 /\ !s2 *)
      conj ~region (elim_neg @@ neg s1 ~region) (elim_neg @@ neg s2 ~region)
    | _ -> elim_neg t
  in
  transform_top_down aux t
;;

(** Move disjuntion upwards *)
let collect_disj t =
  let aux t =
    match Shape.Located.elem @@ proj2 t with
    | SBinOp
        ( { elem = Conj; region }
        , { elem = SBinOp ({ elem = Disj; _ }, s1, s2); _ }
        , { elem = SBinOp ({ elem = Disj; _ }, s3, s4); _ } ) ->
      (* (s1 + s2) * (s3 + s4) <=>
            (s1 * s2) + (s1 * s4) + (s2 * s3) + (s2 * s4) *)
      disj ~region (conj ~region s1 s2)
      @@ disj ~region (conj ~region s1 s4)
      @@ disj ~region (conj ~region s2 s3) (conj ~region s2 s4)
    | SBinOp
        ( { elem = Conj; region }
        , { elem = SBinOp ({ elem = Disj; _ }, s1, s2); _ }
        , s3 ) ->
      (* (s1 + s2) * s3 <=> (s1 * s3)  + (s2 * s3) *)
      disj ~region (conj ~region s1 @@ embed s3) (conj ~region s2 @@ embed s3)
    | SBinOp
        ( { elem = Conj; region }
        , s1
        , { elem = SBinOp ({ elem = Disj; _ }, s2, s3); _ } ) ->
      (* s1 * (s2 + s3) <=> (s1 * s2) + (s1 * s3) *)
      disj ~region (conj ~region (embed s1) s2) (conj ~region (embed s1) s3)
    | _ -> t
  in
  transform_bottom_up aux t
;;
