open Core_kernel
open Lib

module Var = struct
  module Minimal = struct
    (** A `Var` is either a named variable or a wildcard *)
    type t =
      | Named of Tmvar.t
      | Wild of int
    [@@deriving eq, compare, sexp]

    let pp ppf = function
      | Named v -> Tmvar.pp ppf v
      | Wild _ -> Fmt.char ppf '_'
    ;;

    let pp = `NoPrec pp
  end

  include Minimal
  include Pretty.Make0 (Minimal)

  module Set = struct
    include Set.Make (Minimal)

    include PartialOrd.Make (struct
      type nonrec t = t

      let leq x y = is_subset x ~of_:y
    end)

    include Pretty.Make0 (struct
      type nonrec t = t

      let pp ppf xs =
        Fmt.(hovbox @@ braces @@ list ~sep:comma pp) ppf @@ elements xs
      ;;

      let pp = `NoPrec pp
    end)
  end
end

module Obligation = struct
  include Set.Make (Var.Set)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf t =
      Fmt.(hovbox @@ braces @@ list ~sep:comma Var.Set.pp) ppf @@ elements t
    ;;

    let pp = `NoPrec pp
  end)

  let min_of in_ =
    let xs, orig =
      match in_ with
      | `ListIn xs -> xs, of_list xs
      | `SetIn orig -> to_list orig, orig
    in
    of_list
    @@ List.filter
         ~f:(fun x -> not @@ exists ~f:(fun y -> Var.Set.lt y x) orig)
         xs
  ;;

  let trivial = singleton Var.Set.empty
  let is_trivial t = equal trivial t
end

module Vertex = struct
  (** A vertex in a schedule graph contains:    
    - `available` literals yet to be scheduled with their original position in 
      the clause and associated obligation
    - `bound` variables of the scheduled literals
    - `debt` i.e. the variables that must be bound in the head of the clause
      to make the ordering well-moded.
    *)
  type t =
    { available : (int * Raw.Lit.t * Obligation.t) list
    ; bound : Var.Set.t
    ; debt : Var.Set.t
    }
  [@@deriving compare, eq]

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp_alt_elem ppf (_, lit, _) = Pred.pp ppf @@ Raw.Lit.pred_of lit

    let pp_alt ppf alt =
      Fmt.(hovbox @@ parens @@ list ~sep:comma pp_alt_elem) ppf alt
    ;;

    let pp ppf { available; bound; debt } =
      Fmt.(
        hovbox
        @@ pair ~sep:comma pp_alt
        @@ pair
             ~sep:sp
             (prefix (any "bound: ") Var.Set.pp)
             (prefix (any "debt: ") Var.Set.pp))
        ppf
        (available, (bound, debt))
    ;;

    let pp = `NoPrec pp
  end)
end

module Edge = struct
  (** An edge contains:
      - `scheduled` literals from those available in the source vertex; and 
      - `cost` of scheduling those literals *)
  type t =
    { scheduled : Raw.Lit.Set.t
    ; cost : Var.Set.t
    }
  [@@deriving compare, eq]

  let empty = { scheduled = Raw.Lit.Set.empty; cost = Var.Set.empty }

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { scheduled; cost } =
      Fmt.(
        hovbox @@ pair ~sep:comma (braces @@ list ~sep:comma Pred.pp) Var.Set.pp)
        ppf
        (List.map ~f:Raw.Lit.(pred_of) @@ Raw.Lit.Set.elements scheduled, cost)
    ;;

    let pp = `NoPrec pp
  end)
end

(** A schedule graph encodes the ordering of body literals *)
type t =
  { head_vars : (int * Var.t) list
  ; graph : (Vertex.t * Edge.t list) Tree.t
  }
[@@deriving compare, eq]

(* == Graph construction ==================================================== *)

(** Helper to get the constraint of a literal. If a literal has negative 
    polarity than all variables are required, otherwise try and find the 
    constraint in the provided map and if there is none return the trivial 
    constraint.
*)
let constraint_of lit ~cstrs =
  match Raw.Lit.pol_of lit with
  | Polarity.Pos ->
    Option.value ~default:Constraint.trivial
    @@ Pred.Map.find cstrs
    @@ Raw.Lit.pred_of lit
  | Neg ->
    let n = Pred.arity_of @@ Raw.Lit.pred_of lit in
    Constraint.(singleton @@ Atomic.of_list @@ List.init n ~f:(fun i -> i))
;;

(** The obligation of a literal is a translation from a dataflow constraint to 
    the literals variables *)
let obligation_of lit ~cstr =
  let terms = Raw.Lit.terms_of lit in
  Obligation.of_list
  @@ List.map ~f:(fun acstr ->
         Var.Set.of_list
         @@ List.filter_mapi terms ~f:(fun idx ->
              function
              | _ when not @@ Constraint.Atomic.mem acstr idx -> None
              | Term.TVar (v, _) -> Some (Var.Named v)
              | TWild _ -> Some (Var.Wild idx)
              | _ -> None))
  @@ Constraint.elements cstr
;;

(** Non-existential variables of a literal *)
let vars lit =
  List.filter_mapi ~f:(fun idx ->
    function
    | Term.TVar (v, _) -> Some (idx, Var.Named v)
    | _ -> None)
  @@ Raw.Lit.terms_of lit
;;

let varset lit =
  Var.Set.of_list @@ List.map ~f:(fun v -> Var.Named v) @@ Raw.Lit.vars_of lit
;;

(* -- Vertex construction --------------------------------------------------- *)

(** Build the root vertex from the clause and constraint function *)
let root cl ~cstrs =
  Vertex.
    { bound = Var.Set.empty
    ; debt = Var.Set.empty
    ; available =
        List.mapi (Raw.Clause.body_of cl) ~f:(fun idx lit ->
            idx, lit, obligation_of lit ~cstr:(constraint_of ~cstrs lit))
    }
;;

(** The next available literals are the previously available literals with 
    scheduled literals removed 
*)
let nextAvailable Vertex.{ available; _ } Edge.{ scheduled; _ } =
  List.filter available ~f:(fun (_, lit, _) ->
      not @@ Raw.Lit.Set.mem scheduled lit)
;;

(** The next set of bound variables are  the current set of bound variables 
    and the union of the (non-wildcard) variables of the scheduled literals 
*)
let nextBound Vertex.{ bound; _ } Edge.{ scheduled; _ } =
  Var.Set.union_list
  @@ List.cons bound
  @@ List.map ~f:varset
  @@ Raw.Lit.Set.elements scheduled
;;

(** The cost at the next vertex is the cost of the previous vertex union
    the cost of the edge 
*)
let nextCost Vertex.{ debt; _ } Edge.{ cost; _ } = Var.Set.union debt cost

let mk_vertex vtx edge =
  Vertex.
    { available = nextAvailable vtx edge
    ; bound = nextBound vtx edge
    ; debt = nextCost vtx edge
    }
;;

(* -- Selection procedure --------------------------------------------------- *)

(** The cost of scheduling a literal relative to what is already bound.
    Note that wildcards are included in the cost of a literal but never in 
    the set of bound variables. Consequently wildcards will never be eliminated
    by scheduling other literals
*)
let cost_of Vertex.{ bound; _ } (_, _, obligation) =
  Obligation.min_of
  @@ `ListIn
       (List.map ~f:(fun cost -> Var.Set.diff cost bound)
       @@ Obligation.elements obligation)
;;

(** A literal is affordable if it has a cost (i.e. an element of its obligation
    less the already bound variables) which is a subset of the variable
    appearing in the head of the clause. 

    This ensures that we only schedule literals with an obligation that has
    already been paid _or_ could be paid by requiring a variable to be bound
    in the head of the clause.
   
*)
let affordable (idx, lit, costs) ~head_vars =
  let hv = Var.Set.of_list @@ List.map ~f:snd head_vars in
  List.filter_map ~f:(fun cost ->
      if Var.Set.is_subset cost ~of_:hv then Some (idx, lit, cost) else None)
  @@ Obligation.to_list costs
;;

(** A literal is restricted from being scheduled by its effects if:
    1) It has any effect; *and*
    2) Scheduling the literal would change the relative order with respect to 
       other literals with and intersecting set of effects
*)
let restricted_effects Vertex.{ available; _ } (idx, lit, cost) =
  let eff = Raw.Lit.effects_of lit in
  if Eff.Set.is_empty eff
  then Some (lit, cost)
  else (
    let restricted =
      List.exists available ~f:(fun (idx2, lit2, _) ->
          if idx2 >= idx
          then false
          else (
            let eff2 = Raw.Lit.effects_of lit2 in
            not @@ Eff.Set.is_empty @@ Eff.Set.inter eff eff2))
    in
    if restricted then None else Some (lit, cost))
;;

(** The selection criteria used for constructing the minimal obligation graph

  1a) If there are any free literals i.e. those we can schedule without 
      increasing the debt, use them as candidate
  1b) If there are no free literals, select the costly literals which _can_ be 
      scheduled; this will eliminate any literal containing a wildcard or any 
      variable not appearing in the head of the clause and not already bound

  2) With the candidate literals from step 1, remove those which are effectful
     and occur _after_ other interacting literals in the original ordering.
*)
let select Vertex.({ available; _ } as vtx) ~head_vars =
  let free, costly =
    List.partition_tf ~f:(fun (_, _, cost) -> Obligation.is_trivial cost)
    @@ List.map available ~f:(fun (idx, lit, oblig) ->
           idx, lit, cost_of vtx (idx, lit, oblig))
  in
  let cands =
    match free with
    | [] -> List.concat_map costly ~f:(affordable ~head_vars)
    | _ -> List.map ~f:(fun (idx, lit, _) -> idx, lit, Var.Set.empty) free
  in
  List.filter_map ~f:(restricted_effects vtx) cands
;;

(** Given the selected literals, group them by cost and construct edges *)
let edges selected =
  let mk_edge lits cost = Edge.{ scheduled = Raw.Lit.Set.of_list lits; cost } in
  let rec aux accu lits cur_cost = function
    | [] ->
      Option.value_map cur_cost ~default:accu ~f:(fun cost ->
          mk_edge lits cost :: accu)
    | (lit, cost) :: rest ->
      (match cur_cost with
      | Some cost' when Var.Set.equal cost cost' ->
        aux accu (lit :: lits) cur_cost rest
      | Some cost' -> aux (mk_edge lits cost' :: accu) [ lit ] (Some cost) rest
      | _ -> aux accu [ lit ] (Some cost) rest)
  in
  aux [] [] None
  @@ List.sort ~compare:(fun (_, c1) (_, c2) -> Var.Set.compare c1 c2) selected
;;

(** Construct the minimum obligation graph of a clause given a set of predicate
    constriants 
*)
let of_clause cl ~cstrs =
  let head_vars = vars @@ Raw.Clause.head_of cl in
  let nexts vtx =
    let es = edges @@ select vtx ~head_vars in
    (vtx, es), List.map ~f:(mk_vertex vtx) es
  in
  { head_vars; graph = Tree.unfold ~f:nexts @@ root ~cstrs cl }
;;

(* == Constraint extraction ================================================= *)

(** Convert a debt to a atomic constraint *)
let atomic_constraint_of_debt debt ~head_vars =
  Constraint.Atomic.of_list
  @@ List.filter_map
       ~f:(fun (idx, v) -> if Var.Set.mem debt v then Some idx else None)
       head_vars
;;

(** Convert a debt to a singleton constraint set *)
let constraint_of_debt debt ~head_vars =
  Constraint.(singleton @@ atomic_constraint_of_debt ~head_vars debt)
;;

(** Extract the possible mode patterns for the variables in the head of the 
    clause.
    
    Note that we only cosider the debt at _terminal_ vertices i.e. those for 
    which all literals have been scheduled 
*)
let extract { head_vars; graph } =
  Constraint.join_list
  @@ List.filter_map ~f:(fun (Vertex.{ available; debt; _ }, _) ->
         if List.is_empty available
         then Some (constraint_of_debt ~head_vars debt)
         else None)
  @@ Tree.leaves graph
;;

(* == Ordering extraction =================================================== *)

let permutations t =
  let neq x y = not @@ Raw.Lit.equal x y in
  let rec aux = function
    | [] -> [ [] ]
    | xs ->
      List.concat_map xs ~f:(fun x ->
          List.map ~f:(fun xs -> x :: xs) @@ aux @@ List.filter ~f:(neq x) xs)
  in
  aux t
;;

let expand pss =
  List.fold_left ~init:[ [] ] pss ~f:(fun paths litset ->
      let segments = permutations @@ Set.elements litset in
      List.(paths >>= fun path -> segments >>= fun seg -> return (path @ seg)))
;;

let bound_by { head_vars; _ } ~bpatt =
  Var.Set.of_list
  @@ List.filter_mapi ~f:(fun idx ->
       function
       | Adornment.Bound ->
         Some (snd @@ List.find_exn head_vars ~f:(fun (i, _) -> i = idx))
       | _ -> None)
  @@ BindingPatt.to_list bpatt
;;

let consistent_paths ({ graph; _ } as t) ~bpatt =
  let bound = bound_by t ~bpatt in
  let rec flatten
      ( Edge.{ scheduled; _ }
      , Tree.Node ((Vertex.{ available; debt; _ }, edges_out), ts) )
    =
    match ts with
    | [] ->
      if List.is_empty available
      then if Var.Set.is_subset debt ~of_:bound then [ [ scheduled ] ] else []
      else []
    | ts ->
      List.map ~f:(fun xs -> scheduled :: xs)
      @@ List.concat_map ~f:flatten
      @@ List.zip_exn edges_out ts
  in
  flatten (Edge.empty, graph)
;;

let orderings t ~bpatt = List.concat_map ~f:expand @@ consistent_paths t ~bpatt

let pp_path ppf =
  Fmt.(vbox @@ list ~sep:cut @@ hbox @@ brackets @@ list ~sep:comma Raw.Lit.pp)
    ppf
;;

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp_lbl ppf (vtx, edges) =
    Fmt.(hbox @@ pair ~sep:sp Vertex.pp (braces @@ list ~sep:comma Edge.pp))
      ppf
      (vtx, edges)
  ;;

  let pp_head_vars ppf vs =
    Fmt.(hbox @@ braces @@ list ~sep:comma @@ pair ~sep:(any "@") int Var.pp)
      ppf
      vs
  ;;

  let pp ppf { head_vars; graph } =
    Fmt.(vbox @@ pair ~sep:cut pp_head_vars (Tree.pp pp_lbl))
      ppf
      (head_vars, graph)
  ;;

  let pp = `NoPrec pp
end)
