open Core_kernel
open Lib
open Reporting

module Lit = struct
  type t =
    { pol : Polarity.t
    ; bpatt : BindingPatt.t
    ; pred : Pred.t
    ; terms : Term.t list
    ; region : Region.t [@compare.ignore]
    }
  [@@deriving compare, sexp]

  let bpatt_of { bpatt; _ } = bpatt

  (** A literal is well moded when its terms are compatible 
      at least one possible mode constraint *)
  let well_moded { bpatt; _ } ~cnstr = BindingPatt.well_moded bpatt ~cnstr

  let adorn Raw.Lit.{ pred; terms; pol; region } ~bpatt =
    let vs =
      List.filter_map ~f:(function
          | Adornment.Bound, Term.TVar (x, _) -> Some x
          | _ -> None)
      @@ List.zip_exn BindingPatt.(to_list bpatt) terms
    in
    { pred; terms; pol; bpatt; region }, Tmvar.Set.of_list vs
  ;;

  (** Given a set of bound variables, adorn the literal and update the set *)
  let from_raw Raw.Lit.{ pred; terms; pol; region } ~bound =
    let ads, bound' =
      List.fold_left ~init:([], bound) terms ~f:(fun (ads, bs) tm ->
          match tm with
          | Term.TSym _ -> Adornment.Bound :: ads, bs
          | TVar (v, _) when Tmvar.Set.mem bs v -> Adornment.Bound :: ads, bs
          | TVar (v, _) -> Adornment.Free :: ads, Tmvar.Set.add bs v
          | TWild _ -> Adornment.Free :: ads, bs)
    in
    let bpatt = BindingPatt.from_list @@ List.rev ads in
    { pred; terms; pol; bpatt; region }, bound'
  ;;

  (* -- Lit implementation -------------------------------------------------- *)
  let pol_of { pol; _ } = pol
  let pred_of { pred; _ } = pred
  let neg t = { t with pol = Polarity.toggle t.pol }

  (* -- HasEffects implementation ------------------------------------------- *)
  let effects_of { pred; _ } = Pred.effects_of pred

  (* -- HasTerms implementation --------------------------------------------- *)
  let terms_of { terms; _ } = terms

  (* -- HasVars implementation ---------------------------------------------- *)
  let vars_of { terms; _ } = List.concat_map ~f:Term.vars_of terms

  (* -- HasRegion implementation -------------------------------------------- *)
  let region_of { region; _ } = region

  (* -- Pretty implementation ----------------------------------------------- *)
  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { pol; pred; bpatt; terms; _ } =
      Fmt.(
        hbox
        @@ pair Polarity.pp
        @@ pair
             (pair Pred.pp
             @@ hbox
             @@ prefix (any "<")
             @@ suffix (any ">")
             @@ BindingPatt.pp)
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (pol, ((pred, bpatt), terms))
    ;;

    let pp = `NoPrec pp
  end)
end

module Clause = struct
  include Clause.Make (Lit)

  (** Generalized clause adornment 
    
        Given the binding pattern for the head, and an optional ordering of literals,
        adorn the body of a clause 
    *)
  let adorn cl ~bpatt ~ord =
    Option.(
      ord bpatt cl
      >>= fun Raw.Clause.{ head; body; region } ->
      let head', bound = Lit.adorn head ~bpatt in
      let body' =
        List.rev
        @@ fst
        @@ List.fold_left ~init:([], bound) body ~f:(fun (ls, bound) lit ->
               let lit', bound' = Lit.from_raw lit ~bound in
               lit' :: ls, bound')
      in
      Some { head = head'; body = body'; region })
  ;;

  let well_moded { body; _ } ~cstrs =
    List.for_all body ~f:(fun lit ->
        let cnstr =
          Option.value ~default:Constraint.trivial
          @@ Pred.Map.find cstrs
          @@ Lit.pred_of lit
        in
        Lit.well_moded lit ~cnstr)
  ;;
end

module Program = struct
  include Program.Make (Lit) (Clause)

  (* -- Generalized program adornment -------------------------------------------------- *)

  module WorkItem = struct
    module X = struct
      type t = Pred.t * BindingPatt.t [@@deriving compare, sexp]

      let pp ppf (p, bp) =
        Fmt.(hbox @@ pair Pred.pp @@ braces @@ BindingPatt.pp) ppf (p, bp)
      ;;

      let pp = `NoPrec pp
      let from_literal Lit.{ pred; bpatt; _ } = pred, bpatt
      let from_clause Clause.{ body; _ } = List.map ~f:from_literal body
    end

    include X
    include Pretty.Make0 (X)
    module Set = Set.Make (X)
  end

  (** Find all clauses for which the predicate is the conclusion *)
  let find_clauses clauses pred =
    List.filter
      ~f:(fun cl -> Pred.equal pred @@ Raw.Clause.head_pred_of cl)
      clauses
  ;;

  (** Find the single clause for the query predicate 
        If there is more than one clause then this is not a query
    *)
  let query_clause clauses pred =
    match find_clauses clauses pred with
    | [ x ] -> Some x
    | _ -> None
  ;;

  (** Given an initial 'worklist' list of predicate/binding pattern pairs
        which have not yet been seen, adorn all clauses in which the predicate 
        is the conclusion and add unseen predicate/binding pairs of each premise of 
        the resulting adorned clauses to the worklist and continue until no clauses
        are generated
    *)
  let work ~ord ~clauses ~seen inits =
    let adorn_clauses (pred, bpatt) =
      Option.all
      @@ List.map ~f:Clause.(adorn ~ord ~bpatt)
      @@ find_clauses clauses pred
    in
    let rec aux seen accu = function
      | [] -> Some (accu, seen)
      | next :: rest ->
        (match adorn_clauses next with
        | None -> None
        | Some acls ->
          let items =
            List.dedup_and_sort
              ~compare:WorkItem.compare
              (rest @ List.concat_map ~f:WorkItem.from_clause acls)
          in
          aux WorkItem.Set.(add seen next) (acls @ accu) items)
    in
    aux seen [] inits
  ;;

  (** Given a global ordering function, raw clauses and a query predicate 
        - adorn the corresponding clause using a binding pattern where all variable are free 
        - add the predicate of each premise to a work like along with its binding pattern
        - accumulate resulting adorned clauses using `adorn_clauses`
    *)
  let adorn_query ~ord ~clauses ~seen qry =
    Option.(
      query_clause clauses qry
      >>= fun qcl ->
      let bpatt =
        BindingPatt.from_list
        @@ List.init qry.Pred.arity ~f:(fun _ -> Adornment.Free)
      in
      Clause.adorn qcl ~ord ~bpatt
      >>= fun aqcl ->
      let items =
        List.filter
          ~f:(fun item -> not @@ WorkItem.Set.mem seen item)
          (WorkItem.from_clause aqcl)
      in
      work ~ord ~clauses ~seen items
      >>= fun (acls, seen') -> Some (aqcl :: acls, seen'))
  ;;

  (** Generalized program adornment *)
  let adorn Raw.Program.{ clauses; cnstrts; queries } ~ord =
    let rec aux (accu, seen) = function
      | [] -> Some { clauses = accu; cnstrts; queries }
      | qry :: rest ->
        (match adorn_query ~ord ~clauses ~seen qry with
        | None -> None
        | Some (cls, seen') -> aux (cls @ accu, seen') rest)
    in
    aux ([], WorkItem.Set.empty) queries
  ;;
end
