open Core_kernel
open Core

module Make (M : ScheduleM.S) = struct
  module TreeM = Tree.Make (M)

  (* -- Intra-clausal analysis ---------------------------------------------- *)
  (* == Graph construction ================================================== *)

  (** Helper to get the constraint of a literal. If a literal has negative 
      polarity than all variables are required, otherwise try and find the 
      constraint in the provided map and if there is none return the trivial 
      constraint.
  *)
  let constraint_of lit =
    M.(
      let pred = Lit.Raw.pred_of lit in
      match Lit.Raw.pol_of lit with
      | Polarity.Pos -> get_pred_constraint @@ Pred.name_of pred
      | Neg -> return @@ Constraint.fully_bound @@ Pred.arity_of pred)
  ;;

  (** Non-existential variables of a literal and their indices *)
  let vars lit =
    List.filter_mapi ~f:(fun idx -> function
      | Term.TVar (v, _) -> Some (idx, Var.Named v)
      | _ -> None)
    @@ Lit.Raw.terms_of lit
  ;;

  (** Unique non-existential variables *)
  let varset lit =
    Var.Set.of_list @@ List.map ~f:(fun v -> Var.Named v) @@ Lit.Raw.vars_of lit
  ;;

  (* -- Vertex construction ------------------------------------------------- *)

  (** Build the root vertex from the clause and constraint function *)
  let root cl =
    M.(
      map ~f:Vertex.make
      @@ all
      @@ List.mapi ~f:(fun idx lit ->
             constraint_of lit
             >>= fun cstr ->
             return (idx, lit, Obligation.of_constrained_lit lit ~cstr))
      @@ Clause.Raw.body_of cl)
  ;;

  (** The next available literals are the previously available literals with 
      scheduled literals removed 
  *)
  let nextAvailable Vertex.{ available; _ } Edge.{ scheduled; _ } =
    List.filter available ~f:(fun (_, lit, _) ->
        not @@ Lit.Raw.Set.mem scheduled lit)
  ;;

  (** The next set of bound variables are  the current set of bound variables 
      and the union of the (non-wildcard) variables of the scheduled literals 
  *)
  let nextBound Vertex.{ bound; _ } Edge.{ scheduled; _ } =
    Var.Set.union_list
    @@ List.cons bound
    @@ List.map ~f:varset
    @@ Lit.Raw.Set.elements scheduled
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

  (* -- Selection procedure ------------------------------------------------- *)

  (** The cost of scheduling a literal relative to what is already bound is 
      the difference between each atomic obligation and the set of bound 
      variables.

      Note that wildcards are included in the cost of a literal but never in 
      the set of bound variables. Consequently wildcards will never be eliminated
      by scheduling other literals
  *)
  let cost_of Vertex.{ bound; _ } (_, _, obligation) =
    Obligation.min_of
    @@ `ListIn
         (List.map ~f:(fun ob -> Var.Set.diff ob bound)
         @@ Obligation.elements obligation)
  ;;

  (** A literal is _affordable_ if it has a cost (i.e. an element of its 
      obligation less the already bound variables) which is a subset of the 
      variable appearing in the head of the clause. 

      This ensures that we only schedule literals with an obligation that has
      already been paid _or_ could be paid by requiring a variable to be bound
      in the head of the clause.
    
  *)
  let is_affordable (idx, lit, costs) ~head_vars =
    let hv = Var.Set.of_list @@ List.map ~f:snd head_vars in
    List.filter_map ~f:(fun cost ->
        if Var.Set.is_subset cost ~of_:hv then Some (idx, lit, cost) else None)
    @@ Obligation.to_list costs
  ;;

  (** Wrapper around typing environment effect lookup *)
  let eff_of lit = M.get_pred_effects @@ Pred.name_of @@ Lit.Raw.pred_of lit

  let eff_intersect idx eff available =
    let rec aux xs =
      M.(
        match xs with
        | [] -> return false
        | (idx2, _, _) :: rest when idx2 >= idx -> aux rest
        | (_, lit, _) :: rest ->
          eff_of lit
          >>= fun eff2 ->
          if not @@ Eff.Set.is_empty @@ Eff.Set.inter eff eff2
          then return true
          else aux rest)
    in
    aux available
  ;;

  (** A literal is restricted from being scheduled by its effects if:
      1) It has any effect; *and*
      2) Scheduling the literal would change the relative order with respect to 
        other literals with and intersecting set of effects
  *)
  let restricted_effects Vertex.{ available; _ } (idx, lit, cost) =
    M.(
      eff_of lit
      >>= fun eff ->
      if Eff.Set.is_empty eff
      then return @@ Some (lit, cost)
      else
        eff_intersect idx eff available
        >>= fun intersects ->
        if intersects then return None else return @@ Some (lit, cost))
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
      available
      |> List.map ~f:(fun (idx, lit, oblig) ->
             idx, lit, cost_of vtx (idx, lit, oblig))
      |> List.partition_tf ~f:(fun (_, _, cost) -> Obligation.is_trivial cost)
    in
    let cands =
      match free with
      | [] -> List.concat_map costly ~f:(is_affordable ~head_vars)
      | _ -> List.map ~f:(fun (idx, lit, _) -> idx, lit, Var.Set.empty) free
    in
    M.(
      map ~f:(List.filter_map ~f:Fn.id)
      @@ all
      @@ List.map ~f:(restricted_effects vtx) cands)
  ;;

  (** Given the selected literals, group them by cost and construct edges *)
  let edges selected =
    let compare (_, vs1) (_, vs2) = Var.Set.compare vs1 vs2
    and break (_, vs1) (_, vs2) = Var.Set.(not @@ equal vs1 vs2) in
    List.sort selected ~compare
    |> List.group ~break
    |> List.map ~f:(fun xs ->
           let lits, costs = List.unzip xs in
           Edge.
             { scheduled = Lit.Raw.Set.of_list lits; cost = List.hd_exn costs })
  ;;

  (** Construct the minimum obligation graph of a clause given a set of predicate
      constriants 
  *)
  let of_clause cl =
    M.(
      let head_vars = vars @@ Clause.Raw.head_of cl in
      let nexts vtx =
        select ~head_vars vtx
        >>= fun selected ->
        let es = edges selected in
        return @@ ((vtx, es), List.map ~f:(mk_vertex vtx) es)
      in
      root cl
      >>= TreeM.unfoldM ~f:nexts
      >>= fun graph -> return Graph.{ head_vars; graph })
  ;;

  (* -- Inter-clausal analysis ------------------------------------------------ *)

  (** Determine the moding constraints of all clauses in a program with respect
      to an initial set (defaults to exposed queries) *)
  let solve ~deps queries =
    let rec aux preds =
      M.(
        match preds with
        | [] -> return ()
        | pred :: rest ->
          let pred_nm = Pred.name_of pred
          and clauses = Dependency.Raw.clauses_of deps pred in
          (* retrieve the current constraint and the clauses in which it appears *)
          get_pred_constraint pred_nm
          >>= fun cstr_in ->
          (* determine the constraint for each clause an use `meet` to find the
             constraint for the predicate, update the global map *)
          List.map clauses ~f:(fun cl -> map ~f:Graph.extract @@ of_clause cl)
          |> all
          >>= fun cstrs ->
          let cstr_out = Constraint.meet_list cstrs in
          (* If this predicate constraint is unchanged we don't need to update
             dependencies; if it is, retrieve dependencies which are related by
             a positive literal (since those related by a negative literal are
             already fully constrained) and add unique entries to the work list
          *)
          if Constraint.equal cstr_in cstr_out
          then aux rest
          else
            set_pred_constraint pred_nm cstr_out
            >>= fun _ ->
            let delta =
              List.filter ~f:(fun p -> not @@ Pred.equal pred p)
              @@ Dependency.Raw.pos_deps_of deps pred
            in
            let ws =
              List.dedup_and_sort ~compare:Pred.compare @@ rest @ delta
            in
            aux ws)
    in
    aux queries
  ;;
end
