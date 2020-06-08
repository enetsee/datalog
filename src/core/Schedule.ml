open Core_kernel
open Lib
open Raw

(** The analysis gives us possible orderings of literals in a clause body 
      along with the cost of that ordering i.e. which term variables must be 
      bound in the clause head *)
type t =
  { head_vars : Tmvar.t list
  ; mog : (Pred.Set.t * Tmvar.Set.t) Tree.t
  }

include Pretty.Make0 (struct
  type nonrec t = t

  let pp_lbl ppf (preds, cost) =
    Fmt.(
      hbox
      @@ pair
           ~sep:sp
           (parens @@ list ~sep:comma Pred.pp)
           (braces @@ list ~sep:comma Tmvar.pp))
      ppf
      (Pred.Set.to_list preds, Tmvar.Set.to_list cost)
  ;;

  let pp ppf { head_vars; mog } =
    Fmt.(
      vbox
      @@ pair ~sep:cut (parens @@ list ~sep:comma Tmvar.pp) (Tree.pp pp_lbl))
      ppf
      (head_vars, mog)
  ;;

  let pp = `NoPrec pp
end)

module State = struct
  (** The state keeps track of:
      - which term variables have been paid for;
      - which are owed; and      
      - the remaining predicate/mode pairs along with their costs 
    *)
  type t =
    { owed : Tmvar.Set.t
    ; paid : Tmvar.Set.t
    ; alts : (Pred.t * Tmvar.Set.t) list
    }

  (** Given a list of terms, a set of unpaid variables and a predicate mode
      determine the 'cost' of the predicate i.e. the set of additional
      term variables that are _required_ and not yet bound *)
  let obligation terms mode ~owed =
    Tmvar.Set.of_list
    @@ List.filter_map ~f:(function
           | _, Mode.Opt | Term.TSym _, _ -> None
           | TVar (nm, _), _ when Tmvar.Set.mem owed nm -> Some nm
           | TVar _, _ -> None
           | TWild _, _ -> failwith "Not implemented")
    @@ List.zip_exn terms
    @@ ModeVector.to_list mode
  ;;

  (** For each _possible_ mode of the predicate, 
        determine the 'cost to pay' wrt already bound variables 
    
        If a literal is negated then we require all terms to be bound
    *)
  let lit_cost Lit.{ pred; terms; pol; _ } ~cnstrs ~owed =
    let mvs =
      match pol with
      | Pos ->
        Constraint.to_MVs ~arity:pred.arity
        @@ Option.value ~default:Constraint.trivial
        @@ Pred.Map.find cnstrs pred
      | Neg ->
        (* Require negative literals to be fully bound *)
        [ ModeVector.from_list @@ List.init pred.arity ~f:(fun _ -> Mode.Req) ]
    in
    List.map mvs ~f:(fun mode -> pred, obligation terms ~owed mode)
  ;;

  (** Helper to collect the set of unique term variables appearing in a list of 
      literals 
  *)
  let lit_vars lits = Tmvar.Set.of_list @@ List.concat_map ~f:Lit.vars_of lits

  (** Given an order list of subgoals, eliminate those subgoals which 
       cannot be reordered due to interaction of effects. *)
  let candidates lits =
    let rec aux accu eff = function
      | next :: rest ->
        let eff' = Lit.effects_of next in
        (* Do the effects of this predicate interfere with accumulated effects? *)
        if Eff.Set.(is_empty @@ inter eff eff')
        then aux (next :: accu) Eff.Set.(union eff eff') rest
        else aux accu eff rest
      | [] -> List.rev accu
    in
    aux [] Eff.Set.empty lits
  ;;

  (** Initialise the intra-clausal analysis state *)
  let init ?(bound = Tmvar.Set.empty) lits ~cnstrs =
    let owed = Tmvar.Set.(diff (lit_vars lits) bound) in
    { owed
    ; paid = bound
    ; alts = List.concat_map ~f:(lit_cost ~cnstrs ~owed) @@ candidates lits
    }
  ;;

  (** Update the state with the selected alternative i.e. a set of predicates and the
        terms variables that have been paid for 
    *)
  let update { owed; paid; _ } (preds, cost) lits ~cnstrs =
    (* Do this using partition *)
    let lits_bought, lits' =
      List.partition_tf
        ~f:(fun lit -> Pred.Set.mem preds @@ Lit.pred_of lit)
        lits
    in
    let bound = lit_vars lits_bought in
    let owed', paid' = Tmvar.Set.(diff owed bound, union paid cost) in
    ( { owed = owed'
      ; paid = paid'
      ; alts =
          List.concat_map ~f:(lit_cost ~owed:owed' ~cnstrs) @@ candidates lits'
      }
    , lits' )
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp_alt ppf (pred, vars) =
      Fmt.(
        hbox
        @@ pair ~sep:(always " : ") Pred.Name.pp
        @@ braces
        @@ list ~sep:comma Tmvar.pp)
        ppf
        (pred.Pred.name, Tmvar.Set.to_list vars)
    ;;

    let pp ppf { owed; paid; alts } =
      Fmt.(
        vbox
        @@ pair
             ~sep:cut
             (hbox @@ prefix (always "owed: ") @@ list ~sep:comma Tmvar.pp)
             (pair
                ~sep:cut
                (hbox @@ prefix (always "paid: ") @@ list ~sep:comma Tmvar.pp)
                (vbox @@ prefix (always "alts:@;") @@ list ~sep:cut pp_alt)))
        ppf
        (Tmvar.Set.to_list owed, (Tmvar.Set.to_list paid, alts))
    ;;

    let pp = `NoPrec pp
  end)
end

(** sort by variable size, increasing, group and take the alternatives
      with equal lowest costs _then_ group by the term variables in their costs
      - this is clumsy using on the standard library
  *)
let least_cost alts =
  (* Collect the alternatives with equal lowest cost *)
  let rec aux cost_opt accu = function
    | [] -> accu
    | ((_, vars) as next) :: rest ->
      let cost' = Tmvar.Set.length vars in
      (match cost_opt with
      | None -> aux (Some cost') [ next ] rest
      | Some cost ->
        if cost' < cost
        then aux (Some cost') [ next ] rest
        else if cost' = cost
        then aux cost_opt (next :: accu) rest
        else aux cost_opt accu rest)
  in
  (* Group alternatives by the variables of their costs *)
  let group_by_vars alts =
    match
      List.sort
        ~compare:(fun (_, vars1) (_, vars2) -> Tmvar.Set.compare vars1 vars2)
        alts
    with
    | (pred, vars) :: xs ->
      let accu, preds, vars =
        List.fold_left
          xs
          ~init:([], [ pred ], vars)
          ~f:(fun (accu, preds, cur_vars) (pred, vars) ->
            if Tmvar.Set.equal vars cur_vars
            then accu, pred :: preds, cur_vars
            else (
              let accu' = (Pred.Set.of_list preds, cur_vars) :: accu in
              accu', [ pred ], vars))
      in
      (Pred.Set.of_list preds, vars) :: accu
    | [] -> []
  in
  group_by_vars @@ aux None [] alts
;;

(** Generate the minimum obligation graph (tree) for a clause given (fixed) 
    predicate constraints *)
let min_obligation Clause.{ head; body; _ } ~cnstrs =
  let rec aux st lits chosen =
    let st', lits' = State.update st chosen lits ~cnstrs in
    let choices = least_cost st'.alts in
    Tree.branch (fst chosen, st'.paid) @@ List.map choices ~f:(aux st' lits')
  in
  let st = State.init body ~cnstrs in
  { head_vars = List.map ~f:Term.lower_var_exn @@ Lit.terms_of head
  ; mog =
      Tree.branch (Pred.Set.empty, Tmvar.Set.empty)
      @@ List.map ~f:(aux st body)
      @@ least_cost st.alts
  }
;;

(** Determine constraints for the predicate in the head of the clause
    based on the costs at terminal nodes  
*)
let clause_constraint { head_vars; mog } =
  let to_constraint vs =
    Constraint.from_MVs
      [ ModeVector.from_list
        @@ List.map head_vars ~f:(function
               | v when Tmvar.Set.mem vs v -> Mode.Req
               | _ -> Mode.Opt)
      ]
  in
  List.fold_left ~init:Constraint.ill ~f:(fun accu (_, vs) ->
      Constraint.join accu @@ to_constraint vs)
  @@ Tree.leaves mog
;;

(** Construct the minimal obligation graph for a clause and generate 
    the mode constraint
*)
let analyse cl ~cnstrs = clause_constraint @@ min_obligation cl ~cnstrs

let paths xs =
  let rec aux accu cost = function
    | [] -> List.rev accu, Option.value_exn cost
    | (preds, cost) :: rest -> aux (preds :: accu) (Some cost) rest
  in
  aux [] None xs
;;

let permutations t =
  let neq x y = not @@ Pred.equal x y in
  let rec aux = function
    | [] -> [ [] ]
    | xs ->
      List.concat_map xs ~f:(fun x ->
          List.map ~f:(fun xs -> x :: xs) @@ aux @@ List.filter ~f:(neq x) xs)
  in
  aux t
;;

let expand pss =
  List.fold_left
    ~init:[ [] ]
    pss
    ~f:(fun (paths : Pred.t list list) (pset : Pred.Set.t) ->
      let segments = permutations @@ Set.elements pset in
      List.(paths >>= fun path -> segments >>= fun seg -> return (path @ seg)))
;;

(** Extract all compatible predicate orderings from the schedule *)
let extract ?bpatt { head_vars; mog } =
  (* determine which variables are available given the binding pattern
     if no binding pattern is provided, assume all are bound
  *)
  let bound =
    match bpatt with
    | None -> Tmvar.Set.of_list head_vars
    | Some bp ->
      Tmvar.Set.of_list
      @@ List.filter_map ~f:(function
             | x, Adornment.Bound -> Some x
             | _ -> None)
      @@ List.zip_exn head_vars
      @@ BindingPatt.to_list bp
  in
  List.concat_map ~f:(fun xs ->
      let pss, cost = paths xs in
      if Tmvar.Set.is_subset cost ~of_:bound then expand pss else [])
  @@ Tree.flatten mog
;;
