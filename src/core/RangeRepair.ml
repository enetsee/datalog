open Core_kernel

type repair =
  | Unfixable of Violation.t
  | Guard of Lit.Raw.t * Clause.Raw.t list * Knowledge.Base.t

type guard =
  | GClause of Clause.Raw.t
  | GFact of Knowledge.t

let partitionGuards gs =
  let rec aux (cls, fcts) = function
    | [] -> List.rev cls, fcts
    | GClause cl :: rest -> aux (cl :: cls, fcts) rest
    | GFact fct :: rest -> aux (cls, Knowledge.Base.add fcts fct) rest
  in
  aux ([], Knowledge.Base.empty) gs
;;

let mk_guard_body lit var idx =
  let pred = Lit.Raw.pred_of lit in
  let terms =
    List.init pred.arity ~f:(fun i ->
        if i = idx then Term.var' var else Term.wild ())
  in
  [ Lit.Raw.lit pred terms ]
;;

let guard_from_src grdLit grdPred v src =
  Dataflow.(
    match src with
    | Src.SLit (lit, idx) ->
      Some (GClause Clause.Raw.(clause grdLit @@ mk_guard_body lit v idx))
    | SConst (Const.CSym sym) ->
      Some (GFact (Knowledge.knowledge grdPred [ sym ]))
    | SConst Const.CWild -> None)
;;

let mk_guard srcs (Violation.{ tmvar; _ } as violation) =
  MonadCompile.(
    fresh_guardsym
    >>= fun name ->
    let grdPred = Pred.logical ~arity:1 name in
    let grdLit = Lit.Raw.lit grdPred Term.[ var' tmvar ] in
    return
    @@ Option.value_map ~default:(Unfixable violation) ~f:(fun gs ->
           let cls, fcts = partitionGuards gs in
           Guard (grdLit, cls, fcts))
    @@ Option.all
    @@ List.map srcs ~f:(guard_from_src grdLit grdPred tmvar))
;;

let fix_violation fg (Violation.{ dest; _ } as violation) =
  match Dataflow.coveringPositives fg ~dest with
  | Some srcs -> mk_guard srcs violation
  | None -> MonadCompile.return @@ Unfixable violation
;;

let collect_repairs rps =
  let rec aux (lits, cls, fcts) = function
    | Guard (lit, cl, fct) :: rest ->
      aux (lit :: lits, cl :: cls, fct :: fcts) rest
    | [] -> MonadCompile.return (lits, cls, fcts)
    | Unfixable violation :: rest -> aux_violation [ violation ] rest
  and aux_violation accu = function
    | Unfixable violation :: rest -> aux_violation (violation :: accu) rest
    | _ :: rest -> aux_violation accu rest
    | _ -> MonadCompile.(fail (Err.RangeViolations (List.rev accu)))
  in
  aux ([], [], []) rps
;;

(** The (potential) range-restriction violations of a clause are the 
    variables appearing in the head of the clause which do not appear in any 
    _positive_ literal in the body of the clause *)
let violations Clause.Raw.{ head; body; _ } =
  let head_pred = Lit.Raw.pred_of head
  and body_vars =
    Tmvar.Set.of_list
    @@ List.concat_map body ~f:(fun (Lit.Raw.{ pol; _ } as lit) ->
           if Polarity.isPos pol then Lit.Raw.vars_of lit else [])
  in
  List.filter_mapi (Lit.Raw.terms_of head) ~f:(fun idx ->
    function
    | Term.TVar (v, region) when Tmvar.Set.(not @@ mem body_vars v) ->
      let dest = Dataflow.Dest.DPred (head_pred, idx) in
      Some Violation.(violation ~region dest v)
    | _ -> None)
;;

(** Attempt to fix each violation in a clause by adding guards witnessing
    the unrestricted variable *)
let fix_clause fg cl =
  MonadCompile.(
    List.map ~f:(fix_violation fg) @@ violations cl
    |> all
    >>= collect_repairs
    >>= fun (lits, clss, fcts) ->
    return
      ( Clause.Raw.
          { cl with
            body =
              List.fold_right ~init:cl.body ~f:(fun x accu -> x :: accu) lits
          }
      , List.concat clss
      , Knowledge.Base.union_list fcts ))
;;

let apply prog =
  let fg = Dataflow.from_prog prog in
  MonadCompile.(
    map ~f:(fun rps ->
        let cls, gclss, kbs = List.unzip3 rps in
        let clauses = cls @ List.concat gclss in
        Program.Raw.{ prog with clauses }, Knowledge.Base.union_list kbs)
    @@ all
    @@ List.map ~f:(fix_clause fg)
    @@ Program.Raw.clauses_of prog)
;;
