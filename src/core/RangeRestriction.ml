open Core_kernel
open Reporting
open Raw

type repair =
  | Unfixable of Region.t
  | Guard of Lit.t * Clause.t list * Knowledge.Set.t

type guard =
  | GClause of Clause.t
  | GFact of Knowledge.t

let partitionGuards gs =
  let rec aux (cls, fcts) = function
    | [] -> List.rev cls, fcts
    | GClause cl :: rest -> aux (cl :: cls, fcts) rest
    | GFact fct :: rest -> aux (cls, Knowledge.Set.add fcts fct) rest
  in
  aux ([], Knowledge.Set.empty) gs
;;

let mk_guard_body lit var idx =
  let pred = Lit.pred_of lit in
  let terms =
    List.init pred.arity ~f:(fun i ->
        if i = idx then Term.var' var else Term.wild ())
  in
  [ Lit.lit pred terms ]
;;

(* TODO:  monadic approach *)
let fresh_pred_sym =
  let i = ref 0 in
  fun pfx ->
    let sym = pfx ^ string_of_int !i in
    i := !i + 1;
    PredSymbol.from_string sym
;;

let guard_from_src grdLit grdPred v src =
  Dataflow.(
    match src with
    | Src.SLit (lit, idx) ->
      Some (GClause Clause.(clause grdLit @@ mk_guard_body lit v idx))
    | SConst (Const.CSym sym) ->
      Some (GFact (Knowledge.knowledge grdPred [ sym ]))
    | SConst Const.CWild -> None)
;;

let mk_guard srcs (v, region) =
  let grdPred = Pred.logical (fresh_pred_sym "_guard") 1 in
  let grdLit = Lit.lit grdPred Term.[ var' v ] in
  Option.value_map ~default:(Unfixable region) ~f:(fun gs ->
      let cls, fcts = partitionGuards gs in
      Guard (grdLit, cls, fcts))
  @@ Option.all
  @@ List.map srcs ~f:(guard_from_src grdLit grdPred v)
;;

let fix_violation fg (dest, (var, region)) =
  match Dataflow.coveringPositives fg ~dest with
  | Some srcs -> mk_guard srcs (var, region)
  | None -> Unfixable region
;;

let collect_repairs rps =
  let rec aux (lits, cls, fcts) = function
    | Guard (lit, cl, fct) :: rest ->
      aux (lit :: lits, cl :: cls, fct :: fcts) rest
    | [] -> Ok (lits, cls, fcts)
    | Unfixable _ :: _ -> Error "Not fixable"
  in
  aux ([], [], []) rps
;;

(** The (potential) range-restriction violations of a clause are the 
    variables appearing in the head of the clause which do not appear in any 
    _positive_ literal in the body of the clause *)
let violations Clause.{ head; body; _ } =
  let head_pred = Lit.pred_of head
  and body_vars =
    Tmvar.Set.of_list
    @@ List.concat_map body ~f:(fun (Lit.{ pol; _ } as lit) ->
           if Polarity.isPos pol then Lit.vars_of lit else [])
  in
  List.filter_mapi (Lit.terms_of head) ~f:(fun idx ->
    function
    | Term.TVar (v, region) when Tmvar.Set.(not @@ mem body_vars v) ->
      Some ((head_pred, idx), (v, region))
    | _ -> None)
;;

(** Attempt to fix each violation in a clause by adding guards witnessing
    the unrestricted variable *)
let fix_clause fg cl =
  Result.map ~f:(fun (lits, clss, fcts) ->
      ( Clause.
          { cl with
            body =
              List.fold_right ~init:cl.body ~f:(fun x accu -> x :: accu) lits
          }
      , List.concat clss
      , Knowledge.Set.union_list fcts ))
  @@ collect_repairs
  @@ List.map ~f:(fun ((pred, idx), v) ->
         fix_violation fg (Dataflow.Dest.DPred (pred, idx), v))
  @@ violations cl
;;

let repair (Program.{ clauses; _ } as prog) =
  let fg = Dataflow.from_prog prog in
  Result.map ~f:(fun rps ->
      let cls, gclss, kbs = List.unzip3 rps in
      let clauses = List.rev cls @ List.concat gclss in
      Program.{ prog with clauses }, Knowledge.Set.union_list kbs)
  @@ Result.all
  @@ List.map ~f:(fix_clause fg) clauses
;;
