open Core_kernel
open Reporting
open Raw
open Lib

module Violation = struct
  type t =
    { dest : Dataflow.Dest.t
    ; tmvar : Tmvar.t
    ; region : Region.t [@compare.ignore]
    }
  [@@deriving compare, eq]

  let violation ?(region = Region.empty) dest tmvar = { dest; tmvar; region }

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { dest; tmvar; _ } =
      Fmt.(hbox @@ pair ~sep:sp Dataflow.Dest.pp (quote Tmvar.pp))
        ppf
        (dest, tmvar)
    ;;

    let pp = `NoPrec pp
  end)
end

type repair =
  | Unfixable of Violation.t
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
    Pred.Name.from_string sym
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

let mk_guard srcs (Violation.{ tmvar; _ } as violation) =
  let grdPred = Pred.logical ~arity:1 @@ fresh_pred_sym "guard" in
  let grdLit = Lit.lit grdPred Term.[ var' tmvar ] in
  Option.value_map ~default:(Unfixable violation) ~f:(fun gs ->
      let cls, fcts = partitionGuards gs in
      Guard (grdLit, cls, fcts))
  @@ Option.all
  @@ List.map srcs ~f:(guard_from_src grdLit grdPred tmvar)
;;

let fix_violation fg (Violation.{ dest; _ } as violation) =
  match Dataflow.coveringPositives fg ~dest with
  | Some srcs -> mk_guard srcs violation
  | None -> Unfixable violation
;;

let collect_repairs rps =
  let rec aux (lits, cls, fcts) = function
    | Guard (lit, cl, fct) :: rest ->
      aux (lit :: lits, cl :: cls, fct :: fcts) rest
    | [] -> Ok (lits, cls, fcts)
    | Unfixable violation :: rest -> aux_violation [ violation ] rest
  and aux_violation accu = function
    | Unfixable violation :: rest -> aux_violation (violation :: accu) rest
    | _ :: rest -> aux_violation accu rest
    | _ -> Error (List.rev accu)
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
      let dest = Dataflow.Dest.DPred (head_pred, idx) in
      Some Violation.(violation ~region dest v)
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
  @@ List.map ~f:(fix_violation fg)
  @@ violations cl
;;

let apply prog =
  let fg = Dataflow.from_prog prog in
  Result.map ~f:(fun rps ->
      let cls, gclss, kbs = List.unzip3 rps in
      let clauses = cls @ List.concat gclss in
      Program.{ prog with clauses }, Knowledge.Set.union_list kbs)
  @@ Result.all
  @@ List.map ~f:(fix_clause fg)
  @@ Raw.Program.clauses_of prog
;;
