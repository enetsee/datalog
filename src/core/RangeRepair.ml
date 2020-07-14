open Core_kernel

module Repair = struct
  type t =
    | Unfixable of Violation.t
    | Guard of Lit.Raw.t * Clause.Raw.t list * Knowledge.Base.t
end

module Guard = struct
  type t =
    | GClause of Clause.Raw.t
    | GFact of Knowledge.t

  let partition gs =
    let rec aux (cls, fcts) = function
      | [] -> List.rev cls, fcts
      | GClause cl :: rest -> aux (cl :: cls, fcts) rest
      | GFact fct :: rest -> aux (cls, Knowledge.Base.add fcts fct) rest
    in
    aux ([], Knowledge.Base.empty) gs
  ;;

  let mk_body lit var idx =
    let pred = Lit.Raw.pred_of lit in
    let terms =
      List.init pred.arity ~f:(fun i ->
          if i = idx then Term.var' var else Term.wild ())
    in
    [ Lit.Raw.lit pred terms ]
  ;;

  let from_src grdLit grdPred v src =
    Dataflow.(
      match src with
      | Src.SLit (lit, idx) ->
        Some (GClause Clause.Raw.(clause grdLit @@ mk_body lit v idx))
      | SConst (Const.CSym sym) ->
        Some (GFact (Knowledge.knowledge grdPred [ sym ]))
      | SConst Const.CWild -> None)
  ;;
end

module type MonadRepair = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val fresh_guardsym : Name.t t
  val err_range_violations : Violation.t list -> _ t
end

module Make (M : MonadRepair) = struct
  let mk_guard srcs (Violation.{ tmvar; _ } as violation) =
    M.(
      fresh_guardsym
      >>= fun name ->
      let grdPred = Pred.pred ~arity:1 name in
      let grdLit = Lit.Raw.lit grdPred Term.[ var' tmvar ] in
      return
      @@ Option.value_map ~default:(Repair.Unfixable violation) ~f:(fun gs ->
             let cls, fcts = Guard.partition gs in
             Repair.Guard (grdLit, cls, fcts))
      @@ Option.all
      @@ List.map srcs ~f:(Guard.from_src grdLit grdPred tmvar))
  ;;

  let fix_violation fg (Violation.{ dest; _ } as violation) =
    match Dataflow.coveringPositives fg ~dest with
    | Some srcs -> mk_guard srcs violation
    | None -> M.return @@ Repair.Unfixable violation
  ;;

  let collect_repairs rps =
    let rec aux (lits, cls, fcts) = function
      | Repair.Guard (lit, cl, fct) :: rest ->
        aux (lit :: lits, cl :: cls, fct :: fcts) rest
      | [] -> M.return (lits, cls, fcts)
      | Unfixable violation :: rest -> aux_violation [ violation ] rest
    and aux_violation accu = function
      | Unfixable violation :: rest -> aux_violation (violation :: accu) rest
      | _ :: rest -> aux_violation accu rest
      | _ -> M.err_range_violations @@ List.rev accu
    in
    aux ([], [], []) rps
  ;;

  (** Attempt to fix each violation in a clause by adding guards witnessing
    the unrestricted variable *)
  let fix_clause fg cl =
    M.(
      List.map ~f:(fix_violation fg) @@ Violation.of_clause cl
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

  let fix_program prog queries =
    let fg = Dataflow.from_prog prog queries in
    M.(
      map ~f:(fun rps ->
          let cls, gclss, kbs = List.unzip3 rps in
          let clauses = cls @ List.concat gclss in
          Program.Raw.program clauses, Knowledge.Base.union_list kbs)
      @@ all
      @@ List.map ~f:(fix_clause fg)
      @@ Program.Raw.clauses_of prog)
  ;;
end
