open Core_kernel
open Core

type repair =
  | Unfixable of Violation.t
  | Guard of Lit.Raw.t * Clause.Raw.t list * Knowledge.Base.t

module Make (M : RepairM.S) = struct
  let mk_guard srcs (Violation.{ tmvar; _ } as violation) =
    M.(
      fresh_guardsym
      >>= fun name ->
      let grdPred = Pred.pred ~arity:1 name in
      let grdLit = Lit.Raw.lit grdPred Term.[ var' tmvar ] in
      return
      @@ Option.value_map ~default:(Unfixable violation) ~f:(fun gs ->
             let cls, fcts = Guard.partition gs in
             Guard (grdLit, cls, fcts))
      @@ Option.all
      @@ List.map srcs ~f:(Guard.from_src grdLit grdPred tmvar))
  ;;

  let fix_violation fg (Violation.{ dest; _ } as violation) =
    match Graph.coveringPositives fg ~dest with
    | Some srcs -> mk_guard srcs violation
    | None -> M.return @@ Unfixable violation
  ;;

  let collect_repairs rps =
    let rec aux (lits, cls, fcts) = function
      | Guard (lit, cl, fct) :: rest ->
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

  let fix_program prog ~exports =
    let fg = Graph.from_prog prog exports in
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
