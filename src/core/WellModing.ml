open Core_kernel
open Raw

(** Single step of the well-moding transform *)
let step ~cnstrs ~pred ~clauses =
  let cstr =
    List.fold_left clauses ~init:Constraint.trivial ~f:(fun accu cl ->
        Constraint.join accu @@ Schedule.analyse ~cnstrs cl)
  in
  cstr, Pred.Map.update cnstrs pred ~f:Fn.(const cstr)
;;

let iterate ~deps ~cnstrs =
  let rec aux cnstrs = function
    | [] -> cnstrs
    | next :: rest ->
      (* retrieve the current constraint and the clauses in which it appears *)
      let cstr_in =
        Option.value ~default:Constraint.trivial @@ Pred.Map.find cnstrs next
      and clauses = Dependency.clauses_of deps next in
      let cstr_out, cnstrs' = step ~cnstrs ~pred:next ~clauses in
      (* If this predicate constraint is unchanged we don't need to update
         dependencies; if it is, retrieve dependencies which are related by
         a positive literal (since those related by a negative literal are
         already fully constrained) and add unique entries to the work list
      *)
      let ws =
        if Constraint.equal cstr_in cstr_out
        then rest
        else
          List.dedup_and_sort ~compare:Pred.compare
          @@ rest
          @ Dependency.pos_deps_of deps next
      in
      aux cnstrs' ws
  in
  aux cnstrs
;;

(** Determine the moding constraints of all clauses in a program with respect
    to an initial set *)
let analyse prog ~deps ~inits =
  iterate ~deps ~cnstrs:Program.(constraints_of prog) inits
;;

let reorder body prds =
  List.map prds ~f:(fun p ->
      List.find_exn ~f:(fun lit -> Pred.equal p @@ Lit.pred_of lit) body)
;;

(** Construct a ordering function from a set of constraints *)
let ordering cnstrs bpatt cl =
  match Schedule.(extract ~bpatt @@ min_obligation ~cnstrs cl) with
  | prds :: _ -> Some Clause.{ cl with body = reorder cl.body prds }
  | _ -> None
;;

(* let apply (Program.{ queries; _ } as prog) =
  Program.Adorned.adorn prog ~ord:(ordering @@ InterClausal.analyse prog queries) *)
