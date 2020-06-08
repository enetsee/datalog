open Core_kernel
open Lib

(** Single step of the well-moding transform *)
let step ~cnstrs ~pred ~clauses =
  let cstr =
    List.fold_left clauses ~init:Constraint.trivial ~f:(fun accu cl ->
        Constraint.meet accu @@ Schedule.analyse ~cnstrs cl)
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
      and clauses = Raw.Dependency.clauses_of deps next in
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
          @ Raw.Dependency.pos_deps_of deps next
      in
      aux cnstrs' ws
  in
  aux cnstrs
;;

(** Determine the moding constraints of all clauses in a program with respect
    to an initial set (defaults to exposed queries) *)
let solve_constraints ?inits prog ~deps =
  Raw.(
    let ws = Option.value ~default:Program.(queries_of prog) inits in
    iterate ~deps ~cnstrs:Program.(constraints_of prog) ws)
;;

(* -- Generalized program adornment ----------------------------------------- *)

module WorkItem = struct
  module X = struct
    type t = Pred.t * BindingPatt.t [@@deriving compare, sexp]

    let pp ppf (p, bp) =
      Fmt.(hbox @@ pair Pred.pp @@ braces @@ BindingPatt.pp) ppf (p, bp)
    ;;

    let pp = `NoPrec pp
    let from_literal Adorned.Lit.{ pred; bpatt; _ } = pred, bpatt
    let from_clause Adorned.Clause.{ body; _ } = List.map ~f:from_literal body
  end

  include X
  include Pretty.Make0 (X)
  module Set = Set.Make (X)
end

let adorn_head_lit raw_lit ~bpatt =
  let vs =
    List.filter_map ~f:(function
        | Adornment.Bound, Term.TVar (x, _) -> Some x
        | _ -> None)
    @@ List.zip_exn BindingPatt.(to_list bpatt)
    @@ Raw.Lit.terms_of raw_lit
  in
  Adorned.Lit.from_raw raw_lit ~bpatt, Tmvar.Set.of_list vs
;;

(** Given a set of bound variables, adorn the literal and update the set of 
    bound variables after sideways information passing *)
let adorn_body_lit raw_lit ~bound =
  let ads, bound' =
    List.fold_left
      Raw.Lit.(terms_of raw_lit)
      ~init:([], bound)
      ~f:(fun (ads, bs) tm ->
        match tm with
        | Term.TSym _ -> Adornment.Bound :: ads, bs
        | TVar (v, _) when Tmvar.Set.mem bs v -> Adornment.Bound :: ads, bs
        | TVar (v, _) -> Adornment.Free :: ads, Tmvar.Set.add bs v
        | TWild _ -> Adornment.Free :: ads, bs)
  in
  let bpatt = BindingPatt.from_list @@ List.rev ads in
  Adorned.Lit.from_raw raw_lit ~bpatt, bound'
;;

(** Generalized clause adornment 
      Given the binding pattern for the head, and an optional ordering of literals,
      adorn the body of a clause 
  *)
let adorn_clause cl ~bpatt ~ord =
  Option.(
    ord bpatt cl
    >>= fun raw_clause ->
    let region = Raw.Clause.region_of raw_clause in
    let head, bound = adorn_head_lit ~bpatt @@ Raw.Clause.head_of raw_clause in
    let body =
      List.rev
      @@ fst
      @@ List.fold_left ~init:([], bound) ~f:(fun (ls, bound) lit ->
             let lit', bound' = adorn_body_lit lit ~bound in
             lit' :: ls, bound')
      @@ Raw.Clause.body_of raw_clause
    in
    Some Adorned.Clause.(clause ~region head body))
;;

(** Given an initial 'worklist' list of predicate/binding pattern pairs
    which have not yet been seen:
    - adorn all clauses in which the predicate is the conclusion (the 
      'predicates clauses'); and 
    - add _unseen_ predicate/binding pairs of each premise of the resulting 
      adorned clauses to the worklist
      
    continue until no clauses are generated
*)
let work ~deps ~ord ~seen inits =
  let adorn_clauses (pred, bpatt) =
    Option.all
    @@ List.map ~f:(adorn_clause ~ord ~bpatt)
    @@ Raw.Dependency.clauses_of deps pred
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
        - adorn the corresponding clause using a binding pattern where all 
          variables are free 
        - add the predicate of each premise to a work like along with its 
          binding pattern
        - accumulate resulting adorned clauses using `adorn_clauses`
  *)
let adorn_query ~deps ~ord ~seen qry_pred qry_clause =
  Option.(
    let bpatt =
      BindingPatt.from_list
      @@ List.init qry_pred.Pred.arity ~f:(fun _ -> Adornment.Free)
    in
    adorn_clause qry_clause ~ord ~bpatt
    >>= fun aqcl ->
    let items =
      List.filter
        ~f:(fun item -> not @@ WorkItem.Set.mem seen item)
        (WorkItem.from_clause aqcl)
    in
    work ~deps ~ord ~seen items
    >>= fun (acls, seen') -> Some (aqcl :: acls, seen'))
;;

(** Generalized program adornment *)
let adorn_program Raw.Program.{ cnstrts; queries; _ } ~deps ~ord =
  let rec aux (accu, seen) = function
    | [] -> Some Adorned.Program.{ clauses = accu; cnstrts; queries }
    | (qry_pred, qry_clause) :: rest ->
      (match adorn_query ~deps ~ord ~seen qry_pred qry_clause with
      | None -> None
      | Some (cls, seen') -> aux (cls @ accu, seen') rest)
  in
  aux ([], WorkItem.Set.empty)
  @@ List.concat_map queries ~f:(fun qry ->
         List.map ~f:(fun cl -> qry, cl) @@ Raw.Dependency.clauses_of deps qry)
;;

(** Construct a ordering function from a set of constraints *)
let ordering cnstrs bpatt cl =
  let reorder body prds =
    List.map prds ~f:(fun p ->
        List.find_exn ~f:(fun lit -> Pred.equal p @@ Raw.Lit.pred_of lit) body)
  in
  match Schedule.(extract ~bpatt @@ min_obligation ~cnstrs cl) with
  | prds :: _ -> Some { cl with body = reorder cl.body prds }
  | _ -> None
;;

(** Schedule a program accounting for extralogical or user defined predicate
    constraints then generate a global ordering an adorn the program 
    TODO: report ill-moding error    
*)
let apply prog =
  let deps = Raw.Dependency.from_program prog in
  let cnstrs = solve_constraints prog ~deps in
  let ord = ordering cnstrs in
  adorn_program prog ~deps ~ord
;;
