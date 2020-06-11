open Core_kernel
open Lib

(* -- Clause adornment ------------------------------------------------------ *)

(** Add a binding pattern to a head literal and return the variables that are 
    bound under that pattern 
*)
let adorn_head_lit raw_lit ~bpatt =
  let vs =
    List.filter_map ~f:(function
        | Binding.Bound, Term.TVar (x, _) -> Some x
        | _ -> None)
    @@ List.zip_exn Binding.(to_list bpatt)
    @@ Lit.Raw.terms_of raw_lit
  in
  Lit.Adorned.from_raw raw_lit ~bpatt, Tmvar.Set.of_list vs
;;

(** Given a set of bound variables, adorn the literal and update the set of 
    bound variables avaiable for literals appearing later in the body 
    throught sideways information passing 
*)
let adorn_body_lit raw_lit ~bound =
  (* determine the adornment for the literal given what is already bound and the
     variables which are bound when evaluating the literal *)
  let ads, bound' =
    List.fold_left
      Lit.Raw.(terms_of raw_lit)
      ~init:([], bound)
      ~f:(fun (ads, bs) tm ->
        match tm with
        | Term.TSym _ -> Binding.Bound :: ads, bs
        | TVar (v, _) when Tmvar.Set.mem bs v -> Bound :: ads, bs
        | TVar (v, _) -> Free :: ads, Tmvar.Set.add bs v
        | TWild _ -> Free :: ads, bs)
  in
  let bpatt = Binding.from_list @@ List.rev ads in
  Lit.Adorned.from_raw raw_lit ~bpatt, bound'
;;

let adorn_body body ~bound =
  List.rev
  @@ fst
  @@ List.fold_left body ~init:([], bound) ~f:(fun (ls, bound) lit ->
         let lit', bound' = adorn_body_lit lit ~bound in
         lit' :: ls, bound')
;;

(** Generalized clause adornment; given the binding pattern for the head:

    - determine an ordering consistent with that pattern, if any
    - adorn the head with the pattern and the set of bound variables in the head
    - adorn each literal in the reordered body from left to rightm accumulating 
      and variables bound by that literal
  *)
let adorn_clause cl ~bpatt ~ord =
  let region = Clause.Raw.region_of cl in
  match ord bpatt cl with
  | Some raw_clause ->
    let head, bound = adorn_head_lit ~bpatt @@ Clause.Raw.head_of raw_clause in
    let body = adorn_body ~bound @@ Clause.Raw.body_of raw_clause in
    Ok Clause.Adorned.(clause ~region head body)
  | _ -> Error (bpatt, region)
;;

(* -- Program adornment ----------------------------------------------------- *)

module WorkItem = struct
  module X = struct
    (** A workitem represents a unique predicate / binding pattern pair for whic
        we must generate adorned clauses *)
    type t = Pred.t * Binding.t [@@deriving compare, sexp]

    let pp ppf (p, bp) =
      Fmt.(hbox @@ pair Pred.pp @@ braces @@ Binding.pp) ppf (p, bp)
    ;;

    let pp = `NoPrec pp
    let from_literal Lit.Adorned.{ pred; bpatt; _ } = pred, bpatt

    let from_clause Clause.Adorned.{ body; _ } =
      List.dedup_and_sort ~compare @@ List.map ~f:from_literal body
    ;;
  end

  include X
  include Pretty.Make0 (X)
  module Set = Set.Make (X)
end

(** wrapper around global constraint map *)
let ordering cstrs bpatt cl =
  match Schedule.(orderings ~bpatt @@ of_clause ~cstrs cl) with
  | body :: _ -> Some { cl with body }
  | _ -> None
;;

let apply prog =
  let deps = Dependency.Raw.from_program prog in
  let cstrs = Schedule.solve prog ~deps in
  let ord = ordering cstrs in
  let queries = Program.Raw.queries_of prog in
  let rec aux accu seen = function
    | (pred, bpatt) :: rest ->
      let seen' = WorkItem.Set.add seen (pred, bpatt) in
      let cls =
        List.map ~f:(adorn_clause ~bpatt ~ord)
        @@ Dependency.Raw.clauses_of deps pred
      in
      let delta =
        List.concat_map cls ~f:(function
            | Ok acls ->
              List.filter ~f:(fun item -> not @@ WorkItem.Set.mem seen' item)
              @@ WorkItem.from_clause acls
            | _ -> [])
      in
      let ws = List.dedup_and_sort ~compare:WorkItem.compare (delta @ rest) in
      aux (cls @ accu) seen' ws
    | _ -> accu
  in
  let clauses, errs =
    List.partition_result
    @@ aux [] WorkItem.Set.empty
    @@ List.map queries ~f:(fun pred -> pred, Binding.mk_free pred)
  in
  match errs with
  | [] -> Ok Program.Adorned.(program ~cstrs clauses queries)
  | _ -> Error errs
;;
