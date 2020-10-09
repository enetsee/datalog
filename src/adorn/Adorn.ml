open Core_kernel
open Core
open Schedule

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
        | Term.TSym _ | TParam _ -> Binding.Bound :: ads, bs
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

module Make (M : AdornM.S) = struct
  module SM = Scheduler.Make (M)

  (** wrapper around global constraint map *)
  let ordering bpatt cl =
    M.(
      SM.of_clause cl
      >>= fun sched ->
      match Graph.(orderings ~bpatt @@ sched) with
      | body :: _ -> return @@ Some { cl with body }
      | _ -> return None)
  ;;

  (** Generalized clause adornment; given the binding pattern for the head:

      - determine an ordering consistent with that pattern, if any
      - adorn the head with the pattern and the set of bound variables in the head
      - adorn each literal in the reordered body from left to rightm accumulating 
        and variables bound by that literal
    *)
  let adorn_clause cl ~bpatt =
    M.(
      let region = Clause.Raw.region_of cl in
      ordering bpatt cl
      >>= function
      | Some raw_clause ->
        let head, bound =
          adorn_head_lit ~bpatt @@ Clause.Raw.head_of raw_clause
        in
        let body = adorn_body ~bound @@ Clause.Raw.body_of raw_clause in
        return Clause.Adorned.(clause ~region head body)
      | _ -> err_no_ordering bpatt region)
  ;;

  (* -- Program adornment ----------------------------------------------------- *)

  let adorn_program_helper ~deps worklist =
    let rec aux accu seen worklist =
      M.(
        match worklist with
        | (pred, bpatt) :: rest ->
          let seen' = WorkItem.Set.add seen (pred, bpatt) in
          let clss = Dependency.Raw.clauses_of deps pred in
          List.map clss ~f:(adorn_clause ~bpatt)
          |> all
          >>= fun aclss ->
          let delta =
            List.concat_map aclss ~f:(fun acls ->
                List.filter ~f:(fun item -> not @@ WorkItem.Set.mem seen' item)
                @@ WorkItem.from_clause acls)
          in
          let ws =
            List.dedup_and_sort ~compare:WorkItem.compare (delta @ rest)
          in
          aux (aclss @ accu) seen' ws
        | _ -> return accu)
    in
    aux [] WorkItem.Set.empty worklist
  ;;

  let adorn_program prog ~exports =
    M.(
      let deps = Dependency.Raw.from_program prog in
      SM.solve ~deps exports
      >>= fun _ ->
      List.map exports ~f:(fun pred -> pred, Binding.mk_free pred)
      |> adorn_program_helper ~deps
      |> map ~f:Program.Adorned.program)
  ;;
end
