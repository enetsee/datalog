open Core_kernel
open Lib

module Make
    (Lit : Lit.S)
    (Clause : Clause.S with module Lit := Lit)
    (Program : Program.S with module Lit := Lit and module Clause := Clause) =
struct
  type t =
    { clauseFwd : int Clause.Map.t
    ; clauseBwd : Clause.t Int.Map.t
    ; predFwd : int Pred.Map.t
    ; predBwd : Pred.t Int.Map.t
          (* The list of predicates in the body of a clause *)
    ; clausePreds : Int.t list Int.Map.t
          (* The list of clauses comprising a predicate *)
    ; predClauses : Int.t list Int.Map.t
          (* The clauses in which the predicate appears as a subgoal along with its polarity*)
    ; predEdges : (Int.t * Polarity.t) list Int.Map.t
    }

  (* -- Construction ---------------------------------------------------------- *)

  let groupBy t ~proj ~cmp =
    let accu, cur_key, elems =
      List.fold_left
        ~f:(fun (accu, cur_key, elems) elem ->
          let key = proj elem in
          match cur_key with
          | None -> accu, Some key, [ elem ]
          | Some key' when cmp key key' = 0 -> accu, cur_key, elem :: elems
          | Some key' -> (key', elems) :: accu, Some key, [ elem ])
        ~init:([], None, [])
      @@ List.sort ~compare:(fun x y -> cmp (proj x) (proj y)) t
    in
    match cur_key with
    | Some key -> (key, elems) :: accu
    | _ -> accu
  ;;

  (** Map from predicate to all clauses in which it appears as the conclusion *)
  let predClauses ~predFwd Program.{ clauses; _ } =
    let ungrouped, clidxs =
      List.unzip
      @@ List.mapi clauses ~f:(fun idx (Clause.{ head; _ } as cl) ->
             (Pred.Map.find_exn predFwd (Lit.pred_of head), idx), (cl, idx))
    in
    let fwd = Clause.Map.of_alist_exn clidxs
    and bwd = Int.Map.of_alist_exn @@ List.map ~f:Tuple2.swap clidxs in
    let pcl =
      Int.Map.of_alist_exn
      @@ List.map ~f:(fun (p, pcls) -> p, List.map ~f:snd pcls)
      @@ groupBy ~proj:fst ~cmp:Int.compare
      @@ ungrouped
    in
    pcl, fwd, bwd
  ;;

  (* Terminal predicates will not be added by `predEdges` so we add them here *)
  let insertInits ~predFwd ~predBwd ~itnls edges =
    let rec aux unseen accu = function
      | [] ->
        accu
        @ List.map ~f:(fun p -> Pred.Map.find_exn predFwd p, [])
        @@ Pred.Set.to_list unseen
      | ((p, _) as next) :: rest ->
        let pred = Int.Map.find_exn predBwd p in
        aux Pred.Set.(remove unseen pred) (next :: accu) rest
    in
    aux itnls [] edges
  ;;

  (** For each intensional predicate, a list of predicates in which it appears as a literal
      along with the polarity with which it appears. Given a the clauses

      p(..) :- q(..), not r(..), s(..). 
      p(..) :- t(..). 
      q(..) :- r(..), s(..). 
      r(..) :- u(..), v(..).
      
      we get the following

      p -> {}
      q => { (Pos,p) }       
      r => { (Neg,p) , (Pos,q) } 
  *)
  let predEdges ~predFwd ~predBwd (Program.{ clauses; _ } as prog) =
    let itnls = Program.intensionals prog in
    Int.Map.of_alist_exn
    (* Ensure all intensional predicates are included *)
    @@ insertInits ~predFwd ~predBwd ~itnls
    (* Group by the source predicate *)
    @@ List.map ~f:(fun (p, pps) -> p, List.map ~f:snd pps)
    @@ groupBy ~proj:fst ~cmp:Int.compare
    @@ (* For each clause, add an edge from the intensional predicates
          in the body literals to the predicate in the head literal and label
          it with the polarity of the body literal *)
    List.concat_map clauses ~f:(fun Clause.{ head; body; _ } ->
        let dest = Pred.Map.find_exn predFwd (Lit.pred_of head) in
        List.filter_map
          ~f:(fun lit ->
            let pred, pol = Lit.(pred_of lit, pol_of lit) in
            if Pred.Set.mem itnls pred
            then Some (Pred.Map.find_exn predFwd pred, (dest, pol))
            else None)
          body)
  ;;

  let clausePreds ~predFwd Program.{ clauses; _ } =
    Int.Map.of_alist_exn
    @@ List.mapi clauses ~f:(fun cl_idx cl ->
           let prds =
             List.map ~f:(fun lit ->
                 Pred.Map.find_exn predFwd @@ Lit.pred_of lit)
             @@ Clause.body_of cl
           in
           cl_idx, prds)
  ;;

  let from_program prog =
    let pidxs = List.mapi ~f:(fun idx p -> p, idx) @@ Program.preds_of prog in
    let predFwd = Pred.Map.of_alist_exn pidxs
    and predBwd = Int.Map.of_alist_exn @@ List.map ~f:Tuple2.swap pidxs in
    let predEdges = predEdges ~predFwd ~predBwd prog
    and clausePreds = clausePreds ~predFwd prog
    and predClauses, clauseFwd, clauseBwd = predClauses ~predFwd prog in
    { clauseFwd
    ; clauseBwd
    ; clausePreds
    ; predFwd
    ; predBwd
    ; predClauses
    ; predEdges
    }
  ;;

  (* -- Query --------------------------------------------------------------- *)

  (** All clauses in which the provided predicate is the conclusion  *)
  let clauses_of_idx { predClauses; clauseBwd; _ } pred_idx =
    Option.(
      value
        ~default:[]
        (Int.Map.find predClauses pred_idx
        |> map ~f:List.(filter_map ~f:Int.Map.(find clauseBwd))))
  ;;

  (** All clauses in which the provided predicate is the conclusion  *)
  let clauses_of { predClauses; clauseBwd; predFwd; _ } pred =
    Option.(
      value
        ~default:[]
        (Pred.Map.find predFwd pred
        >>= Int.Map.find predClauses
        |> map ~f:List.(filter_map ~f:Int.Map.(find clauseBwd))))
  ;;

  (** All predicates which depend on the provided predicate subjec to a 
     predicate on the polarity of the edge *)
  let deps_helper ~p { predEdges; predFwd; predBwd; _ } pred =
    Option.(
      value
        ~default:[]
        (Pred.Map.find predFwd pred
        >>= Int.Map.find predEdges
        |> map
             ~f:
               List.(
                 filter_map ~f:(fun (idx, pol) ->
                     if p pol then Int.Map.find predBwd idx else None))))
  ;;

  (** All predicates which depend on the provided predicate through a positive
      edge *)
  let pos_deps_of = deps_helper ~p:Polarity.isPos

  let deps_of = deps_helper ~p:Fn.(const true)
  let neg_deps_of = deps_helper ~p:Polarity.isNeg

  (** Determine if a predicate exists and is used in at least one clause *)
  let is_used { predFwd; predEdges; _ } pred =
    Option.(
      value
        ~default:false
        (Pred.Map.find predFwd pred
        >>= Int.Map.find predEdges
        |> map ~f:Fn.(compose not List.is_empty)))
  ;;

  (** The `dead` clauses are those which are not accessible from any exposed
      query. 

            
  *)
  let live_clause_idxs
      { predFwd; predClauses; clausePreds; _ }
      Program.{ queries; _ }
    =
    let rec aux live_cls seen_prd = function
      | [] -> live_cls
      | prd_idx :: rest ->
        let cls =
          Option.(value ~default:[] @@ Int.Map.find predClauses prd_idx)
        in
        let prds =
          List.concat_map cls ~f:(fun idx ->
              Option.(value ~default:[] @@ Int.Map.find clausePreds idx))
        in
        let live_cls' = Int.Set.(union (of_list cls) live_cls) in
        let seen_pred' = Int.Set.add seen_prd prd_idx in
        let ws =
          List.filter ~f:(fun idx -> not @@ Int.Set.mem seen_prd idx)
          @@ prds
          @ rest
        in
        aux live_cls' seen_pred' ws
    in
    aux Int.Set.empty Int.Set.empty
    @@ List.filter_map ~f:(Pred.Map.find predFwd) queries
  ;;

  (* -- Stratification ------------------------------------------------------ *)
  include Graph_util.Scc.Make (Int)

  (* Check if any predicate involde in the loop is present with a negative edge *)
  let checkLoop { predEdges; predBwd; _ } preds =
    let ps = Int.Set.of_list preds in
    let negs =
      List.concat_map preds ~f:(fun src ->
          List.filter_map ~f:(fun (dest, pol) ->
              if Int.Set.mem ps dest && Polarity.isNeg pol
              then Some Int.Map.(find_exn predBwd src, find_exn predBwd dest)
              else None)
          @@ Int.Map.find_exn predEdges src)
    in
    match negs with
    | [] -> Ok preds
    | _ -> Error negs
  ;;

  let expand t = function
    | No_loop elem -> Ok (clauses_of_idx t elem)
    | Has_loop elems ->
      Result.map ~f:List.(concat_map ~f:(clauses_of_idx t)) @@ checkLoop t elems
  ;;

  let stratify ({ predEdges; _ } as t) =
    Result.(
      all
      @@ List.map ~f:(expand t)
      @@ Array.to_list
      @@ topological
      @@ Int.Map.map
           ~f:(fun dlbls -> Int.Set.of_list @@ List.map ~f:fst dlbls)
           predEdges)
  ;;

  (* -- Pretty implementation ----------------------------------------------- *)

  let pp_predFwd ppf predFwd =
    Fmt.(vbox @@ list ~sep:cut @@ pair ~sep:(always " => ") Pred.pp int) ppf
    @@ Pred.Map.to_alist predFwd
  ;;

  let pp_clauseFwd ppf clauseFwd =
    Fmt.(vbox @@ list ~sep:cut @@ pair ~sep:(always " => ") Clause.pp int) ppf
    @@ Clause.Map.to_alist clauseFwd
  ;;

  let pp_predClauses ppf predClauses =
    Fmt.(
      vbox
      @@ list ~sep:cut
      @@ pair ~sep:(always " => ") (prefix (any "pred ") int)
      @@ braces
      @@ list ~sep:comma (prefix (any "clause ") int))
      ppf
    @@ Int.Map.to_alist predClauses
  ;;

  let pp_clausePreds ppf clausePreds =
    Fmt.(
      vbox
      @@ list ~sep:cut
      @@ pair ~sep:(always " => ") (prefix (any "clause ") int)
      @@ braces
      @@ list ~sep:comma (prefix (any "pred ") int))
      ppf
    @@ Int.Map.to_alist clausePreds
  ;;

  let pp_predEdges ppf predEdges =
    Fmt.(
      vbox
      @@ list ~sep:cut
      @@ pair ~sep:(always " => ") (prefix (any "pred ") int)
      @@ braces
      @@ list ~sep:comma
      @@ parens
      @@ pair ~sep:comma (prefix (any "clause ") int) Polarity.pp_verbose)
      ppf
    @@ Int.Map.to_alist predEdges
  ;;

  let pp ppf { predFwd; clauseFwd; predClauses; predEdges; clausePreds; _ } =
    Fmt.(
      vbox
      @@ pair
           ~sep:cut
           (pair
              ~sep:cut
              (prefix (any "Clause indexes@.@.") @@ pp_clauseFwd)
              (prefix (any "@.Predicate indexes@.@.") @@ pp_predFwd))
           (pair
              ~sep:cut
              (pair
                 ~sep:cut
                 (prefix (any "@.Predicate clause components @.@.")
                 @@ pp_predClauses)
                 (prefix (any "@.Clause body predicates @.@.") @@ pp_clausePreds))
              (prefix (any "@.Predicate literal appearances @.@.")
              @@ pp_predEdges)))
      ppf
      ((clauseFwd, predFwd), ((predClauses, clausePreds), predEdges))
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end
