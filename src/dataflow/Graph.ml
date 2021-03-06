open Core_kernel
open Core
open Lib

type t =
  { nodes : int list
  ; edges : (int * int) list
  ; fwdMap : int Node.Map.t
  ; bwdMap : Node.t Int.Map.t
  }

(* -- Graph construction ---------------------------------------------------- *)

let addBinder bindings ~var ~node =
  Core.Tmvar.Map.update
    bindings
    var
    ~f:(Option.value_map ~default:[ node ] ~f:List.(cons node))
;;

let getBinders bindings ~var =
  Option.value ~default:[ Node.NNull ] @@ Tmvar.Map.find bindings var
;;

let updateBinders bindings ~var ~nodes =
  Tmvar.Map.update
    bindings
    var
    ~f:(Option.value_map ~default:nodes ~f:Fn.(const nodes))
;;

(** accumulate bindings in the head literal of a clause *)
let clauseHead Lit.Raw.{ terms; pred; _ } =
  fst
  @@ List.fold_left terms ~init:(Tmvar.Map.empty, 0) ~f:(fun (accu, idx) ->
       function
       | Term.TVar (var, _) ->
         addBinder accu ~node:Node.(NPred (pred, idx)) ~var, idx + 1
       | _ -> accu, idx + 1)
;;

(** accumulate edges and bindings in a body literal of a clause *)
let clauseBody
    ?(bindings = Tmvar.Map.empty)
    (Lit.Raw.{ pol; pred; terms; _ } as lit)
    ~intensionals
  =
  fst
  @@ List.fold_left
       terms
       ~init:(([], bindings), 0)
       ~f:(fun ((edges, bindings), idx) term ->
         match term with
         | Term.TWild (nm_opt, _) ->
           if Pred.Set.mem intensionals pred
           then
             ( ( Node.(NConst (CWild nm_opt), NPred (pred, idx)) :: edges
               , bindings )
             , idx + 1 )
           else (edges, bindings), idx + 1
         | TSym (sym, _) ->
           if Pred.Set.mem intensionals pred
           then
             ( (Node.(NConst (CSym sym), NPred (pred, idx)) :: edges, bindings)
             , idx + 1 )
           else (edges, bindings), idx + 1
         | TParam (nm, _) ->
           if Pred.Set.mem intensionals pred
           then
             ( (Node.(NConst (CParam nm), NPred (pred, idx)) :: edges, bindings)
             , idx + 1 )
           else (edges, bindings), idx + 1
         | TVar (var, _) ->
           let litnode = Node.NLit (lit, idx)
           and srcs = getBinders ~var bindings in
           let dests =
             if Pred.Set.mem intensionals pred
             then Node.[ litnode; NPred (pred, idx) ]
             else [ litnode ]
           in
           let edges' =
             List.(srcs >>= fun src -> dests >>= fun dest -> return (src, dest))
           and bindings' =
             if Polarity.isPos pol
             then updateBinders bindings ~var ~nodes:[ litnode ]
             else bindings
           in
           (edges' @ edges, bindings'), idx + 1)
;;

let clauseEdges Clause.Raw.{ head; body; _ } ~intensionals =
  fst
  @@ List.fold_left
       body
       ~init:([], clauseHead head)
       ~f:(fun (edges, bindings) lit ->
         let edges', bindings' = clauseBody ~bindings lit ~intensionals in
         edges' @ edges, bindings')
;;

let queryEdges (Pred.{ arity; _ } as pred) =
  List.init arity ~f:(fun idx -> Node.(NNull, NPred (pred, idx)))
;;

let progEdges (Program.Raw.{ clauses } as prog) queries =
  let intensionals = Program.Raw.intensionals prog in
  List.concat_map ~f:queryEdges queries
  @ List.concat_map ~f:(clauseEdges ~intensionals) clauses
;;

let from_prog prog queries =
  let pedges =
    List.dedup_and_sort
      ~compare:(Tuple2.compare ~cmp1:Node.compare ~cmp2:Node.compare)
    @@ progEdges prog queries
  in
  let idx_nodes =
    List.mapi ~f:(fun idx n -> idx, n)
    @@ List.dedup_and_sort ~compare:Node.compare
    @@ List.map ~f:fst pedges
    @ List.map ~f:snd pedges
  in
  let bwdMap = Int.Map.of_alist_exn idx_nodes
  and fwdMap =
    Node.Map.of_alist_exn @@ List.map ~f:(fun (idx, n) -> n, idx) idx_nodes
  in
  let nodes = List.map ~f:fst idx_nodes
  and edges =
    List.map
      ~f:(fun (src, dest) ->
        Node.Map.(find_exn fwdMap src, find_exn fwdMap dest))
      pedges
  in
  { nodes; edges; fwdMap; bwdMap }
;;

(* -- Query ----------------------------------------------------------------- *)

(** Links to the Node, the Node itself, a label, links from the Node. *)
let context { edges; bwdMap; _ } (n : int) =
  let srcs =
    List.filter_map ~f:(fun (s, d) -> if d = n then Some s else None) edges
  and dests =
    List.filter_map ~f:(fun (s, d) -> if s = n then Some d else None) edges
  in
  Some (srcs, n, Int.Map.find_exn bwdMap n, dests)
;;

let coveringPositives t ~dest =
  let rec aux visited = function
    | node when Int.Set.mem visited node -> Some []
    | node ->
      Option.(
        context t node
        >>= fun (pre, _, lbl, _) ->
        (match lbl with
        | Node.NNull -> None
        | NConst const -> Some [ Src.SConst const ]
        | NLit (lit, idx) -> Some [ Src.SLit (lit, idx) ]
        | NPred _ ->
          Option.(
            let visited' = Int.Set.add visited node in
            List.map ~f:(aux visited') pre
            |> all
            >>= fun xss -> Some (List.concat xss))))
  in
  Option.(
    Node.Map.find t.fwdMap (Dest.to_node dest)
    >>= fun node ->
    context t node
    >>= fun (pre, _, _, _) ->
    List.map ~f:(aux Int.Set.empty) pre
    |> all
    >>= fun xss -> Some (List.concat xss))
;;

let isPredPredicate ({ fwdMap; bwdMap; _ } as t) lit idx =
  match Node.Map.find fwdMap (Node.NLit (lit, idx)) with
  | None -> false
  | Some node ->
    Option.value_map ~default:false ~f:(fun (pre, _, _, _) ->
        List.exists
          ~f:(fun n ->
            match Int.Map.find bwdMap n with
            | Some (Node.NPred _) -> true
            | _ -> false)
          pre)
    @@ context t node
;;

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { bwdMap; edges; _ } =
    Fmt.(
      vbox
      @@ pair
           ~sep:cut
           (vbox @@ list ~sep:cut @@ pair ~sep:(always " => ") int Node.pp)
           (vbox @@ list ~sep:cut @@ parens @@ pair ~sep:comma int int))
      ppf
      (Int.Map.to_alist bwdMap, edges)
  ;;

  let pp = `NoPrec pp
end)
