open Core_kernel
open Lib
open Raw

module Const = struct
  type t =
    | CWild
    | CSym of Symbol.t
  [@@deriving compare, eq, sexp]

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | CSym sym -> Symbol.pp ppf sym
      | CWild -> Fmt.char ppf '_'
    ;;

    let pp = `NoPrec pp
  end)
end

module Node = struct
  module Minimal = struct
    type t =
      | NNull
      | NConst of Const.t
      | NPred of Pred.t * int
      | NLit of Lit.t * int
    [@@deriving compare, eq, sexp]

    let pp ppf = function
      | NNull -> Fmt.string ppf "null"
      | NConst const -> Fmt.(prefix (always "constant: ") Const.pp) ppf const
      | NPred (pr, idx) ->
        Fmt.(hbox @@ prefix (always "pred: ") @@ pair ~sep:comma Pred.pp int)
          ppf
          (pr, idx)
      | NLit (lit, idx) ->
        Fmt.(hbox @@ prefix (always "lit: ") @@ pair ~sep:comma Lit.pp int)
          ppf
          (lit, idx)
    ;;

    let pp = `NoPrec pp
  end

  include Minimal
  include Pretty.Make0 (Minimal)
  module Map = Map.Make (Minimal)
end

type t =
  { nodes : int list
  ; edges : (int * int) list
  ; fwdMap : int Node.Map.t
  ; bwdMap : Node.t Int.Map.t
  }

(* -- Graph construction ---------------------------------------------------- *)

let addBinder bindings ~var ~node =
  Tmvar.Map.update
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
let clauseHead Lit.{ terms; pred; _ } =
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
    (Lit.{ pol; pred; terms; _ } as lit)
    ~intensionals
  =
  fst
  @@ List.fold_left
       terms
       ~init:(([], bindings), 0)
       ~f:(fun ((edges, bindings), idx) term ->
         match term with
         | Term.TWild _ ->
           if Pred.Set.mem intensionals pred
           then
             ( (Node.(NConst CWild, NPred (pred, idx)) :: edges, bindings)
             , idx + 1 )
           else (edges, bindings), idx + 1
         | TSym (sym, _) ->
           if Pred.Set.mem intensionals pred
           then
             ( (Node.(NConst (CSym sym), NPred (pred, idx)) :: edges, bindings)
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

let clauseEdges Clause.{ head; body; _ } ~intensionals =
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

let progEdges (Program.{ clauses; queries; _ } as prog) =
  let intensionals = Program.intensionals prog in
  List.concat_map ~f:queryEdges queries
  @ List.concat_map ~f:(clauseEdges ~intensionals) clauses
;;

let from_prog prog =
  let pedges =
    List.dedup_and_sort
      ~compare:(Tuple2.compare ~cmp1:Node.compare ~cmp2:Node.compare)
    @@ progEdges prog
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

module Src = struct
  type t =
    | SLit of Lit.t * int
    | SConst of Const.t
  [@@deriving compare, eq]

  let to_node = function
    | SLit (lit, idx) -> Node.NLit (lit, idx)
    | SConst const -> Node.NConst const
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | SLit (lit, idx) ->
        Fmt.(
          hbox
          @@ prefix (always "literal ")
          @@ pair ~sep:(always "@") Lit.pp int)
          ppf
          (lit, idx)
      | SConst c -> Fmt.(prefix (always "constant ") Const.pp) ppf c
    ;;

    let pp = `NoPrec pp
  end)
end

module Dest = struct
  type t =
    | DLit of Lit.t * int
    | DPred of Pred.t * int
  [@@deriving compare, eq]

  let to_node = function
    | DLit (lit, idx) -> Node.NLit (lit, idx)
    | DPred (pred, idx) -> Node.NPred (pred, idx)
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | DLit (lit, idx) ->
        Fmt.(
          hbox
          @@ prefix (always "literal ")
          @@ pair ~sep:(always "@") Lit.pp int)
          ppf
          (lit, idx)
      | DPred (pred, idx) ->
        Fmt.(
          hbox
          @@ prefix (always "literal ")
          @@ pair ~sep:(always "@") Pred.pp int)
          ppf
          (pred, idx)
    ;;

    let pp = `NoPrec pp
  end)
end

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
