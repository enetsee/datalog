open Core_kernel
open Reporting
open Lib

type t =
  | SClause of
      { head : Head.Term.t
      ; body : Body.t
      ; region : Region.t
      }
  | SQuery of
      { head : Head.Tmvar.t option
      ; body : Body.t
      ; region : Region.t
      }
  | SFact of Head.Symbol.t
  | SDecl of Decl.t

type repr =
  | RCls of Core.Clause.Raw.t
  | RQry of Core.Clause.Raw.t
  | RKnw of Core.Knowledge.t
  | RDcl

(* -- Helpers --------------------------------------------------------------- *)
let clause ?region head body =
  let region =
    Option.value
      region
      ~default:Region.(merge Head.Term.(region head) Body.(region body))
  in
  SClause { head; body; region }
;;

let query ?region body =
  let region = Option.value region ~default:Body.(region body) in
  SQuery { head = None; body; region }
;;

let fact head = SFact head
let decl d = SDecl d

(* -- Transformation -------------------------------------------------------- *)

let generate_query_head t =
  MonadCompile.(
    match t with
    | SQuery { head = Some _; region; _ } -> fail (Err.QueryNamed region)
    | SQuery { body; region; _ } ->
      fresh_querysym
      >>= fun predSym ->
      let vars =
        List.map ~f:Located.(locate ~region)
        @@ List.dedup_and_sort ~compare:Core.Tmvar.compare
        @@ Body.vars_of body
      in
      let atom = Atom.Tmvar.atom Located.(locate predSym ~region) vars in
      let hd = Head.Tmvar.atom @@ Located.locate atom ~region in
      return @@ SQuery { head = Some hd; body; region }
    | t -> return t)
;;

let set_nature t =
  MonadCompile.(
    match t with
    | SDecl _ -> return t
    | SFact head ->
      Head.Symbol.check_extralogical_clash head >>= fun _ -> return t
    | SClause { head; body; region } ->
      Head.Term.check_extralogical_clash head
      >>= fun _ ->
      Body.set_nature body >>= fun body -> return @@ clause ~region head body
    | SQuery { head = Some hd; body; region } ->
      Head.Tmvar.check_extralogical_clash hd
      >>= fun _ ->
      Body.set_nature body
      >>= fun body -> return @@ SQuery { head = Some hd; body; region }
    | SQuery { head; body; region } ->
      Body.set_nature body
      >>= fun body -> return @@ SQuery { head; body; region })
;;

let normalize = function
  | SClause ({ body; _ } as t) ->
    List.map ~f:(fun body -> SClause { t with body }) @@ Body.normalize body
  | t -> [ t ]
;;

(* -- HasCoreRepr implementation ------------------------------------------ *)

let query_to_core head_opt body region =
  MonadCompile.(
    match head_opt with
    | Some head ->
      map2
        Head.Tmvar.(to_core head)
        Body.(to_core body)
        ~f:(fun head body -> RQry Core.Clause.Raw.{ head; body; region })
    | _ -> fail Err.(QueryUnnamed region))
;;

let clause_to_core head body region =
  MonadCompile.map2
    Head.Term.(to_core head)
    Body.(to_core body)
    ~f:(fun head body -> RCls Core.Clause.Raw.{ head; body; region })
;;

let fact_to_core head =
  MonadCompile.map ~f:(fun k -> RKnw k) @@ Head.Symbol.to_core head
;;

let to_core = function
  | SClause { head; body; region } -> clause_to_core head body region
  | SQuery { head; body; region } -> query_to_core head body region
  | SFact head -> fact_to_core head
  | SDecl _ -> MonadCompile.return RDcl
;;

(* -- Pretty implementation ----------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf = function
    | SClause { head; body; _ } ->
      Fmt.(
        hovbox
        @@ suffix (any ".")
        @@ pair ~sep:(any " :-@; ") Head.Term.pp Body.pp)
        ppf
        (head, body)
    | SQuery { head = Some hd; body; _ } ->
      Fmt.(
        hovbox
        @@ suffix (any ".")
        @@ pair ~sep:(any " :-@; ") Head.Tmvar.pp Body.pp)
        ppf
        (hd, body)
    | SQuery { body; _ } ->
      Fmt.(hovbox @@ prefix (any "?-@; ") @@ suffix (any ".") @@ Body.pp)
        ppf
        body
    | SFact head -> Fmt.(hovbox @@ suffix (any ".") @@ Head.Symbol.pp) ppf head
    | SDecl d -> Decl.pp ppf d
  ;;

  let pp = `NoPrec pp
end)
