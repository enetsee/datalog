open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    | SClause of Clause.t Located.t
    | SFact of Fact.t Located.t
    | SQuery of Query.t Located.t
  [@@deriving eq, compare]

  let pp ppf = function
    | SClause c -> (Located.pp Clause.pp) ppf c
    | SFact c -> (Located.pp Fact.pp) ppf c
    | SQuery c -> (Located.pp Query.pp) ppf c
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Constructor helpers --------------------------------------------------- *)

let clause elem ~region = SClause { elem; region }
let fact elem ~region = SFact { elem; region }
let query elem ~region = SQuery { elem; region }

(* -- Denstructor helpers --------------------------------------------------- *)

let lower_clause = function
  | SClause c -> Some c
  | _ -> None
;;

let lower_query = function
  | SQuery q -> Some q
  | _ -> None
;;

let lower_fact = function
  | SFact f -> Some f
  | _ -> None
;;

(* -- Query ----------------------------------------------------------------- *)

let atoms = function
  | SClause { elem; _ } -> Clause.atoms elem
  | SQuery { elem; _ } -> Query.atoms elem
  | SFact { elem; _ } -> Fact.atoms elem
;;

let tmvars = function
  | SClause { elem; _ } -> Clause.tmvars elem
  | SQuery { elem; _ } -> Query.tmvars elem
  | SFact { elem; _ } -> Fact.tmvars elem
;;

(* == Transformations ======================================================= *)

(* -- Transformation helpers ------------------------------------------------ *)

let transform_atom t ~f =
  match t with
  | SClause { elem = { head; body }; region } ->
    let head = Subgoal.transform_atom ~f head
    and body = Subgoal.transform_atom ~f body in
    clause Clause.{ head; body } ~region
  | SQuery { elem = { head; body }; region } ->
    let body = Subgoal.transform_atom ~f body in
    query Query.{ head; body } ~region
  | SFact { elem = { head }; region } ->
    let head = Subgoal.transform_atom ~f head in
    fact Fact.{ head } ~region
;;

let transform_body t ~f =
  match t with
  | SClause { elem; region } -> clause ~region { elem with body = f elem.body }
  | SQuery { elem; region } -> query ~region { elem with body = f elem.body }
  | SFact _ -> t
;;

let transform_head t ~f =
  match t with
  | SClause { elem; region } -> clause ~region { elem with head = f elem.head }
  | SFact { elem; region } -> fact ~region { elem with head = f elem.head }
  | SQuery _ -> t
;;

let transform_clause t ~f =
  match t with
  | SClause { elem; region } -> clause ~region @@ f elem
  | _ -> t
;;

let transform_query t ~f =
  match t with
  | SQuery { elem; region } -> query ~region @@ f elem
  | _ -> t
;;

let transform_fact t ~f =
  match t with
  | SFact { elem; region } -> fact ~region @@ f elem
  | _ -> t
;;

module Effect (M : sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t
end) =
struct
  module S = Subgoal.Effect (M)

  let transform_atom t ~f =
    match t with
    | SClause { elem = { head; body }; region } ->
      M.map2
        (S.transform_atom ~f head)
        (S.transform_atom ~f body)
        ~f:(fun head body -> clause Clause.{ head; body } ~region)
    | SQuery { elem = { head; body }; region } ->
      M.map (S.transform_atom ~f body) ~f:(fun body ->
          query Query.{ head; body } ~region)
    | SFact { elem = { head }; region } ->
      M.map (S.transform_atom ~f head) ~f:(fun head ->
          fact Fact.{ head } ~region)
  ;;

  let transform_clause t ~f =
    match t with
    | SClause { elem; region } -> M.map ~f:(clause ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_query t ~f =
    match t with
    | SQuery { elem; region } -> M.map ~f:(query ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_fact t ~f =
    match t with
    | SFact { elem; region } -> M.map ~f:(fact ~region) @@ f elem
    | _ -> M.return t
  ;;

  (** Apply effectul subgoal transformation to the body of a sentence *)
  let transform_body t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun body -> clause { elem with body } ~region) @@ f elem.body
      | SQuery { elem; region } ->
        map ~f:(fun body -> query { elem with body } ~region) @@ f elem.body
      | _ -> return t)
  ;;

  (** Apply effectul subgoal transformation to the head of a sentence *)
  let transform_head t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun head -> clause { elem with head } ~region) @@ f elem.head
      | SFact { elem; region } ->
        map ~f:(fun head -> fact { elem with head } ~region) @@ f elem.head
      | _ -> return t)
  ;;
end

module Effect2 (M : sig
  include Monad.S2
  include Applicative.S2 with type ('a, 'b) t := ('a, 'b) t
end) =
struct
  module S = Subgoal.Effect2 (M)

  let transform_atom t ~f =
    match t with
    | SClause { elem = { head; body }; region } ->
      M.map2
        (S.transform_atom ~f head)
        (S.transform_atom ~f body)
        ~f:(fun head body -> clause Clause.{ head; body } ~region)
    | SQuery { elem = { head; body }; region } ->
      M.map (S.transform_atom ~f body) ~f:(fun body ->
          query Query.{ head; body } ~region)
    | SFact { elem = { head }; region } ->
      M.map (S.transform_atom ~f head) ~f:(fun head ->
          fact Fact.{ head } ~region)
  ;;

  let transform_clause t ~f =
    match t with
    | SClause { elem; region } -> M.map ~f:(clause ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_query t ~f =
    match t with
    | SQuery { elem; region } -> M.map ~f:(query ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_fact t ~f =
    match t with
    | SFact { elem; region } -> M.map ~f:(fact ~region) @@ f elem
    | _ -> M.return t
  ;;

  (** Apply effectul subgoal transformation to the body of a sentence *)
  let transform_body t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun body -> clause { elem with body } ~region) @@ f elem.body
      | SQuery { elem; region } ->
        map ~f:(fun body -> query { elem with body } ~region) @@ f elem.body
      | _ -> return t)
  ;;

  (** Apply effectul subgoal transformation to the head of a sentence *)
  let transform_head t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun head -> clause { elem with head } ~region) @@ f elem.head
      | SFact { elem; region } ->
        map ~f:(fun head -> fact { elem with head } ~region) @@ f elem.head
      | _ -> return t)
  ;;
end

module Effect3 (M : sig
  include Monad.S3
  include Applicative.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end) =
struct
  module S = Subgoal.Effect3 (M)

  let transform_atom t ~f =
    match t with
    | SClause { elem = { head; body }; region } ->
      M.map2
        (S.transform_atom ~f head)
        (S.transform_atom ~f body)
        ~f:(fun head body -> clause Clause.{ head; body } ~region)
    | SQuery { elem = { head; body }; region } ->
      M.map (S.transform_atom ~f body) ~f:(fun body ->
          query Query.{ head; body } ~region)
    | SFact { elem = { head }; region } ->
      M.map (S.transform_atom ~f head) ~f:(fun head ->
          fact Fact.{ head } ~region)
  ;;

  let transform_clause t ~f =
    match t with
    | SClause { elem; region } -> M.map ~f:(clause ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_query t ~f =
    match t with
    | SQuery { elem; region } -> M.map ~f:(query ~region) @@ f elem
    | _ -> M.return t
  ;;

  let transform_fact t ~f =
    match t with
    | SFact { elem; region } -> M.map ~f:(fact ~region) @@ f elem
    | _ -> M.return t
  ;;

  (** Apply effectul subgoal transformation to the body of a sentence *)
  let transform_body t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun body -> clause { elem with body } ~region) @@ f elem.body
      | SQuery { elem; region } ->
        map ~f:(fun body -> query { elem with body } ~region) @@ f elem.body
      | _ -> return t)
  ;;

  (** Apply effectul subgoal transformation to the head of a sentence *)
  let transform_head t ~f =
    M.(
      match t with
      | SClause { elem; region } ->
        map ~f:(fun head -> clause { elem with head } ~region) @@ f elem.head
      | SFact { elem; region } ->
        map ~f:(fun head -> fact { elem with head } ~region) @@ f elem.head
      | _ -> return t)
  ;;
end

(* -- Normalization transforms ---------------------------------------------- *)
module Logged = Effect3 (Logger)

let set_foreign_func t ~ffns =
  Logger.(
    match t with
    | SFact { elem = { head }; region } ->
      Subgoal.Logged.transform_atom ~f:Atom.(ffn_clash ~ffns) head
      >>= fun head -> return @@ fact Fact.{ head } ~region
    | SClause { elem = { head; body }; region } ->
      Subgoal.Logged.transform_atom ~f:Atom.(ffn_clash ~ffns) head
      >>= fun head ->
      let body = Subgoal.transform_atom ~f:Atom.(set_foreign_func ~ffns) body in
      return @@ clause Clause.{ head; body } ~region
    | SQuery { elem = { head = Some head; body }; region } ->
      Subgoal.Logged.transform_atom ~f:Atom.(ffn_clash ~ffns) head
      >>= fun head ->
      let body = Subgoal.transform_atom ~f:Atom.(set_foreign_func ~ffns) body in
      return @@ query Query.{ head = Some head; body } ~region
    | SQuery { elem = { head; body }; region } ->
      let body = Subgoal.transform_atom ~f:Atom.(set_foreign_func ~ffns) body in
      return @@ query Query.{ head; body } ~region)
;;

let split_disj t =
  Logger.(
    match t with
    | SFact _ -> return [ t ]
    | SClause { elem = { head; body }; region } ->
      return
      @@ List.map ~f:(fun body -> clause Clause.{ head; body } ~region)
      @@ Subgoal.split_disj body
    | SQuery { elem = { head = Some _ as head; body }; region } ->
      return
      @@ List.map ~f:(fun body -> query Query.{ head; body } ~region)
      @@ Subgoal.split_disj body
    | SQuery { region; _ } -> fail @@ query_not_named region)
;;

let name_query t =
  Logger.(
    match t with
    | SQuery { elem = { head; body }; region } ->
      (match head with
      | Some _ -> fail @@ query_already_named region
      | _ ->
        fresh ()
        >>= fun nm ->
        let predSym =
          Located.locate ~region:Region.empty @@ Core.PredSymbol.from_string nm
        in
        let terms =
          List.map ~f:Located.(locate ~region)
          @@ Subgoal.tmvars Core.Term.tmvars body
        in
        let atom = Atom.atom predSym terms in
        let head = Subgoal.atom atom ~region in
        return @@ query Query.{ head = Some head; body } ~region)
    | _ -> return t)
;;
