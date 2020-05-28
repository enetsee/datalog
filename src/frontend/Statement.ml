open Core_kernel
open Reporting
open Lib

module X = struct
  type t =
    | StSentence of Sentence.t
    | StDecl of Decl.t
  [@@deriving eq, compare]

  let pp ppf = function
    | StSentence s -> Sentence.pp ppf s
    | StDecl decl -> Decl.pp ppf decl
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Constructors ---------------------------------------------------------- *)
let sentence s = StSentence s
let decl decl = StDecl decl

(* -- Denstructors ---------------------------------------------------------- *)

let lower_sentence = function
  | StSentence s -> Some s
  | _ -> None
;;

(* -- Query ----------------------------------------------------------------- *)

let atoms = function
  | StSentence s -> Sentence.atoms s
  | _ -> []
;;

let tmvars = function
  | StSentence s -> Sentence.tmvars s
  | _ -> []
;;

(* == Transformations ======================================================= *)

(* -- Transformation helpers ------------------------------------------------ *)

let transform_atom t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_atom ~f s)
  | _ -> t
;;

let transform_head t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_head ~f s)
  | _ -> t
;;

let transform_body t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_body ~f s)
  | _ -> t
;;

let transform_clause t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_clause ~f s)
  | _ -> t
;;

let transform_query t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_query ~f s)
  | _ -> t
;;

let transform_fact t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_fact ~f s)
  | _ -> t
;;

let transform_sentence t ~f =
  match t with
  | StSentence s -> StSentence (f s)
  | _ -> t
;;

module Effect (M : sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t
end) =
struct
  module S = Sentence.Effect (M)

  let transform_atom t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_atom ~f s
    | _ -> M.return t
  ;;

  let transform_head t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_head ~f s
    | _ -> M.return t
  ;;

  let transform_body t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_body ~f s
    | _ -> M.return t
  ;;

  let transform_clause t ~f =
    match t with
    | StSentence s ->
      M.map ~f:(fun s -> StSentence s) @@ S.transform_clause ~f s
    | _ -> M.return t
  ;;

  let transform_query t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_query ~f s
    | _ -> M.return t
  ;;

  let transform_fact t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_fact ~f s
    | _ -> M.return t
  ;;

  let transform_sentence t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ f s
    | _ -> M.return t
  ;;
end

module Effect3 (M : sig
  include Monad.S3
  include Applicative.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end) =
struct
  module S = Sentence.Effect3 (M)

  let transform_atom t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_atom ~f s
    | _ -> M.return t
  ;;

  let transform_head t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_head ~f s
    | _ -> M.return t
  ;;

  let transform_body t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_body ~f s
    | _ -> M.return t
  ;;

  let transform_clause t ~f =
    match t with
    | StSentence s ->
      M.map ~f:(fun s -> StSentence s) @@ S.transform_clause ~f s
    | _ -> M.return t
  ;;

  let transform_query t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_query ~f s
    | _ -> M.return t
  ;;

  let transform_fact t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_fact ~f s
    | _ -> M.return t
  ;;

  let transform_sentence t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ f s
    | _ -> M.return t
  ;;
end

(* -- Normalization transforms ---------------------------------------------- *)

let split_disj t =
  Logger.(
    match t with
    | StSentence s -> map ~f:(List.map ~f:sentence) @@ Sentence.split_disj s
    | _ -> return [])
;;

let name_query t =
  Logger.(
    match t with
    | StSentence s -> map ~f:sentence @@ Sentence.name_query s
    | _ -> return t)
;;
