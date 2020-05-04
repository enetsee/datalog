open Core_kernel
open Reporting
open Lib

module X = struct
  (* Include for use with declarations later *)
  type t = StSentence of Sentence.t [@@deriving eq, compare]

  let pp ppf = function
    | StSentence s -> Sentence.pp ppf s
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Transformations ------------------------------------------------------- *)

let transform_atom t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_atom ~f s)
;;

let transform_head t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_head ~f s)
;;

let transform_body t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_body ~f s)
;;

let transform_clause t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_clause ~f s)
;;

let transform_query t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_query ~f s)
;;

let transform_fact t ~f =
  match t with
  | StSentence s -> StSentence (Sentence.transform_fact ~f s)
;;

let transform_sentence t ~f =
  match t with
  | StSentence s -> StSentence (f s)
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
  ;;

  let transform_head t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_head ~f s
  ;;

  let transform_body t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_body ~f s
  ;;

  let transform_clause t ~f =
    match t with
    | StSentence s ->
      M.map ~f:(fun s -> StSentence s) @@ S.transform_clause ~f s
  ;;

  let transform_query t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_query ~f s
  ;;

  let transform_fact t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ S.transform_fact ~f s
  ;;

  let transform_sentence t ~f =
    match t with
    | StSentence s -> M.map ~f:(fun s -> StSentence s) @@ f s
  ;;
end
