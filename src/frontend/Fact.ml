open Core_kernel
open Reporting
open Lib

module X = struct
  type t = { head : Core.Term.t Subgoal.t } [@@deriving eq, compare]

  let fact head = { head }
  let pp ppf { head } = Fmt.(Subgoal.pp Core.Term.pp ++ any ".") ppf head
  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)

(* -- Query ----------------------------------------------------------------- *)

let atoms { head } = Subgoal.atoms head
let tmvars { head } = Subgoal.tmvars Core.Term.tmvars head

(* -- Core translation ------------------------------------------------------ *)

(** Translate terms in the head of a fact to symbols guarding against 
    non-symbol terms being present
*)
let term_to_core Located.{ elem; region } =
  Logger.(
    match elem with
    | Core.Term.TSym sym -> return sym
    | TVar _ -> fail @@ fact_not_range_restricted region
    | TWild -> fail @@ fact_has_wildcard region)
;;

(** Translate a `Fact` to it's corresponding representation in the `Core`
    language i.e. `Knowledge` 
*)
let to_core { head } =
  Logger.(
    match Subgoal.proj head with
    | { elem = SAtom Atom.{ predSym; terms }; region } ->
      List.map ~f:term_to_core terms
      |> all
      >>= fun terms ->
      let arity = List.length terms in
      let annot = Core.Annotation.base region in
      let pred =
        Core.Pred.
          { predSym = predSym.elem
          ; annot = Core.Annotation.base predSym.region
          ; arity
          ; nature = Core.Nature.Logical
          }
      in
      return Core.Knowledge.{ pred; terms; annot }
    | { region; _ } -> fail @@ head_not_atom region)
;;
