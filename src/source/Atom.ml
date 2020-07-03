open Core_kernel
open Lib
open Reporting

module type Minimal = sig
  module Term : Term.S

  type t =
    { pred_nm : Core.Name.t Located.t
    ; terms : Term.t list
    }

  include HasCoreRepr.S with type t := t
end

module TermMinimal :
  Minimal with module Term = Term.Term and type repr = Core.Lit.Raw.t = struct
  module Term = Term.Term

  type t =
    { pred_nm : Core.Name.t Located.t
    ; terms : Core.Term.t list
    }

  type repr = Core.Lit.Raw.t

  let to_core { pred_nm = Located.{ elem = name; region }; terms } =
    Result.(
      List.map ~f:Term.to_core terms
      |> all
      |> map ~f:(fun terms ->
             Core.(
               let arity = List.length terms in
               let pred = Pred.pred name ~arity in
               Lit.Raw.(lit pred terms ~region))))
  ;;
end

module TmvarMinimal :
  Minimal with module Term = Term.Tmvar and type repr = Core.Lit.Raw.t = struct
  module Term = Term.Tmvar

  type t =
    { pred_nm : Core.Name.t Located.t
    ; terms : Core.Tmvar.t Located.t list
    }

  type repr = Core.Lit.Raw.t

  let to_core { pred_nm = Located.{ elem = name; region }; terms } =
    Result.(
      List.map ~f:Term.to_core terms
      |> all
      |> map ~f:(fun terms ->
             let arity = List.length terms in
             Core.(
               let pred = Pred.pred name ~arity in
               Lit.Raw.(lit pred terms ~region))))
  ;;
end

module SymbolMinimal :
  Minimal with module Term = Term.Symbol and type repr = Core.Knowledge.t =
struct
  module Term = Term.Symbol

  type t =
    { pred_nm : Core.Name.t Located.t
    ; terms : Core.Term.t list
    }

  type repr = Core.Knowledge.t

  let to_core { pred_nm = Located.{ elem = name; region }; terms } =
    Result.(
      List.map ~f:Term.to_core terms
      |> all
      >>= fun terms ->
      let arity = List.length terms in
      Core.(
        let pred = Pred.pred name ~arity in
        return @@ Knowledge.knowledge pred terms ~region))
  ;;
end

(** An `Atom.t` is an atomic formula that may appear in the head or body of a 
    Datalog clause, query or fact Here the atom is abstracted over the type of 
    terms. 
  *)
module type S = sig
  include Minimal
  include Pretty.S0 with type t := t
  include Core.HasVars.S with type t := t

  val atom : Core.Name.t Located.t -> Term.t list -> t
end

module Make (M : Minimal) : S with module Term = M.Term and type repr = M.repr =
struct
  include M

  let atom pred_nm terms = { pred_nm; terms }

  (* -- HasVars implementation ---------------------------------------------- *)

  let vars_of { terms; _ } = List.concat_map terms ~f:Term.vars_of

  (* -- Pretty implementation ----------------------------------------------- *)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { pred_nm; terms } =
      Fmt.(
        hovbox
        @@ pair (Located.pp Core.Name.pp)
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (pred_nm, terms)
    ;;

    let pp = `NoPrec pp
  end)
end

(** Atomic formulae over term variables  *)
module Tmvar = Make (TmvarMinimal)

(** Atomic formulae over symbols  *)
module Symbol = Make (SymbolMinimal)

(** Atomic formulae over full terms *)
module Term = Make (TermMinimal)
