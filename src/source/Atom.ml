open Core_kernel
open Lib
open Reporting

module type Minimal = sig
  module Term : Term.S

  type t =
    { predSym : Core.Pred.Name.t Located.t
    ; terms : Term.t list
    ; nature : Core.Nature.t option
    }

  include HasCoreRepr.S with type t := t
end

module TermMinimal :
  Minimal with module Term = Term.Term and type repr = Core.Raw.Lit.t = struct
  module Term = Term.Term

  type t =
    { predSym : Core.Pred.Name.t Located.t
    ; terms : Core.Term.t list
    ; nature : Core.Nature.t option
    }

  type repr = Core.Raw.Lit.t

  let to_core { predSym = Located.{ elem; region }; terms; nature } =
    MonadCompile.(
      List.map ~f:Term.to_core terms
      |> all
      |> map ~f:(fun terms ->
             Core.(
               let arity = List.length terms
               and nature = Option.value ~default:Nature.Logical nature
               and name = elem in
               let pred = Pred.{ name; arity; nature } in
               Raw.Lit.(lit pred terms ~region))))
  ;;
end

module TmvarMinimal :
  Minimal with module Term = Term.Tmvar and type repr = Core.Raw.Lit.t = struct
  module Term = Term.Tmvar

  type t =
    { predSym : Core.Pred.Name.t Located.t
    ; terms : Core.Tmvar.t Located.t list
    ; nature : Core.Nature.t option
    }

  type repr = Core.Raw.Lit.t

  let to_core { predSym = Located.{ elem; region }; terms; nature } =
    MonadCompile.(
      List.map ~f:Term.to_core terms
      |> all
      |> map ~f:(fun terms ->
             Core.(
               let arity = List.length terms
               and nature = Option.value ~default:Nature.Logical nature
               and name = elem in
               let pred = Pred.{ name; arity; nature } in
               Raw.Lit.(lit pred terms ~region))))
  ;;
end

module SymbolMinimal :
  Minimal with module Term = Term.Symbol and type repr = Core.Knowledge.t =
struct
  module Term = Term.Symbol

  type t =
    { predSym : Core.Pred.Name.t Located.t
    ; terms : Core.Term.t list
    ; nature : Core.Nature.t option
    }

  type repr = Core.Knowledge.t

  let to_core { predSym = Located.{ elem; region }; terms; nature } =
    MonadCompile.(
      List.map ~f:Term.to_core terms
      |> all
      >>= fun terms ->
      let arity = List.length terms in
      let nature = Option.value ~default:Core.Nature.Logical nature in
      let pred = Core.Pred.{ name = elem; arity; nature } in
      return @@ Core.Knowledge.knowledge pred terms ~region)
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

  val atom : Core.Pred.Name.t Located.t -> Term.t list -> t
  val set_nature : t -> t MonadCompile.t
  val check_extralogical_clash : t -> unit MonadCompile.t
end

module Make (M : Minimal) : S with module Term = M.Term and type repr = M.repr =
struct
  include M

  let atom predSym terms = { predSym; terms; nature = None }

  (* -- Compilation --------------------------------------------------------- *)

  let set_nature t =
    MonadCompile.(
      ask
      >>= fun Env.{ extras; _ } ->
      return
      @@ Option.value_map ~default:t ~f:(fun n -> { t with nature = Some n })
      @@ Core.Pred.Name.Map.find extras t.predSym.elem)
  ;;

  let check_extralogical_clash { predSym = Located.{ elem; region }; _ } =
    MonadCompile.(
      ask
      >>= fun Env.{ extras; _ } ->
      match Core.Pred.Name.Map.find extras elem with
      | Some _ -> warn @@ PredNameClash region
      | _ -> return ())
  ;;

  (* -- HasVars implementation ---------------------------------------------- *)

  let vars_of { terms; _ } = List.concat_map terms ~f:Term.vars_of

  (* -- Pretty implementation ----------------------------------------------- *)

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { predSym; terms; _ } =
      Fmt.(
        hovbox
        @@ pair (Located.pp Core.Pred.Name.pp)
        @@ parens
        @@ list ~sep:comma Term.pp)
        ppf
        (predSym, terms)
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
