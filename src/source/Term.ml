open Lib
open Reporting

(* Signature for type that may appear as terms within atomic formulae 
   and can be translated to `Core.Term.t` *)
module type S = sig
  include Pretty.S0
  include Core.HasVars.S with type t := t
  include HasCoreRepr.S with type t := t
end

module Term : S with type t = Core.Term.t and type repr = Core.Term.t = struct
  type t = Core.Term.t
  type repr = Core.Term.t

  let pp = Core.Term.pp
  let pp_prec = Core.Term.pp_prec
  let to_string = Core.Term.to_string
  let vars_of = Core.Term.vars_of
  let to_core t = MonadCompile.return t
end

module Tmvar :
  S with type t = Core.Tmvar.t Located.t and type repr = Core.Term.t = struct
  type t = Core.Tmvar.t Located.t
  type repr = Core.Term.t

  let to_core Located.{ elem; region } =
    MonadCompile.return @@ Core.Term.var' ~region elem
  ;;

  let vars_of Located.{ elem; _ } = [ elem ]

  include Pretty.Make0 (struct
    type nonrec t = Core.Tmvar.t Located.t

    let pp = `NoPrec Located.(pp Core.Tmvar.pp)
  end)
end

module Symbol : S with type t = Core.Term.t and type repr = Core.Symbol.t =
struct
  type t = Core.Term.t
  type repr = Core.Symbol.t

  let pp = Core.Term.pp
  let pp_prec = Core.Term.pp_prec
  let to_string = Core.Term.to_string
  let vars_of = Core.Term.vars_of

  let to_core t =
    MonadCompile.(
      match t with
      | Core.Term.TSym (sym, _) -> return sym
      | TWild region -> fail Err.(FactHasWildcard region)
      | TVar (_, region) -> fail (Err.FactNotRangeRestricted region))
  ;;
end
