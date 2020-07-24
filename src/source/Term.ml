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

  module Make (M : SourceM.S) = struct
    let to_core t = M.return t
  end
end

module Tmvar :
  S with type t = Core.Tmvar.t Located.t and type repr = Core.Term.t = struct
  type t = Core.Tmvar.t Located.t
  type repr = Core.Term.t

  module Make (M : SourceM.S) = struct
    let to_core Located.{ elem; region } =
      M.return Core.Term.(var' ~region elem)
    ;;
  end

  let vars_of Located.{ elem; _ } = [ elem ]

  include Pretty.Make0 (struct
    type nonrec t = Core.Tmvar.t Located.t

    let pp = `NoPrec Located.(pp Core.Tmvar.pp)
  end)
end

module Symbol :
  S with type t = Core.Term.t and type repr = Core.Knowledge.KTerm.t = struct
  type t = Core.Term.t
  type repr = Core.Knowledge.KTerm.t

  let pp = Core.Term.pp
  let pp_prec = Core.Term.pp_prec
  let to_string = Core.Term.to_string
  let vars_of = Core.Term.vars_of

  module Make (M : SourceM.S) = struct
    let to_core t =
      M.(
        match t with
        | Core.Term.TSym (sym, _) -> return (Core.Knowledge.KTerm.KSymbol sym)
        | TParam (nm, _) -> return (Core.Knowledge.KTerm.KParam nm)
        | TWild (_, region) -> err_fact_has_wildcard region
        | TVar (_, region) -> err_fact_not_range_restricted region)
    ;;
  end
end
