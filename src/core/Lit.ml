open Lib
open Reporting

module type S = sig
  type t [@@deriving compare, sexp, eq]

  include Pretty.S0 with type t := t
  include HasVars.S with type t := t
  include HasTerms.S with type t := t
  include HasEffects.S with type t := t
  include HasRegion.S with type t := t

  val pred_of : t -> Pred.t
  val pol_of : t -> Polarity.t
  val neg : t -> t
end
