open Core_kernel
open Core

module type S = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val get_pred_constraint : Name.t -> Constraint.t t
  val set_pred_constraint : Name.t -> Constraint.t -> unit t
  val get_pred_effects : Name.t -> Eff.Set.t t
end
