open Reporting

module type S = sig
  include Adorn.AdornM
  include RangeRepair.RepairM with type 'a t := 'a t
  include Typing.TypingM with type 'a t := 'a t

  val with_tydefs : Ty.TRG.t -> 'a t -> 'a t
  val get_typing_env : TypingEnv.t t
  val set_typing_env : TypingEnv.t -> unit t
  val err_neg_cycles : (Pred.t * Pred.t) list -> _ t
  val err_unresolved_export : Name.t -> Region.t -> _ t
  val err_empty_relations : Pred.t list -> _ t
  val warn_unused_clauses : Reporting.Region.t list -> unit t
end
