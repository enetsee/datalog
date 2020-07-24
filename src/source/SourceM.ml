open Core_kernel
open Core
open Reporting

module type S = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val is_name_already_bound : Name.t -> bool t
  val warn_no_exports : unit -> unit t
  val err_name_shadowed : Name.t -> Region.t -> _ t
  val err_reserved_word : Region.t -> _ t
  val err_head_not_atom : Region.t -> _ t
  val err_fact_has_wildcard : Region.t -> _ t
  val err_fact_not_range_restricted : Region.t -> _ t
  val err_clause_neg : Region.t -> _ t
  val err_clause_disj : Region.t -> _ t
  val err_clause_null_op : Region.t -> _ t
end
