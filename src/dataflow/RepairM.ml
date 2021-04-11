open Core_kernel

module type S = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val fresh_guardsym : Name.t t
  val err_range_violations : Violation.t list -> _ t
end
