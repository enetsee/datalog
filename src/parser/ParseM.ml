open Core_kernel
open Reporting

module type S = sig
  include Monad.S

  val err_bad_parse_state : unit -> _ t
  val err_parse_err : msg:string option -> region:Region.t -> _ t
  val err_file_not_found : string -> _ t
end
