open Core_kernel
open Lib

module type S1 = sig
  module Err : Tycon.S0
  include Monad.S

  val fail : Err.t -> 'a t
end

module Make (Err : Tycon.S0) :
  S1 with module Err := Err and type 'a t = ('a, Err.t) result = struct
  type 'a t = ('a, Err.t) result

  let map t ~f = Result.map t ~f
  let bind t ~f = Result.bind t ~f
  let return x = Ok x

  include Monad.Make (struct
    type nonrec 'a t = 'a t

    let map = `Custom map
    let bind = bind
    let return = return
  end)

  let fail err = Error err
end
