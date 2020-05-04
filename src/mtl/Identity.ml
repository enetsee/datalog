open Core_kernel

module Minimal = struct
  type 'a t = 'a

  let map = `Custom (fun t ~f -> f t)
  let bind t ~f = f t
  let apply f t = f t
  let return t = t
end

include Minimal
include Monad.Make (Minimal)
include Applicative.Make (Minimal)

let run (m : 'a t) : 'a = m
