open Core_kernel

(* A wrapper around StateT Result to hide complexity and provide some helpers *)

(** TODO: flatten this down and possibly add reader? *)
module M = Mtl.StateT.Make2 (Result)

module State = struct
  type t =
    { prefix : string option
    ; reserved : String.Set.t
    ; counter : int
    }

  let counter { counter; _ } = counter
  let prefix { prefix; _ } = prefix
end

module Err = struct
  type t = QueryNamed of Reporting.Region.t
end

module Minimal = struct
  type 'a t = ('a, State.t, Err.t) M.t

  let bind = M.bind
  let map = `Custom M.map
  let apply = M.apply
  let return = M.return
end

include Minimal
include Monad.Make (Minimal)
include Applicative.Make (Minimal)

let fail err = M.lift @@ Error err
let query_already_named region : 'a t = fail @@ Err.QueryNamed region

let rec fresh () : string t =
  M.(
    get
    >>= fun (State.{ counter; reserved; prefix } as st) ->
    put { st with counter = counter + 1 }
    >>= fun _ ->
    let ctr_string = string_of_int counter in
    let candidate =
      Option.value_map
        ~default:ctr_string
        ~f:(fun pfx -> pfx ^ ctr_string)
        prefix
    in
    if String.Set.mem reserved candidate then fresh () else return candidate)
;;
