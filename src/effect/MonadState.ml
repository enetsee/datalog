open Core_kernel
open Lib

module type Minimal1 = sig
  module State : Tycon.S0
  include Monad.S

  val state : (State.t -> 'a * State.t) -> 'a t
end

module type S1 = sig
  include Minimal1

  val get : State.t t
  val put : State.t -> unit t
  val modify : (State.t -> State.t) -> unit t
  val gets : (State.t -> 'a) -> 'a t
end

module Make (X : Minimal1) :
  S1 with module State := X.State and type 'a t := 'a X.t = struct
  include X

  let get = state (fun s -> s, s)
  let put s = state (fun _ -> (), s)
  let modify f = state (fun s -> (), f s)
  let gets f = map ~f get
end
