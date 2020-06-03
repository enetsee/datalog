open Core_kernel
open Lib

module type Minimal1 = sig
  module Topic : Monoid.S0
  include Monad.S

  (** embeds a simple writer action *)
  val writer : 'a * Topic.t -> 'a t

  (** an action that executes the action m and adds its output to the value of 
      the computation. *)
  val listen : 'a t -> ('a * Topic.t) t

  (** an action that executes the action m, which returns a value and a function, 
      and returns the value, applying the function to the output *)
  val pass : ('a * (Topic.t -> Topic.t)) t -> 'a t
end

module type S1 = sig
  include Minimal1

  (** an action that produces the output w *)
  val tell : Topic.t -> unit t

  (** an action that executes the action m and adds the result of applying f to 
      the output to the value of the computation. *)
  val listens : 'a t -> f:(Topic.t -> 'b) -> ('a * 'b) t

  (** an action that executes the action m and applies the function f to its 
      output, leaving the return value unchanged. *)
  val censor : 'a t -> f:(Topic.t -> Topic.t) -> 'a t
end

module Make (X : Minimal1) :
  S1 with type 'a t := 'a X.t and module Topic := X.Topic = struct
  include X

  let tell topic = writer ((), topic)
  let listens m ~f = bind (listen m) ~f:(fun (a, topic) -> return (a, f topic))
  let censor m ~f = pass @@ bind m ~f:(fun a -> return (a, f))
end

module MakeTopic (Topic : Monoid.S0) (X : Minimal1 with module Topic := Topic) :
  S1 with type 'a t := 'a X.t and module Topic := Topic = struct
  include X

  let tell topic = writer ((), topic)
  let listens m ~f = bind (listen m) ~f:(fun (a, topic) -> return (a, f topic))
  let censor m ~f = pass @@ bind m ~f:(fun a -> return (a, f))
end
