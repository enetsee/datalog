open Core_kernel
open Lib

module type S = sig
  module M : Monad.S
  include Monad.S2
  include Applicative.S2 with type ('a, 'b) t := ('a, 'b) t

  val lift : 'a M.t -> ('a, 'e) t
  val get : ('st, 'st) t
  val put : 'st -> (unit, 'st) t
  val state : ('st -> ('a * 'st) M.t) -> ('a, 'st) t
  val modify : ('st -> 'st) -> (unit, 'st) t
  val run : ('a, 'b) t -> init:'b -> ('a * 'b) M.t
  val eval : ('a, 'b) t -> init:'b -> 'a M.t
  val exec : ('a, 'b) t -> init:'b -> 'b M.t
end

module type S2 = sig
  module M : Monad.S2
  include Monad.S3
  include Applicative.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

  val lift : ('a, 'f) M.t -> ('a, 'e, 'f) t
  val get : ('st, 'st, 'f) t
  val put : 'st -> (unit, 'st, 'f) t
  val run : ('a, 'b, 'c) t -> init:'b -> ('a * 'b, 'c) M.t
  val eval : ('a, 'b, 'c) t -> init:'b -> ('a, 'c) M.t
  val exec : ('a, 'b, 'c) t -> init:'b -> ('b, 'c) M.t
end

module Make (M : Monad.S) : S with module M := M = struct
  type ('a, 'e) t = { apply : 'r. 'e -> ('a -> 'e -> 'r M.t) -> 'r M.t }

  let lift m = { apply = (fun state k -> M.(m >>= fun y -> k y state)) }
  let get = { apply = (fun state k -> k state state) }
  let put st = { apply = (fun _ k -> k () st) }

  let state f : ('a, 'st) t =
    { apply = (fun st k -> M.(f st >>= fun (x, st') -> k x st')) }
  ;;

  let return x = { apply = (fun state k -> k x state) }

  let bind { apply } ~f =
    { apply =
        (fun state k -> apply state @@ fun a state' -> (f a).apply state' k)
    }
  ;;

  let map { apply } ~f =
    { apply = (fun state k -> apply state @@ fun x -> k @@ f x) }
  ;;

  let apply f { apply } =
    { apply =
        (fun state k ->
          f.apply state @@ fun g state' -> apply state' Fn.(compose k g))
    }
  ;;

  include Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

  include Applicative.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let apply = apply
    let map = `Custom map
  end)

  let run { apply } ~init = apply init (fun a s -> M.return (a, s))
  let eval { apply } ~init = apply init (fun a _ -> M.return a)
  let exec { apply } ~init = apply init (fun _ s -> M.return s)
  let modify f = get >>= fun st -> put (f st)
end

module Make2 (M : Monad.S2) : S2 with module M := M = struct
  type ('a, 'e, 'f) t =
    { apply : 'r. 'e -> ('a -> 'e -> ('r, 'f) M.t) -> ('r, 'f) M.t }

  let lift m = { apply = (fun state k -> M.(m >>= fun y -> k y state)) }
  let get = { apply = (fun state k -> k state state) }
  let put st = { apply = (fun _ k -> k () st) }
  let return x = { apply = (fun state k -> k x state) }

  let bind { apply } ~f =
    { apply =
        (fun state k -> apply state @@ fun a state' -> (f a).apply state' k)
    }
  ;;

  let map { apply } ~f =
    { apply = (fun state k -> apply state @@ fun x -> k @@ f x) }
  ;;

  let apply f { apply } =
    { apply =
        (fun state k ->
          f.apply state @@ fun g state' -> apply state' Fn.(compose k g))
    }
  ;;

  include Monad.Make3 (struct
    type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c) t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

  include Applicative.Make3 (struct
    type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c) t

    let return = return
    let apply = apply
    let map = `Custom map
  end)

  let run { apply } ~init = apply init (fun a s -> M.return (a, s))
  let eval { apply } ~init = apply init (fun a _ -> M.return a)
  let exec { apply } ~init = apply init (fun _ s -> M.return s)
end

module type S_with_state = sig
  module State : Tycon.S0
  module M : Monad.S
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val lift : 'a M.t -> 'a t
  val get : State.t t
  val put : State.t -> unit t
  val state : (State.t -> ('a * State.t) M.t) -> 'a t
  val modify : (State.t -> State.t) -> unit t
  val run : 'a t -> init:State.t -> ('a * State.t) M.t
  val eval : 'a t -> init:State.t -> 'a M.t
  val exec : 'a t -> init:State.t -> State.t M.t
end

module type S2_with_state = sig
  module State : Tycon.S0
  module M : Monad.S2
  include Monad.S2
  include Applicative.S2 with type ('a, 'e) t := ('a, 'e) t

  val lift : ('a, 'e) M.t -> ('a, 'e) t
  val get : (State.t, 'e) t
  val put : State.t -> (unit, 'e) t
  val state : (State.t -> ('a * State.t, 'e) M.t) -> ('a, 'e) t
  val modify : (State.t -> State.t) -> (unit, 'e) t
  val run : ('a, 'e) t -> init:State.t -> ('a * State.t, 'e) M.t
  val eval : ('a, 'e) t -> init:State.t -> ('a, 'e) M.t
  val exec : ('a, 'e) t -> init:State.t -> (State.t, 'e) M.t
end

module Make_with_state (State : Tycon.S0) (M : Monad.S) :
  S_with_state with module State := State and module M := M = struct
  type 'a t = { apply : 'r. State.t -> ('a -> State.t -> 'r M.t) -> 'r M.t }

  let lift m = { apply = (fun state k -> M.(m >>= fun y -> k y state)) }
  let get = { apply = (fun state k -> k state state) }
  let put st = { apply = (fun _ k -> k () st) }
  let state f = { apply = (fun st k -> M.(f st >>= fun (x, st') -> k x st')) }
  let return x = { apply = (fun state k -> k x state) }

  let bind { apply } ~f =
    { apply =
        (fun state k -> apply state @@ fun a state' -> (f a).apply state' k)
    }
  ;;

  let map { apply } ~f =
    { apply = (fun state k -> apply state @@ fun x -> k @@ f x) }
  ;;

  let apply f { apply } =
    { apply =
        (fun state k ->
          f.apply state @@ fun g state' -> apply state' Fn.(compose k g))
    }
  ;;

  include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

  include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let apply = apply
    let map = `Custom map
  end)

  let run { apply } ~init = apply init (fun a s -> M.return (a, s))
  let eval { apply } ~init = apply init (fun a _ -> M.return a)
  let exec { apply } ~init = apply init (fun _ s -> M.return s)
  let modify f = get >>= fun st -> put (f st)
end

module Make2_with_state (State : Tycon.S0) (M : Monad.S2) :
  S2_with_state with module State := State and module M := M = struct
  type ('a, 'f) t =
    { apply : 'r. State.t -> ('a -> State.t -> ('r, 'f) M.t) -> ('r, 'f) M.t }

  let lift m = { apply = (fun state k -> M.(m >>= fun y -> k y state)) }
  let get = { apply = (fun state k -> k state state) }
  let put st = { apply = (fun _ k -> k () st) }
  let state f = { apply = (fun st k -> M.(f st >>= fun (x, st') -> k x st')) }
  let return x = { apply = (fun state k -> k x state) }

  let bind { apply } ~f =
    { apply =
        (fun state k -> apply state @@ fun a state' -> (f a).apply state' k)
    }
  ;;

  let map { apply } ~f =
    { apply = (fun state k -> apply state @@ fun x -> k @@ f x) }
  ;;

  let apply f { apply } =
    { apply =
        (fun state k ->
          f.apply state @@ fun g state' -> apply state' Fn.(compose k g))
    }
  ;;

  include Monad.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let bind = bind
    let map = `Custom map
  end)

  include Applicative.Make2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = return
    let apply = apply
    let map = `Custom map
  end)

  let run { apply } ~init = apply init (fun a s -> M.return (a, s))
  let eval { apply } ~init = apply init (fun a _ -> M.return a)
  let exec { apply } ~init = apply init (fun _ s -> M.return s)
  let modify f = get >>= fun st -> put (f st)
end
