open Core_kernel
open Lib

module Make (F : Functor.S1) = struct
  type 'a t = { cofree : ('a, 'a t F.t) Tuple2.t Lazy.t }

  let from_fn f = { cofree = Lazy.from_fun f }
  let mk a t = { cofree = Lazy.from_fun (fun _ -> a, t) }
  let head { cofree } = fst @@ Lazy.force cofree
  let tail { cofree } = snd @@ Lazy.force cofree

  let rec build (k : 's -> 'a * 's F.t) s =
    { cofree =
        Lazy.from_fun (fun _ -> Tuple2.map_snd ~f:(F.map ~f:(build k)) @@ k s)
    }
  ;;

  let unfold (e : 's -> 'a) n = build (fun s -> e s, n s)
end
