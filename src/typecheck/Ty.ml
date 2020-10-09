open Core_kernel
open Core

module type MonadTy = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val subtypes_of : Ty.t -> Ty.Set.t t
end

module Make (M : MonadTy) = struct
  let meet t1 t2 =
    M.(
      match t1, t2 with
      | _ when Ty.equal t1 t2 -> return t1
      | Ty.Top, t | t, Ty.Top -> return t
      | Bot, _ | _, Bot -> return Ty.Bot
      | Number, Int | Int, Number -> return Ty.Int
      | Number, Real | Real, Number -> return Ty.Real
      | _ ->
        subtypes_of t1
        >>= fun s1 ->
        if Ty.Set.mem s1 t2
        then return t2
        else
          subtypes_of t2
          >>= fun s2 ->
          if Ty.Set.mem s2 t1
          then return t1
          else (
            match List.hd Ty.Set.(elements @@ inter s1 s2) with
            | Some t -> return t
            | _ -> return Ty.Bot))
  ;;

  let meets ts =
    match ts with
    | [] -> M.return None
    | x :: xs ->
      M.map ~f:Option.some
      @@ List.fold_left
           xs
           ~init:M.(return x)
           ~f:M.(fun mty ty -> mty >>= meet ty)
  ;;

  let leq t1 t2 = M.map ~f:Ty.(equal t1) @@ meet t1 t2
end
