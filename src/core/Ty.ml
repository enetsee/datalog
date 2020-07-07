open Core_kernel
open Lib

module Minimal = struct
  type t =
    | Top
    | Symbol
    | Number
    | Bool
    | Real
    | Int
    | Date
    | Named of Name.t
    | Bot
  [@@deriving eq, compare, sexp]

  let pp ppf = function
    | Top -> Fmt.string ppf "top"
    | Bot -> Fmt.string ppf "bottom"
    | Symbol -> Fmt.string ppf "symbol"
    | Named name -> Name.pp ppf name
    | Number -> Fmt.string ppf "number"
    | Int -> Fmt.string ppf "int"
    | Real -> Fmt.string ppf "real"
    | Bool -> Fmt.string ppf "bool"
    | Date -> Fmt.string ppf "date"
  ;;

  let pp = `NoPrec pp
  let named str = Named (Name.from_string str)
  let named' n = Named n
end

include Minimal
include Pretty.Make0 (Minimal)
module Map = Map.Make (Minimal)
module Set = Set.Make (Minimal)

module TRG = struct
  type t = Set.t Map.t

  let empty = Map.empty
  let subtypes_of trg ~ty = Map.find trg ty

  (** Given a map of (subty,ty) pairs, find its transitive closure as a map
      from a type to all of its subtypes *)
  let from_list xs =
    let step init =
      List.fold_left ~init ~f:(fun accu ty ->
          let subs =
            Set.union_list
            @@ List.map ~f:(fun ty' ->
                   if equal ty ty'
                   then Set.(singleton ty')
                   else
                     Option.value ~default:Set.(singleton ty')
                     @@ Map.find accu ty')
            @@ Set.to_list
            @@ Map.find_exn accu ty
          in
          Map.update accu ty ~f:Fn.(const subs))
      @@ Map.keys init
    in
    let rec fix prev =
      let next = step prev in
      if Map.equal Set.equal next prev then next else fix next
    in
    fix
    @@ Map.of_alist_exn
    @@ List.map ~f:(function
           | (ty, _) :: _ as xs -> ty, Set.of_list @@ (ty :: List.map ~f:snd xs)
           | _ -> failwith "impossible")
    @@ List.group ~break:(fun (ty1, _) (ty2, _) -> not @@ equal ty1 ty2)
    @@ List.dedup_and_sort ~compare:(Tuple2.compare ~cmp1:compare ~cmp2:compare)
    @@ List.concat_map ~f:(fun (subty, ty) -> [ subty, subty; ty, subty ]) xs
  ;;
end

let top = Top
let bottom = Bot

module type MonadTy = sig
  include Monad.S
  include Applicative.S with type 'a t := 'a t

  val subtypes_of : Minimal.t -> Set.t t
end

module Make (M : MonadTy) = struct
  let meet t1 t2 =
    M.(
      match t1, t2 with
      | _ when equal t1 t2 -> return t1
      | Top, t | t, Top -> return t
      | Bot, _ | _, Bot -> return Bot
      | Number, Int | Int, Number -> return Int
      | Number, Real | Real, Number -> return Real
      | _ ->
        subtypes_of t1
        >>= fun s1 ->
        if Set.mem s1 t2
        then return t2
        else
          subtypes_of t2
          >>= fun s2 ->
          if Set.mem s2 t1
          then return t1
          else (
            match List.hd Set.(elements @@ inter s1 s2) with
            | Some t -> return t
            | _ -> return Bot))
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

  let leq t1 t2 = M.map ~f:(equal t1) @@ meet t1 t2
end

module Defn = struct
  type t = Subty of Minimal.t

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | Subty ty -> Fmt.(prefix (any "<:@;") pp) ppf ty
    ;;

    let pp = `NoPrec pp
  end)
end
