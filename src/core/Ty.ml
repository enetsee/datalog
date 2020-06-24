open Core_kernel
open Lib

module Name = struct
  type t = { name : string } [@@deriving eq, compare, sexp]

  let of_string name = { name }

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf { name } = Fmt.(prefix (always "@") string) ppf name
    let pp = `NoPrec pp
  end)

  module Map = Map.Make (String)
end

module Minimal = struct
  type t =
    | Top
    | Number
    | Bool
    | Real
    | Int
    | Symbol
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
  ;;

  let pp = `NoPrec pp
  let named str = Named (Name.of_string str)
  let named' n = Named n
end

include Minimal
include Pretty.Make0 (Minimal)
module Map = Map.Make (Minimal)
module Set = Set.Make (Minimal)

let top = Top
let bottom = Bot

(** Given a map of (subty,ty) pairs, find its transitive closure as a map
    from a type to all of its subtypes *)
let transitive_closure xs =
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

let meet t1 t2 ~trg =
  match t1, t2 with
  | _ when equal t1 t2 -> t1
  | Top, t | t, Top -> t
  | Bot, _ | _, Bot -> Bot
  | Number, Real | Real, Number -> Real
  | Number, Int | Int, Number -> Int
  | _ ->
    Option.(
      value
        ~default:Bot
        (Map.find trg t1
        >>= fun s1 ->
        if Set.mem s1 t2
        then Some t2
        else
          Map.find trg t2
          >>= fun s2 ->
          if Set.mem s2 t1
          then Some t1
          else List.hd @@ Set.(elements @@ inter s1 s2)))
;;
