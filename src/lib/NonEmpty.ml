module X = struct
  type +'a t = 'a * 'a list

  let equal eq_a (x, xs) (y, ys) = eq_a x y && List.for_all2 eq_a xs ys

  let rec compare cmp_a (x, xs) (y, ys) =
    let c = cmp_a x y in
    if c <> 0 then c else compare_helper cmp_a (xs, ys)

  and compare_helper cmp_a = function
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | x :: xs, y :: ys -> compare cmp_a (x, xs) (y, ys)
  ;;

  let append (x, xs) (y, ys) = x, xs @ (y :: ys)

  let pp pp_a ppf t =
    Fmt.(brackets @@ pair ~sep:comma pp_a @@ list ~sep:comma pp_a) ppf t
  ;;

  let pp = `NoPrec pp
  let map (x, xs) ~f = f x, List.map f xs
  let foldLeft (x, xs) ~f ~init = List.fold_left f (f init x) xs
end

include X
include Semigroup.Make1 (X)
include Functor.Make1 (X)
include Pretty.Make1 (X)
include Foldable.Make1 (X)

let singleton a = a, []
let cons x (hd, tl) = x, hd :: tl
let to_list (hd, tl) = hd :: tl
