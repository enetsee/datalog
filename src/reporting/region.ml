open Lib
open Core_kernel

type t =
  { start_ : Position.t
  ; end_ : Position.t
  }
[@@deriving eq]

let empty = { start_ = Position.empty; end_ = Position.empty }
let merge { start_; _ } { end_; _ } = { start_; end_ }

let is_empty {start_;end_} = Position.is_empty start_ && Position.is_empty end_
let merge_many = function
  | [] -> empty
  | init :: rest -> List.fold_left rest ~init ~f:merge
;;

include Monoid.Make0 (struct
  type nonrec t = t

  let mempty = empty
  let append = merge
end)
