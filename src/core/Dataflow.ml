(* open Graph *)

open Core_kernel

module Const = struct
  type t =
    | CSym of Symbol.t
    | CWild
  [@@deriving compare]
end

module Sink = struct
  type 'a t =
    | SnkLit of 'a Literal.t * int
    | SnkPred of 'a Pred.t * int
end

module Node = struct
  type 'a t =
    | NPred of
        { pred : 'a Pred.t
        ; argindex : int
        }
    | NLit of
        { lit : 'a Literal.t
        ; argindex : int
        }
    | NConst of Const.t
    | NNull
  [@@deriving compare]
end

(* module Gr = 
  Imperative.Graph.Concrete(Node) *)

module Source = struct
  type 'a t =
    | SrcLit of 'a Literal.t * int
    | SrcConst of Const.t
end
