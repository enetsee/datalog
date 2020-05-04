open Lib
open Core
open Reporting

type 'a t =
  { predSym : Core.PredSymbol.t Located.t
  ; terms : 'a Located.t list
  ; nature : Core.ForeignFunc.t option [@compare.ignore]
  }

include Pretty.S1 with type 'a t := 'a t
include Functor.S1 with type 'a t := 'a t
include HasTmvars.S1 with type 'a t := 'a t

val atom : PredSymbol.t Located.t -> 'a Located.t list -> 'a t
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val ffn_clash : 'a t -> ffns:ForeignFunc.t PredSymbol.Map.t -> 'a t Logger.t
val set_foreign_func : 'a t -> ffns:ForeignFunc.t PredSymbol.Map.t -> 'a t
