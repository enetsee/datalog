open Lib
open Core
open Reporting

(** Atomic formula 
  This type is polymorphic in the type of terms so we can statically restrict
  a sentence to contain e.g. only variables.
*)
type 'a t =
  { predSym : Core.PredSymbol.t Located.t
  ; terms : 'a Located.t list
  ; nature : Core.ForeignFunc.t option [@compare.ignore]
  }

include Pretty.S1 with type 'a t := 'a t
include Functor.S1 with type 'a t := 'a t

(** Support querying of term-variables contained in the atomic formula *)
include HasTmvars.S1 with type 'a t := 'a t

(** Support translation to a `Core` `Literal`; also requires that the 
    type of `terms` be translatable to `Core` `Term`s.  *)
include
  Translatable.S1
    with type 'a t := 'a t
     and type subelem := Term.t
     and type elem := Region.t Literal.t

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Construct an atom ignoring it's nature *)
val atom : PredSymbol.t Located.t -> 'a Located.t list -> 'a t

(** Test if the predicate symbol clases with a foreign function name *)
val ffn_clash
  :  'a t
  -> ffns:ForeignFunc.t PredSymbol.Map.t
  -> ('a t, State.t, Err.t) Logger.t

(** Modify the atom's nature if the predicate symbol is used for a foreign 
   function *)
val set_foreign_func : 'a t -> ffns:ForeignFunc.t PredSymbol.Map.t -> 'a t
