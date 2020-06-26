open Core

type t =
  { attrs : Tmvar.t list
  ; data : Symbol.t Tree.t
  }
