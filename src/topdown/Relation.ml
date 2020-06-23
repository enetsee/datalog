open Core

type t =
  { attrs : Tmvar.t list
  ; data : Symbol.t Tree.t
  }

let empty attrs = { attrs; data = Tree.leaf Symbol.null }
