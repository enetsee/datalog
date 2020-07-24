open Core

type t =
  { fresh_src : int
  ; typing_env : TypingEnv.t
  }
[@@deriving fields]

let default = { fresh_src = 0; typing_env = Builtin.typing_env }
