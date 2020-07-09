open Lib
module Make (State : Tycon.S0) = StateT.Make_with_state (State) (Identity)
include StateT.Make (Identity)
