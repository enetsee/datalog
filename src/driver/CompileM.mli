include
  Effect.MonadRWSFail.S1
    with module Err := Err
     and module Topic := Topic
     and module State := State
     and module Env := Env

include Parser.ParseM.S with type 'a t := 'a t
include Source.SourceM.S with type 'a t := 'a t
include Core.CoreM.S with type 'a t := 'a t
