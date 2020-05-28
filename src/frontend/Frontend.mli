val raw
  :  [< `Path of string | `String of string ]
  -> (Program.t, State.t, Err.t) Core.Logger.t

val foreignFuncEmbed
  :  [< `Path of string | `String of string ]
  -> (Program.t, State.t, Err.t) Core.Logger.t

val namedQueries
  :  [< `Path of string | `String of string ]
  -> (Program.t, State.t, Err.t) Core.Logger.t

val normalized
  :  [< `Path of string | `String of string ]
  -> (Program.t, State.t, Err.t) Core.Logger.t

val toCore
  :  [< `Path of string | `String of string ]
  -> ( Reporting.Region.t Core.Program.Unstratified.t * 'a Core.Knowledge.t list
     , State.t
     , Err.t )
     Core.Logger.t

val run : ('a, State.t, 'b) Logger.t -> ('a * Warning.t list, 'b) result
val print_result : (Program.t * Warning.t list, Err.t) result -> unit

val print_core_result
  :  ( ('a Core.Program.Unstratified.t * 'b Core.Knowledge.t list)
       * Warning.t list
     , Err.t )
     result
  -> unit
