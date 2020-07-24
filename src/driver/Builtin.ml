open Core_kernel
open Core

(* == Relational operators ================================================== *)

(* -- Equality -------------------------------------------------------------- *)

let name_eq = Name.from_string "eq"

let typing_eq =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Top; Top ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ti_eq =
  Core.(
    TypingEnv.
      { typing = typing_eq
      ; nature = Nature.Extralogical []
      ; cstr = Constraint.(of_list Atomic.[ of_list [ 0 ]; of_list [ 1 ] ])
      })
;;

(* -- LT -------------------------------------------------------------------- *)

let name_lt = Name.from_string "lt"

let typing_lt =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ti_lt =
  Core.(
    TypingEnv.
      { typing = typing_eq
      ; nature = Nature.Extralogical []
      ; cstr = Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
      })
;;

(* -- GT -------------------------------------------------------------------- *)

let name_gt = Name.from_string "gt"

let typing_gt =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ti_gt =
  Core.(
    TypingEnv.
      { typing = typing_gt
      ; nature = Nature.Extralogical []
      ; cstr = Constraint.(of_list Atomic.[ of_list [ 0; 1 ] ])
      })
;;

(* == Arithmetic operators ================================================== *)

let name_mult = Name.from_string "mult"

let typing_mult =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1; 2 ])
;;

let ti_mult =
  Core.(
    TypingEnv.
      { typing = typing_mult
      ; nature = Nature.Extralogical []
      ; cstr =
          Constraint.(
            of_list
              Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ]; of_list [ 1; 2 ] ])
      })
;;

let name_div = Name.from_string "div"

let typing_div =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1; 2 ])
;;

let ti_div =
  Core.(
    TypingEnv.
      { typing = typing_div
      ; nature = Nature.Extralogical []
      ; cstr =
          Constraint.(
            of_list
              Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ]; of_list [ 1; 2 ] ])
      })
;;

let name_sub = Name.from_string "sub"

let typing_sub =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1; 2 ])
;;

let ti_sub =
  Core.(
    TypingEnv.
      { typing = typing_sub
      ; nature = Nature.Extralogical []
      ; cstr =
          Constraint.(
            of_list
              Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ]; of_list [ 1; 2 ] ])
      })
;;

let name_add = Name.from_string "add"

let typing_add =
  Core.Typing.singleton
  @@ Core.TTC.ttc
       Core.Ty.[ Number; Number; Number ]
       ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1; 2 ])
;;

let ti_add =
  Core.(
    TypingEnv.
      { typing = typing_add
      ; nature = Nature.Extralogical []
      ; cstr =
          Constraint.(
            of_list
              Atomic.[ of_list [ 0; 1 ]; of_list [ 0; 2 ]; of_list [ 1; 2 ] ])
      })
;;

let preds =
  [ name_eq, ti_eq
  ; name_gt, ti_gt
  ; name_lt, ti_lt
  ; name_mult, ti_mult
  ; name_div, ti_div
  ; name_sub, ti_sub
  ; name_add, ti_add
  ]
;;

let typing_env = TypingEnv.(add_preds ~preds empty)
