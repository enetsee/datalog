open Core_kernel
open Core
open Testable

let ty_cycle = Ty.(named "cycle")
let ty_bicycle = Ty.(named "bicycle")
let ty_unicycle = Ty.(named "unicycle")
let ty_pedals = Ty.(named "pedals")
let ty_saddle = Ty.(named "saddle")
let ty_wheel = Ty.(named "wheel")
let ty_spokes = Ty.(named "spokes")
let ty_tire = Ty.(named "tire")
let ty_tube = Ty.(named "tube")

(* -- Transitive closure ---------------------------------------------------- *)
let subtys =
  Ty.
    [ ty_cycle, Symbol
    ; ty_bicycle, ty_cycle
    ; ty_unicycle, ty_cycle
    ; ty_pedals, Symbol
    ; ty_saddle, Symbol
    ; ty_wheel, Symbol
    ; ty_spokes, Symbol
    ; ty_tire, Symbol
    ; ty_tube, Symbol
    ]
;;

let closure =
  Ty.Map.of_alist_exn
    Ty.
      [ ( Symbol
        , Set.of_list
            [ Symbol
            ; ty_cycle
            ; ty_bicycle
            ; ty_unicycle
            ; ty_pedals
            ; ty_saddle
            ; ty_wheel
            ; ty_spokes
            ; ty_tire
            ; ty_tube
            ] )
      ; ty_cycle, Set.of_list [ ty_cycle; ty_bicycle; ty_unicycle ]
      ; ty_bicycle, Set.singleton ty_bicycle
      ; ty_unicycle, Set.singleton ty_unicycle
      ; ty_pedals, Set.singleton ty_pedals
      ; ty_saddle, Set.singleton ty_saddle
      ; ty_wheel, Set.singleton ty_wheel
      ; ty_spokes, Set.singleton ty_spokes
      ; ty_tire, Set.singleton ty_tire
      ; ty_tube, Set.singleton ty_tube
      ]
;;

(* -- Type meet ------------------------------------------------------------- *)

let ty_transitive_closure_example () =
  Alcotest.(check trg)
    "Transitive closure of subtype relationships"
    closure
    Ty.(transitive_closure subtys)
;;

let mk_ty_meet expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Ty.pp
      @@ pair ~sep:(any "@;/\\@;") Ty.pp Ty.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.(check ty) msg expect Ty.(meet ~trg:closure lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

(* -- TTCs ------------------------------------------------------------------ *)

let ttc_top2 = TTC.top 2

let ttc_bot2_eq =
  TTC.ttc [ Bot; Bot ] ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_a = TTC.ttc [ Symbol; Symbol ]
let ttc_b = TTC.ttc [ ty_bicycle; ty_wheel ]
let ttc_a_b = TTC.ttc [ Symbol; Symbol; ty_bicycle; ty_wheel ]

let ttc_a_eq =
  TTC.ttc
    [ Symbol; Symbol ]
    ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_b_eq =
  TTC.ttc
    [ ty_bicycle; ty_wheel ]
    ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
;;

let ttc_a_b_eq =
  TTC.ttc
    [ Symbol; Symbol; ty_bicycle; ty_wheel ]
    ~equiv:Partition.(of_list Int.Set.[ of_list [ 0; 1 ]; of_list [ 2; 3 ] ])
;;

let mk_ttc_meet expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair ~sep:(any "@;/\\@;") TTC.pp TTC.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.(check ttc) msg expect TTC.(meet ~trg:closure lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

let mk_ttc_product expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair ~sep:(any "@;*@;") TTC.pp TTC.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.(check ttc) msg expect TTC.(product lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

let mk_ttc_project expect flds rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") TTC.pp
      @@ pair (prefix (any "pi") @@ braces @@ list ~sep:comma int)
      @@ parens TTC.pp)
      (expect, (Int.Set.to_list flds, rhs))
  in
  let f () = Alcotest.(check ttc) msg expect TTC.(project rhs ~flds) in
  Alcotest.test_case msg `Quick f
;;

let ttc_meet_helper () =
  Alcotest.(check @@ list ty)
    "Meet helper"
    Ty.[ bottom; bottom ]
    (TTC.meet_helper
       [ ty_bicycle; ty_wheel ]
       ~trg:closure
       ~equiv:Partition.(singleton @@ Int.Set.of_list [ 0; 1 ]))
;;

(* -- Typings --------------------------------------------------------------- *)


let pr_cycle = Pred.(pred ~arity:1 @@ Name.from_string "cycle") 
let pr_unicycle = Pred.(pred ~arity:4 @@ Name.from_string "unicycle")
let pr_bicycle = Pred.(pred ~arity:4 @@ Name.from_string "bicycle" )
let pr_wheel = Pred.(pred ~arity:3 @@ Name.from_string "wheel")
let pr_tire = Pred.(pred ~arity:2 @@ Name.from_string "tire")
let pr_hasPart = Pred.(pred ~arity:2 @@ Name.from_string "hasPart")
let pr_hasPartTC = Pred.(pred ~arity:2 @@ Name.from_string "hasPart+")
let pr_query = Pred.(pred ~arity:2 @@ Name.from_string "query")

let vx,vy,vz = Term.(var "x", var "y", var "z")

let edb = 
  Pred.Map.of_alist_exn
    [ pr_cycle, Typing.of_schema [ ty_cycle ]
    ; pr_unicycle, Typing.of_schema [ ty_unicycle ; ty_saddle ; ty_wheel; ty_pedals]
    ; pr_bicycle , Typing.of_schema [ ty_bicycle; ty_wheel; ty_wheel; ty_pedals]
    ; pr_wheel, Typing.of_schema [ty_wheel; ty_tire; ty_spokes]
    ; pr_tire , Typing.of_schema [ ty_tire; ty_tube ]
    ]

let ff = Binding.(from_list [Free;Free])
let fff = Binding.(from_list [Free;Free;Free])
let ffff = Binding.(from_list [Free;Free;Free;Free])
let lit_hasPart = Lit.Adorned.from_raw ~bpatt:ff @@ Lit.Raw.lit pr_hasPart [vx;vy]
let lit_unicycle_12 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_unicycle Term.[vx;vy;wild();wild()]
let lit_unicycle_13 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_unicycle Term.[vx;wild();vy;wild()]

let lit_unicycle_14 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_unicycle Term.[vx;wild();wild();vy]

let lit_bicycle_12 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_bicycle Term.[vx;vy;wild();wild()]
let lit_bicycle_13 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_bicycle Term.[vx;wild();vy;wild()]

let lit_bicycle_14 = Lit.Adorned.from_raw ~bpatt:ffff @@ Lit.Raw.lit pr_bicycle Term.[vx;wild();wild();vy]

let lit_wheel_12 = Lit.Adorned.from_raw ~bpatt:fff @@ Lit.Raw.lit pr_wheel Term.[vx;vy;wild()]
let lit_wheel_13 = Lit.Adorned.from_raw ~bpatt:fff @@ Lit.Raw.lit pr_wheel Term.[vx;wild();vy]

let lit_tire = Lit.Adorned.from_raw ~bpatt:ff @@ Lit.Raw.lit pr_tire [vx;vy]

let stratum1 = Clause.Adorned.(
  [ clause lit_hasPart [lit_unicycle_12]
  ; clause lit_hasPart [lit_unicycle_13]
  ; clause lit_hasPart [lit_unicycle_14]
  ; clause lit_hasPart [lit_bicycle_12]
  ; clause lit_hasPart [lit_bicycle_13]
  ; clause lit_hasPart [lit_bicycle_14]
  ; clause lit_hasPart [lit_wheel_12]
  ; clause lit_hasPart [lit_wheel_13]
  ; clause lit_hasPart [lit_tire]
  ]
  )

let tys_1 = Typing.type_of_stratum stratum1 ~edb ~trg:closure
let type_of_stratum1 () = 
  Alcotest.(check typing)
  "Type of `hasPart`"
  Typing.empty
  Pred.Map.(find_exn tys_1 pr_hasPart)

let lit_hasPartTC = Lit.Adorned.from_raw ~bpatt:ff @@ Lit.Raw.lit pr_hasPartTC [vx;vy]
let lit_hasPartTCBody = Lit.Adorned.from_raw ~bpatt:ff @@ Lit.Raw.lit pr_hasPartTC [vx;vz]

let lit_hasPartBody = Lit.Adorned.from_raw ~bpatt:ff @@ Lit.Raw.lit pr_hasPart [vz;vy]

let stratum2 = Clause.Adorned.(
[ clause lit_hasPartTC [lit_hasPart]
; clause lit_hasPartTC [lit_hasPartTCBody; lit_hasPartBody]
])

let edb' = Typing.merge edb tys_1 

let tys_2 = Typing.type_of_stratum stratum2 ~edb:edb' ~trg:closure
let type_of_stratum2 () = 
  Alcotest.(check typing)
  "Type of `hasPart+`"
  Typing.empty
  Pred.Map.(find_exn tys_2 pr_hasPartTC)

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case
        "Transitive closure of subtype relationships"
        `Quick
        ty_transitive_closure_example
    ; test_case "meet helper" `Quick ttc_meet_helper
    ; mk_ty_meet ty_bicycle Ty.Symbol ty_bicycle
    ; mk_ty_meet ty_bicycle ty_cycle ty_bicycle
    ; mk_ty_meet Ty.bottom ty_unicycle ty_bicycle
    ; mk_ttc_meet ttc_b ttc_top2 ttc_b
    ; mk_ttc_meet ttc_a ttc_top2 ttc_a
    ; mk_ttc_meet ttc_b ttc_a ttc_b
    ; mk_ttc_meet ttc_b ttc_a_eq ttc_b
    ; mk_ttc_meet ttc_bot2_eq ttc_a_eq ttc_b_eq
    ; mk_ttc_product ttc_a_b ttc_a ttc_b
    ; mk_ttc_product ttc_a_b_eq ttc_a_eq ttc_b_eq
    ; mk_ttc_project ttc_a Int.Set.(of_list [ 0; 1 ]) ttc_a_b
    ; mk_ttc_project ttc_b Int.Set.(of_list [ 2; 3 ]) ttc_a_b
    ; mk_ttc_project ttc_a_eq Int.Set.(of_list [ 0; 1 ]) ttc_a_b_eq
    ; mk_ttc_project ttc_b_eq Int.Set.(of_list [ 2; 3 ]) ttc_a_b_eq
    (* ; test_case
        "Type of stratum 1"
        `Quick
        type_of_stratum1 *)
; test_case
        "Type of stratum 2"
        `Quick
        type_of_stratum2
    ]
;;
