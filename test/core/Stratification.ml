open Core
open Core_kernel

let output = Alcotest.result Testable.stratified_program Testable.err

(* == Examples adapted from chapter 15 of 'Foundations of Databases' ======== *)

module Alice = struct
  let vx = Term.var "X"
  let mk_lit pr = Lit.Raw.(lit pr [ vx ])
  let pr_a = Pred.(pred ~arity:1 @@ Name.from_string "a")
  let lit_a = mk_lit pr_a
  let pr_b = Pred.(pred ~arity:1 @@ Name.from_string "b")
  let lit_b = mk_lit pr_b
  let pr_c = Pred.(pred ~arity:1 @@ Name.from_string "c")
  let lit_c = mk_lit pr_c
  let pr_d = Pred.(pred ~arity:1 @@ Name.from_string "d")
  let lit_d = mk_lit pr_d
  let pr_e = Pred.(pred ~arity:1 @@ Name.from_string "e")
  let lit_e = mk_lit pr_e
  let pr_s = Pred.(pred ~arity:1 @@ Name.from_string "s")
  let lit_s = mk_lit pr_s
  let pr_t = Pred.(pred ~arity:1 @@ Name.from_string "t")
  let lit_t = mk_lit pr_t
  let pr_u = Pred.(pred ~arity:1 @@ Name.from_string "u")
  let lit_u = mk_lit pr_u
  let pr_v = Pred.(pred ~arity:1 @@ Name.from_string "v")
  let lit_v = mk_lit pr_v
  let pr_qry = Pred.(pred ~arity:0 @@ Name.from_string "query")
  let lit_qry = Lit.Raw.lit pr_qry []

  let cl_s =
    Clause.Adorned.(
      clause
        Lit.Adorned.(from_raw lit_s ~bpatt:Binding.(from_list [ Bound ]))
        Lit.Adorned.
          [ from_raw lit_b ~bpatt:Binding.(from_list [ Bound ])
          ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
          ])
  ;;

  let cl_t =
    Clause.Adorned.(
      clause
        Lit.Adorned.(from_raw lit_t ~bpatt:Binding.(from_list [ Bound ]))
        Lit.Adorned.
          [ from_raw lit_c ~bpatt:Binding.(from_list [ Bound ])
          ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
          ])
  ;;

  let cl_u =
    Clause.Adorned.(
      clause
        Lit.Adorned.(from_raw lit_u ~bpatt:Binding.(from_list [ Bound ]))
        Lit.Adorned.
          [ from_raw lit_d ~bpatt:Binding.(from_list [ Bound ])
          ; neg @@ from_raw lit_t ~bpatt:Binding.(from_list [ Bound ])
          ])
  ;;

  let cl_v =
    Clause.Adorned.(
      clause
        Lit.Adorned.(from_raw lit_v ~bpatt:Binding.(from_list [ Free ]))
        Lit.Adorned.
          [ from_raw lit_e ~bpatt:Binding.(from_list [ Free ])
          ; neg @@ from_raw lit_s ~bpatt:Binding.(from_list [ Bound ])
          ; neg @@ from_raw lit_u ~bpatt:Binding.(from_list [ Bound ])
          ])
  ;;

  let cl_qry =
    Clause.Adorned.(
      clause
        Lit.Adorned.(from_raw lit_qry ~bpatt:Binding.(from_list []))
        Lit.Adorned.[ from_raw lit_v ~bpatt:Binding.(from_list [ Free ]) ])
  ;;

  let cls_good = [ cl_s; cl_t; cl_u; cl_v; cl_qry ]
  let strata = [ [ cl_t ]; [ cl_u ]; [ cl_s ]; [ cl_v ]; [ cl_qry ] ]
  let queries = [ pr_qry ]

  (** Example 15.2.5, adapted
  
    s(X) :- b(X), not a(X).
    t(X) :- c(X), not a(X).
    u(X) :- d(x), not t(X).
    v(X) :- e(x), not s(X), not u(X).
    query() :- v(X).
  *)
  let prg_good = Program.Adorned.(sorted @@ program cls_good queries [] [])

  let prg_good_stratified =
    Program.Stratified.(sorted { strata; queries; data = []; params = [] })
  ;;

  let no_cycles () =
    Alcotest.(check output)
      "Alice example 15.2.5"
      (Ok prg_good_stratified)
      MonadCompile.(
        eval @@ map ~f:Program.Stratified.sorted @@ Compile.stratify prg_good)
  ;;

  let cls_bad =
    Clause.Adorned.
      [ clause
          Lit.Adorned.(from_raw lit_s ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_t ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_t ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_b ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_s ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_qry ~bpatt:Binding.(from_list []))
          Lit.Adorned.[ from_raw lit_t ~bpatt:Binding.(from_list [ Free ]) ]
      ]
  ;;

  (** Example 15.1.1, adapted

    s(X) :- a(X), not t(X).
    t(X) :- b(X), not s(X).
    query() :- t(X).

  *)
  let prg_bad = Program.Adorned.(sorted @@ program cls_bad queries [] [])

  let direct_cycle () =
    Alcotest.(check output)
      "Alice example Fig 15.1, direct cycle"
      (Error MonadCompile.Err.(NegativeCycles [ pr_t, pr_s; pr_s, pr_t ]))
      MonadCompile.(
        eval @@ map ~f:Program.Stratified.sorted @@ Compile.stratify prg_bad)
  ;;

  let pr_edge = Pred.(pred ~arity:2 @@ Name.from_string "edge")
  let pr_connected = Pred.(pred ~arity:2 @@ Name.from_string "connected")
  let pr_comp = Pred.(pred ~arity:2 @@ Name.from_string "comp")
end

module Comp = struct
  let pr_n = Pred.(pred ~arity:1 @@ Name.from_string "n")
  let lit_n1 = Lit.Raw.(lit pr_n Term.[ var "X" ])
  let lit_n2 = Lit.Raw.(lit pr_n Term.[ var "Y" ])
  let pr_g = Pred.(pred ~arity:2 @@ Name.from_string "g")
  let lit_g1 = Lit.Raw.(lit pr_g Term.[ var "X"; var "Y" ])
  let lit_g2 = Lit.Raw.(lit pr_g Term.[ var "X"; var "Z" ])
  let pr_t = Pred.(pred ~arity:2 @@ Name.from_string "t")
  let lit_t_head = Lit.Raw.(lit pr_t Term.[ var "X"; var "Y" ])
  let lit_t_body = Lit.Raw.(lit pr_t Term.[ var "Z"; var "Y" ])
  let pr_ct = Pred.(pred ~arity:2 @@ Name.from_string "ct")
  let lit_ct = Lit.Raw.(lit pr_ct Term.[ var "X"; var "Y" ])
  let pr_qry = Pred.(pred ~arity:0 @@ Name.from_string "qry")
  let lit_qry = Lit.Raw.(lit pr_qry [])
  let bb = Binding.(from_list [ Bound; Bound ])
  let ff = Binding.(from_list [ Free; Free ])
  let bf = Binding.(from_list [ Bound; Free ])
  let f = Binding.(from_list [ Free ])

  let cl_t1 =
    Clause.Adorned.clause
      Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
      Lit.Adorned.[ from_raw lit_g1 ~bpatt:bb ]
  ;;

  let cl_t2 =
    Clause.Adorned.clause
      Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
      Lit.Adorned.[ from_raw lit_g2 ~bpatt:bf; from_raw lit_t_body ~bpatt:bb ]
  ;;

  let cl_ct =
    Clause.Adorned.clause
      Lit.Adorned.(from_raw lit_ct ~bpatt:ff)
      Lit.Adorned.
        [ from_raw lit_n1 ~bpatt:f
        ; from_raw lit_n2 ~bpatt:f
        ; neg @@ from_raw lit_t_head ~bpatt:bb
        ]
  ;;

  let cls_adorned = [ cl_t1; cl_t2; cl_ct ]
  let strata = [ [ cl_t1; cl_t2 ]; [ cl_ct ] ]
  let queries = [ pr_ct ]
  let prg_adrn = Program.Adorned.(sorted @@ program cls_adorned queries [] [])

  let prg_strat =
    Program.Stratified.(sorted { strata; queries; params = []; data = [] })
  ;;

  let pos_cycle () =
    Alcotest.(check output)
      "Alice example Pc,cmp, direct cycle"
      (Ok prg_strat)
      MonadCompile.(
        eval @@ map ~f:Program.Stratified.sorted @@ Compile.stratify prg_adrn)
  ;;
end

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Alice examples, no cycles" `Quick Alice.no_cycles
    ; test_case
        "Alice examples, direct negative cycle"
        `Quick
        Alice.direct_cycle
    ; test_case "Alice examples, direct positive cycle" `Quick Comp.pos_cycle
    ]
;;
