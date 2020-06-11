open Core
open Core_kernel

let testable_strat = Stratified.(Alcotest.testable pp equal)

let testable_neg_cycles =
  let equal xs ys =
    let xs' =
      List.sort
        ~compare:Tuple2.(compare ~cmp1:Pred.compare ~cmp2:Pred.compare)
        xs
    and ys' =
      List.sort
        ~compare:Tuple2.(compare ~cmp1:Pred.compare ~cmp2:Pred.compare)
        ys
    in
    List.equal Tuple2.(equal ~eq1:Pred.equal ~eq2:Pred.equal) xs' ys'
  and pp =
    Fmt.(
      vbox @@ list ~sep:cut @@ hbox @@ pair ~sep:(any " => ") Pred.pp Pred.pp)
  in
  Alcotest.testable pp equal
;;

(* == Examples adapted from chapter 15 of 'Foundations of Databases' ======== *)

module Alice = struct
  let vx = Term.var "X"
  let mk_lit pr = Raw.Lit.(lit pr [ vx ])
  let pr_a = Pred.(logical ~arity:1 @@ Name.from_string "a")
  let lit_a = mk_lit pr_a
  let pr_b = Pred.(logical ~arity:1 @@ Name.from_string "b")
  let lit_b = mk_lit pr_b
  let pr_c = Pred.(logical ~arity:1 @@ Name.from_string "c")
  let lit_c = mk_lit pr_c
  let pr_d = Pred.(logical ~arity:1 @@ Name.from_string "d")
  let lit_d = mk_lit pr_d
  let pr_e = Pred.(logical ~arity:1 @@ Name.from_string "e")
  let lit_e = mk_lit pr_e
  let pr_s = Pred.(logical ~arity:1 @@ Name.from_string "s")
  let lit_s = mk_lit pr_s
  let pr_t = Pred.(logical ~arity:1 @@ Name.from_string "t")
  let lit_t = mk_lit pr_t
  let pr_u = Pred.(logical ~arity:1 @@ Name.from_string "u")
  let lit_u = mk_lit pr_u
  let pr_v = Pred.(logical ~arity:1 @@ Name.from_string "v")
  let lit_v = mk_lit pr_v
  let pr_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
  let lit_qry = Raw.Lit.lit pr_qry []

  let cl_s =
    Adorned.(
      Clause.(
        clause
          Lit.(from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ]))
          Lit.
            [ from_raw lit_b ~bpatt:BindingPatt.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:BindingPatt.(from_list [ Bound ])
            ]))
  ;;

  let cl_t =
    Adorned.(
      Clause.(
        clause
          Lit.(from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ]))
          Lit.
            [ from_raw lit_c ~bpatt:BindingPatt.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:BindingPatt.(from_list [ Bound ])
            ]))
  ;;

  let cl_u =
    Adorned.(
      Clause.(
        clause
          Lit.(from_raw lit_u ~bpatt:BindingPatt.(from_list [ Bound ]))
          Lit.
            [ from_raw lit_d ~bpatt:BindingPatt.(from_list [ Bound ])
            ; neg @@ from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ])
            ]))
  ;;

  let cl_v =
    Adorned.(
      Clause.(
        clause
          Lit.(from_raw lit_v ~bpatt:BindingPatt.(from_list [ Free ]))
          Lit.
            [ from_raw lit_e ~bpatt:BindingPatt.(from_list [ Free ])
            ; neg @@ from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ])
            ; neg @@ from_raw lit_u ~bpatt:BindingPatt.(from_list [ Bound ])
            ]))
  ;;

  let cl_qry =
    Adorned.(
      Clause.(
        clause
          Lit.(from_raw lit_qry ~bpatt:BindingPatt.(from_list []))
          Lit.[ from_raw lit_v ~bpatt:BindingPatt.(from_list [ Free ]) ]))
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
  let prg_good = Adorned.Program.(sorted @@ program cls_good queries)

  let prg_good_stratified = Stratified.(sorted { strata; queries })

  let no_cycles () =
    Alcotest.(check @@ result testable_strat testable_neg_cycles)
      "Alice example 15.2.5"
      (Ok prg_good_stratified)
      Compile.(Result.map ~f:Stratified.sorted @@ stratify prg_good)
  ;;

  let cls_bad =
    Adorned.(
      Clause.
        [ clause
            Lit.(from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_a ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_b ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_qry ~bpatt:BindingPatt.(from_list []))
            Lit.[ from_raw lit_t ~bpatt:BindingPatt.(from_list [ Free ]) ]
        ])
  ;;

  (** Example 15.1.1, adapted

    s(X) :- a(X), not t(X).
    t(X) :- b(X), not s(X).
    query() :- t(X).

  *)
  let prg_bad = Adorned.Program.(sorted @@ program cls_bad queries)

  let direct_cycle () =
    Alcotest.(check @@ result testable_strat testable_neg_cycles)
      "Alice example Fig 15.1, direct cycle"
      (Error [ pr_t, pr_s; pr_s, pr_t ])
      Compile.(Result.map ~f:Stratified.sorted @@ stratify prg_bad)
  ;;

  let pr_edge = Pred.(logical ~arity:2 @@ Name.from_string "edge")
  let pr_connected = Pred.(logical ~arity:2 @@ Name.from_string "connected")
  let pr_comp = Pred.(logical ~arity:2 @@ Name.from_string "comp")
end

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case "Alice examples, no cycles" `Quick Alice.no_cycles
    ; test_case "Alice examples, direct cycle" `Quick Alice.direct_cycle
    ]
;;
