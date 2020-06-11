open Core

let testable_pred = Pred.(Alcotest.testable pp equal)

module Neg = struct
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
  let pr_w = Pred.(logical ~arity:1 @@ Name.from_string "w")
  let lit_w = mk_lit pr_w
  let pr_qry = Pred.(logical ~arity:0 @@ Name.from_string "query")
  let lit_qry = Raw.Lit.lit pr_qry []

  let cls =
    Raw.Clause.
      [ clause lit_s [ lit_b; Raw.Lit.neg lit_a ]
      ; clause lit_t [ lit_c; Raw.Lit.neg lit_a ]
      ; clause lit_u [ lit_d; Raw.Lit.neg lit_t ]
      ; clause lit_v [ lit_e; Raw.Lit.neg lit_s; Raw.Lit.neg lit_u ]
      ; clause lit_qry [ lit_v ]
      ]
  ;;

  (**  

    s(X) :- b(X), not a(X).
    t(X) :- c(X), not a(X).
    u(X) :- d(X), not t(X).
    v(X) :- e(X), not s(X), not u(X).
    query() :- v(X).
      
    query() 
      ^
      |
     +ve   o- -ve - s()
      |    |
     v() <-|
           |
           o- -ve - u()
  
  *)
  let prg = Raw.Program.(sorted @@ program cls [ pr_qry ])

  let deps = Raw.Dependency.from_program prg

  let pos_deps_query () =
    Alcotest.(check @@ list testable_pred)
      "`query` predicate has no outgoing positive edges"
      []
      Raw.Dependency.(pos_deps_of deps pr_qry)
  ;;

  let neg_deps_query () =
    Alcotest.(check @@ list testable_pred)
      "`query` predicate has no outgoing negative edges"
      []
      Raw.Dependency.(neg_deps_of deps pr_qry)
  ;;

  let pos_deps_v () =
    Alcotest.(check @@ list testable_pred)
      "`v` predicate has one outgoing positive edges"
      [ pr_qry ]
      Raw.Dependency.(pos_deps_of deps pr_v)
  ;;

  let neg_deps_v () =
    Alcotest.(check @@ list testable_pred)
      "`v` predicate has no outgoing negative edges"
      []
      Raw.Dependency.(neg_deps_of deps pr_v)
  ;;

  let neg_deps_s () =
    Alcotest.(check @@ list testable_pred)
      "`s` predicate has one outgoing negative edges"
      [ pr_v ]
      Raw.Dependency.(neg_deps_of deps pr_s)
  ;;

  let neg_deps_u () =
    Alcotest.(check @@ list testable_pred)
      "`u` predicate has one outgoing negative edges"
      [ pr_v ]
      Raw.Dependency.(neg_deps_of deps pr_u)
  ;;

  let pos_deps_s () =
    Alcotest.(check @@ list testable_pred)
      "`s` predicate has no outgoing positive edges"
      []
      Raw.Dependency.(pos_deps_of deps pr_s)
  ;;

  let pos_deps_u () =
    Alcotest.(check @@ list testable_pred)
      "`u` predicate has no outgoing positive edges"
      []
      Raw.Dependency.(pos_deps_of deps pr_u)
  ;;
end

module Comp = struct
  let pr_n = Pred.(logical ~arity:1 @@ Name.from_string "n")
  let lit_n1 = Raw.Lit.(lit pr_n Term.[ var "X" ])
  let lit_n2 = Raw.Lit.(lit pr_n Term.[ var "Y" ])
  let pr_g = Pred.(logical ~arity:2 @@ Name.from_string "g")
  let lit_g1 = Raw.Lit.(lit pr_g Term.[ var "X"; var "Y" ])
  let lit_g2 = Raw.Lit.(lit pr_g Term.[ var "X"; var "Z" ])
  let pr_t = Pred.(logical ~arity:2 @@ Name.from_string "t")
  let lit_t_head = Raw.Lit.(lit pr_t Term.[ var "X"; var "Y" ])
  let lit_t_body = Raw.Lit.(lit pr_t Term.[ var "Z"; var "Y" ])
  let pr_ct = Pred.(logical ~arity:2 @@ Name.from_string "ct")
  let lit_ct = Raw.Lit.(lit pr_ct Term.[ var "X"; var "Y" ])
  let pr_qry = Pred.(logical ~arity:0 @@ Name.from_string "qry")
  let lit_qry = Raw.Lit.(lit pr_qry [])

  let cls =
    Raw.(
      Clause.
        [ clause lit_t_head [ lit_g1 ]
        ; clause lit_t_head [ lit_g2; lit_t_body ]
        ; clause lit_ct Lit.[ lit_n1; lit_n2; neg lit_t_head ]
        ])
  ;;

  (** 
    t(X,Y) :- g(X,Y).
    t(X,Y) :- g(X,Z), t(Z,Y).
    ct(X,Y) :- n(X), n(Y), not t(X,Y).

    ct () <- -ve -- t () <---o
                     |       |
                     o- +ve -o
  *)
  let prg = Raw.Program.program cls [ pr_ct ]

  let deps = Raw.Dependency.from_program prg

  let neg_deps_ct () =
    Alcotest.(check @@ list testable_pred)
      "`ct` predicate has no outgoing negative edges"
      []
      Raw.Dependency.(neg_deps_of deps pr_ct)
  ;;

  let neg_deps_t () =
    Alcotest.(check @@ list testable_pred)
      "`t` predicate has one outgoing negative edges"
      [ pr_ct ]
      Raw.Dependency.(neg_deps_of deps pr_t)
  ;;

  let pos_deps_ct () =
    Alcotest.(check @@ list testable_pred)
      "`ct` predicate has no outgoing positive edges"
      []
      Raw.Dependency.(pos_deps_of deps pr_ct)
  ;;

  let pos_deps_t () =
    Alcotest.(check @@ list testable_pred)
      "`t` predicate has one outgoing positive edges"
      [ pr_t ]
      Raw.Dependency.(pos_deps_of deps pr_t)
  ;;
end

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case
        "Neg: `query` predicate has no outgoing positive edges"
        `Quick
        Neg.pos_deps_query
    ; test_case
        "Neg: `query` predicate has no outgoing negative edges"
        `Quick
        Neg.neg_deps_query
    ; test_case
        "Neg: `v` predicate has one outgoing positive edges"
        `Quick
        Neg.pos_deps_v
    ; test_case
        "Neg: `v` predicate has no outgoing negative edges"
        `Quick
        Neg.neg_deps_v
    ; test_case
        "Neg: `s` predicate has one outgoing negative edges"
        `Quick
        Neg.neg_deps_s
    ; test_case
        "Neg: `u` predicate has one outgoing negative edges"
        `Quick
        Neg.neg_deps_u
    ; test_case
        "Neg: `s` predicate has no outgoing positive edges"
        `Quick
        Neg.pos_deps_s
    ; test_case
        "Neg: `u` predicate has no outgoing positive edges"
        `Quick
        Neg.pos_deps_u
    ; test_case
        "Comp: `ct` predicate has no outgoing negative edges"
        `Quick
        Comp.neg_deps_ct
    ; test_case
        "Comp: `t` predicate has one outgoing negative edges"
        `Quick
        Comp.neg_deps_t
    ; test_case
        "Comp: `ct` predicate has no outgoing positive edges"
        `Quick
        Comp.pos_deps_ct
    ; test_case
        "Comp: `t` predicate has one outgoing positive edges"
        `Quick
        Comp.pos_deps_t
    ]
;;
