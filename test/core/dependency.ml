open Core

let testable_pred = Pred.(Alcotest.testable pp equal)

module Neg = struct
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
  let pr_w = Pred.(pred ~arity:1 @@ Name.from_string "w")
  let lit_w = mk_lit pr_w
  let pr_qry = Pred.(pred ~arity:0 @@ Name.from_string "query")
  let lit_qry = Lit.Raw.lit pr_qry []

  let cls =
    Clause.Raw.
      [ clause lit_s [ lit_b; Lit.Raw.neg lit_a ]
      ; clause lit_t [ lit_c; Lit.Raw.neg lit_a ]
      ; clause lit_u [ lit_d; Lit.Raw.neg lit_t ]
      ; clause lit_v [ lit_e; Lit.Raw.neg lit_s; Lit.Raw.neg lit_u ]
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
  let prg = Program.Raw.(sorted @@ program cls)

  let queries = [ pr_qry ]
  let deps = Dependency.Raw.from_program prg

  let pos_deps_query () =
    Alcotest.(check @@ list testable_pred)
      "`query` predicate has no outgoing positive edges"
      []
      Dependency.Raw.(pos_deps_of deps pr_qry)
  ;;

  let neg_deps_query () =
    Alcotest.(check @@ list testable_pred)
      "`query` predicate has no outgoing negative edges"
      []
      Dependency.Raw.(neg_deps_of deps pr_qry)
  ;;

  let pos_deps_v () =
    Alcotest.(check @@ list testable_pred)
      "`v` predicate has one outgoing positive edges"
      [ pr_qry ]
      Dependency.Raw.(pos_deps_of deps pr_v)
  ;;

  let neg_deps_v () =
    Alcotest.(check @@ list testable_pred)
      "`v` predicate has no outgoing negative edges"
      []
      Dependency.Raw.(neg_deps_of deps pr_v)
  ;;

  let neg_deps_s () =
    Alcotest.(check @@ list testable_pred)
      "`s` predicate has one outgoing negative edges"
      [ pr_v ]
      Dependency.Raw.(neg_deps_of deps pr_s)
  ;;

  let neg_deps_u () =
    Alcotest.(check @@ list testable_pred)
      "`u` predicate has one outgoing negative edges"
      [ pr_v ]
      Dependency.Raw.(neg_deps_of deps pr_u)
  ;;

  let pos_deps_s () =
    Alcotest.(check @@ list testable_pred)
      "`s` predicate has no outgoing positive edges"
      []
      Dependency.Raw.(pos_deps_of deps pr_s)
  ;;

  let pos_deps_u () =
    Alcotest.(check @@ list testable_pred)
      "`u` predicate has no outgoing positive edges"
      []
      Dependency.Raw.(pos_deps_of deps pr_u)
  ;;
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

  let cls =
    Clause.Raw.
      [ clause lit_t_head [ lit_g1 ]
      ; clause lit_t_head [ lit_g2; lit_t_body ]
      ; clause lit_ct Lit.Raw.[ lit_n1; lit_n2; neg lit_t_head ]
      ]
  ;;

  (** 
    t(X,Y) :- g(X,Y).
    t(X,Y) :- g(X,Z), t(Z,Y).
    ct(X,Y) :- n(X), n(Y), not t(X,Y).

    ct () <- -ve -- t () <---o
                     |       |
                     o- +ve -o
  *)
  let prg = Program.Raw.program cls

  let queries = [ pr_ct ]
  let deps = Dependency.Raw.from_program prg

  let neg_deps_ct () =
    Alcotest.(check @@ list testable_pred)
      "`ct` predicate has no outgoing negative edges"
      []
      Dependency.Raw.(neg_deps_of deps pr_ct)
  ;;

  let neg_deps_t () =
    Alcotest.(check @@ list testable_pred)
      "`t` predicate has one outgoing negative edges"
      [ pr_ct ]
      Dependency.Raw.(neg_deps_of deps pr_t)
  ;;

  let pos_deps_ct () =
    Alcotest.(check @@ list testable_pred)
      "`ct` predicate has no outgoing positive edges"
      []
      Dependency.Raw.(pos_deps_of deps pr_ct)
  ;;

  let pos_deps_t () =
    Alcotest.(check @@ list testable_pred)
      "`t` predicate has one outgoing positive edges"
      [ pr_t ]
      Dependency.Raw.(pos_deps_of deps pr_t)
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
