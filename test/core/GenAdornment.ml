open Core
open Core_kernel
open Reporting

let testable_adorned_program = Adorned.Program.(Alcotest.testable pp equal)

let testable_adornment_error =
  let pp =
    Fmt.(
      vbox
      @@ list ~sep:cut
      @@ hbox
      @@ pair ~sep:(any " @ ") BindingPatt.pp Region.pp)
  and eq = List.equal Tuple2.(equal ~eq1:BindingPatt.equal ~eq2:Region.equal) in
  Alcotest.testable pp eq
;;

module ClientServer = struct
  let p_check_client =
    Pred.(logical ~arity:1 @@ Name.from_string "check_client")
  ;;

  let lit_check_client = Raw.Lit.(lit p_check_client Term.[ var "Pass" ])

  let lit_check_client_sym =
    Raw.Lit.(
      lit p_check_client Term.[ sym @@ Symbol.from_string "PASSWORD123" ])
  ;;

  let p_check_server =
    Pred.(logical ~arity:1 @@ Name.from_string "check_server")
  ;;

  let lit_check_server = Raw.Lit.(lit p_check_server Term.[ var "Hash" ])

  let lit_check_server_sym =
    Raw.Lit.(
      lit p_check_server Term.[ sym @@ Symbol.from_string "UEFTU1dPUkQxMjMK" ])
  ;;

  let p_weak = Pred.(logical ~arity:2 @@ Name.from_string "weak")
  let lit_weak = Raw.Lit.(lit p_weak Term.[ var "Pass"; var "Hash" ])
  let p_hash = Pred.(extralogical ~arity:2 @@ Name.from_string "hash")
  let lit_hash = Raw.Lit.(lit p_hash Term.[ var "Pass"; var "Hash" ])
  let cstr_hash = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
  let p_rainbow = Pred.(extralogical ~arity:2 @@ Name.from_string "rainbow")
  let lit_rainbow = Raw.Lit.(lit p_rainbow Term.[ var "Pass"; var "Hash" ])
  let cstr_rainbow = Constraint.(of_list Atomic.[ of_list [ 1 ] ])
  let p_qry1 = Pred.(logical ~arity:0 @@ Name.from_string "query1")
  let lit_qry1 = Raw.Lit.(lit p_qry1 [])
  let p_qry2 = Pred.(logical ~arity:0 @@ Name.from_string "query2")
  let lit_qry2 = Raw.Lit.(lit p_qry2 [])

  let cnstrs_client_server =
    Pred.Map.of_alist_exn [ p_hash, cstr_hash; p_rainbow, cstr_rainbow ]
  ;;

  let cls_client_server =
    Raw.(
      Clause.
        [ clause lit_check_client [ lit_weak ]
        ; clause lit_check_server [ lit_weak ]
        ; clause lit_weak [ lit_hash; lit_rainbow ]
        ; clause lit_qry1 [ lit_check_client_sym ]
        ; clause lit_qry2 [ lit_check_server_sym ]
        ])
  ;;

  let qrys_client_server = [ p_qry1; p_qry2 ]

  let prg_client_server =
    Raw.Program.(
      program ~cstrs:cnstrs_client_server cls_client_server qrys_client_server)
  ;;

  let cls_client_server_adorned =
    Adorned.(
      Clause.
        [ clause
            Lit.(
              from_raw lit_check_client ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Bound; Free ])
              ]
        ; clause
            Lit.(
              from_raw lit_check_server ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Free; Bound ])
              ]
        ; clause
            Lit.(
              from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Free; Bound ]))
            Lit.
              [ from_raw
                  lit_rainbow
                  ~bpatt:BindingPatt.(from_list [ Free; Bound ])
              ; from_raw
                  lit_hash
                  ~bpatt:BindingPatt.(from_list [ Bound; Bound ])
              ]
        ; clause
            Lit.(
              from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Bound; Free ]))
            Lit.
              [ from_raw lit_hash ~bpatt:BindingPatt.(from_list [ Bound; Free ])
              ; from_raw
                  lit_rainbow
                  ~bpatt:BindingPatt.(from_list [ Bound; Bound ])
              ]
        ; clause
            Lit.(from_raw lit_qry1 ~bpatt:BindingPatt.(from_list []))
            Lit.
              [ from_raw
                  lit_check_client_sym
                  ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_qry2 ~bpatt:BindingPatt.(from_list []))
            Lit.
              [ from_raw
                  lit_check_server_sym
                  ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ])
  ;;

  let prg_client_server_adorned =
    Adorned.Program.(
      sorted
      @@ program
           ~cstrs:cnstrs_client_server
           cls_client_server_adorned
           qrys_client_server)
  ;;

  let client_server () =
    Alcotest.(check @@ result testable_adorned_program testable_adornment_error)
      "Client/server generalized adornment example"
      (Ok prg_client_server_adorned)
      GenAdorn.(Result.map ~f:Adorned.Program.sorted @@ apply prg_client_server)
  ;;
end

module Negation = struct
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

  let cls_raw =
    Raw.Clause.
      [ clause lit_s [ lit_b; Raw.Lit.neg lit_a ]
      ; clause lit_t [ lit_c; Raw.Lit.neg lit_a ]
      ; clause lit_u [ lit_d; Raw.Lit.neg lit_t ]
      ; clause lit_v [ lit_e; Raw.Lit.neg lit_s; Raw.Lit.neg lit_u ]
      ; clause lit_qry [ lit_v ]
      ]
  ;;

  let cls_adorned =
    Adorned.(
      Clause.
        [ clause
            Lit.(from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_b ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_a ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_c ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_a ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_u ~bpatt:BindingPatt.(from_list [ Bound ]))
            Lit.
              [ from_raw lit_d ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_t ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_v ~bpatt:BindingPatt.(from_list [ Free ]))
            Lit.
              [ from_raw lit_e ~bpatt:BindingPatt.(from_list [ Free ])
              ; neg @@ from_raw lit_s ~bpatt:BindingPatt.(from_list [ Bound ])
              ; neg @@ from_raw lit_u ~bpatt:BindingPatt.(from_list [ Bound ])
              ]
        ; clause
            Lit.(from_raw lit_qry ~bpatt:BindingPatt.(from_list []))
            Lit.[ from_raw lit_v ~bpatt:BindingPatt.(from_list [ Free ]) ]
        ])
  ;;

  let prg_raw = Raw.Program.(sorted @@ program cls_raw [ pr_qry ])
  let prg_adorned = Adorned.Program.(sorted @@ program cls_adorned [ pr_qry ])

  let negation () =
    Alcotest.(check @@ result testable_adorned_program testable_adornment_error)
      "Negation generalized adornment example"
      (Ok prg_adorned)
      GenAdorn.(Result.map ~f:Adorned.Program.sorted @@ apply prg_raw)
  ;;
end

module Complement = struct
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

  let cls_raw =
    Raw.(
      Clause.
        [ clause lit_t_head [ lit_g1 ]
        ; clause lit_t_head [ lit_g2; lit_t_body ]
        ; clause lit_ct Lit.[ neg lit_t_head; lit_n1; lit_n2 ]
        ])
  ;;

  let bb = BindingPatt.(from_list [ Bound; Bound ])
  let ff = BindingPatt.(from_list [ Free; Free ])
  let bf = BindingPatt.(from_list [ Bound; Free ])
  let f = BindingPatt.(from_list [ Free ])

  let cls_adorned =
    Adorned.(
      Clause.
        [ clause
            Lit.(from_raw lit_t_head ~bpatt:bb)
            Lit.[ from_raw lit_g1 ~bpatt:bb ]
        ; clause
            Lit.(from_raw lit_t_head ~bpatt:bb)
            Lit.[ from_raw lit_g2 ~bpatt:bf; from_raw lit_t_body ~bpatt:bb ]
        ; clause
            Lit.(from_raw lit_ct ~bpatt:ff)
            Lit.
              [ from_raw lit_n1 ~bpatt:f
              ; from_raw lit_n2 ~bpatt:f
              ; neg @@ from_raw lit_t_head ~bpatt:bb
              ]
        ])
  ;;

  let prg_raw = Raw.Program.program cls_raw [ pr_ct ]
  let prg_adrn = Adorned.Program.(sorted @@ program cls_adorned [ pr_ct ])

  let complement () =
    Alcotest.(check @@ result testable_adorned_program testable_adornment_error)
      "Complement of transitive closure"
      (Ok prg_adrn)
      GenAdorn.(Result.map ~f:Adorned.Program.sorted @@ apply prg_raw)
  ;;
end

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case
        "Client/server generalized adornment example"
        `Quick
        ClientServer.client_server
    ; test_case
        "Negation generalized adornment example"
        `Quick
        Negation.negation
    ; test_case "Complement of transitive closure" `Quick Complement.complement
    ]
;;
