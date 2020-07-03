open Core
open Core_kernel

let output = Alcotest.(result Testable.adorned_program Testable.err)

module ClientServer = struct
  let p_check_client = Pred.(pred ~arity:1 @@ Name.from_string "check_client")
  let lit_check_client = Lit.Raw.(lit p_check_client Term.[ var "Pass" ])

  let lit_check_client_sym =
    Lit.Raw.(
      lit p_check_client Term.[ sym @@ Symbol.from_string "PASSWORD123" ])
  ;;

  let p_check_server = Pred.(pred ~arity:1 @@ Name.from_string "check_server")
  let lit_check_server = Lit.Raw.(lit p_check_server Term.[ var "Hash" ])

  let lit_check_server_sym =
    Lit.Raw.(
      lit p_check_server Term.[ sym @@ Symbol.from_string "UEFTU1dPUkQxMjMK" ])
  ;;

  let p_weak = Pred.(pred ~arity:2 @@ Name.from_string "weak")
  let lit_weak = Lit.Raw.(lit p_weak Term.[ var "Pass"; var "Hash" ])

  (** Extrapred `hash` predicate *)
  let p_hash = Pred.(pred ~arity:2 @@ Name.from_string "hash")

  let ti_hash =
    TypingEnv.
      { typing = Typing.of_schema Ty.[ Symbol; Symbol ]
      ; cstr = Constraint.(of_list Atomic.[ of_list [ 0 ] ])
      ; nature = Nature.Extralogical []
      }
  ;;

  let lit_hash = Lit.Raw.(lit p_hash Term.[ var "Pass"; var "Hash" ])
  let p_rainbow = Pred.(pred ~arity:2 @@ Name.from_string "rainbow")

  let ti_rainbow =
    TypingEnv.
      { typing = Typing.of_schema Ty.[ Symbol; Symbol ]
      ; cstr = Constraint.(of_list Atomic.[ of_list [ 1 ] ])
      ; nature = Nature.Extralogical []
      }
  ;;

  let lit_rainbow = Lit.Raw.(lit p_rainbow Term.[ var "Pass"; var "Hash" ])
  let p_qry1 = Pred.(pred ~arity:0 @@ Name.from_string "query1")
  let lit_qry1 = Lit.Raw.(lit p_qry1 [])
  let p_qry2 = Pred.(pred ~arity:0 @@ Name.from_string "query2")
  let lit_qry2 = Lit.Raw.(lit p_qry2 [])

  let tyenv_client_server =
    TypingEnv.(
      add_pred ~name:Pred.(name_of p_rainbow) ~info:ti_rainbow
      @@ add_pred ~name:Pred.(name_of p_hash) ~info:ti_hash
      @@ empty)
  ;;

  let cls_client_server =
    Clause.Raw.
      [ clause lit_check_client [ lit_weak ]
      ; clause lit_check_server [ lit_weak ]
      ; clause lit_weak [ lit_hash; lit_rainbow ]
      ; clause lit_qry1 [ lit_check_client_sym ]
      ; clause lit_qry2 [ lit_check_server_sym ]
      ]
  ;;

  let qrys_client_server = [ p_qry1; p_qry2 ]

  let prg_client_server =
    Program.Raw.(program cls_client_server qrys_client_server [] [])
  ;;

  let cls_client_server_adorned =
    Clause.Adorned.
      [ clause
          Lit.Adorned.(
            from_raw lit_check_client ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_weak ~bpatt:Binding.(from_list [ Bound; Free ]) ]
      ; clause
          Lit.Adorned.(
            from_raw lit_check_server ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_weak ~bpatt:Binding.(from_list [ Free; Bound ]) ]
      ; clause
          Lit.Adorned.(
            from_raw lit_weak ~bpatt:Binding.(from_list [ Free; Bound ]))
          Lit.Adorned.
            [ from_raw lit_rainbow ~bpatt:Binding.(from_list [ Free; Bound ])
            ; from_raw lit_hash ~bpatt:Binding.(from_list [ Bound; Bound ])
            ]
      ; clause
          Lit.Adorned.(
            from_raw lit_weak ~bpatt:Binding.(from_list [ Bound; Free ]))
          Lit.Adorned.
            [ from_raw lit_hash ~bpatt:Binding.(from_list [ Bound; Free ])
            ; from_raw lit_rainbow ~bpatt:Binding.(from_list [ Bound; Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_qry1 ~bpatt:Binding.(from_list []))
          Lit.Adorned.
            [ from_raw lit_check_client_sym ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_qry2 ~bpatt:Binding.(from_list []))
          Lit.Adorned.
            [ from_raw lit_check_server_sym ~bpatt:Binding.(from_list [ Bound ])
            ]
      ]
  ;;

  let prg_client_server_adorned =
    Program.Adorned.(
      sorted @@ program cls_client_server_adorned qrys_client_server [] [])
  ;;

  let client_server () =
    Alcotest.(check output)
      "Client/server generalized adornment example"
      (Ok prg_client_server_adorned)
      MonadCompile.(
        eval ~st:State.{ default with typing_env = tyenv_client_server }
        @@ map ~f:Program.Adorned.sorted
        @@ Adorn.adorn_program prg_client_server)
  ;;
end

module Negation = struct
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

  let cls_raw =
    Clause.Raw.
      [ clause lit_s [ lit_b; Lit.Raw.neg lit_a ]
      ; clause lit_t [ lit_c; Lit.Raw.neg lit_a ]
      ; clause lit_u [ lit_d; Lit.Raw.neg lit_t ]
      ; clause lit_v [ lit_e; Lit.Raw.neg lit_s; Lit.Raw.neg lit_u ]
      ; clause lit_qry [ lit_v ]
      ]
  ;;

  let cls_adorned =
    Clause.Adorned.
      [ clause
          Lit.Adorned.(from_raw lit_s ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_b ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_t ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_c ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_u ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_d ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_t ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_v ~bpatt:Binding.(from_list [ Free ]))
          Lit.Adorned.
            [ from_raw lit_e ~bpatt:Binding.(from_list [ Free ])
            ; neg @@ from_raw lit_s ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_u ~bpatt:Binding.(from_list [ Bound ])
            ]
      ; clause
          Lit.Adorned.(from_raw lit_qry ~bpatt:Binding.(from_list []))
          Lit.Adorned.[ from_raw lit_v ~bpatt:Binding.(from_list [ Free ]) ]
      ]
  ;;

  let prg_raw = Program.Raw.(sorted @@ program cls_raw [ pr_qry ] [] [])

  let prg_adorned =
    Program.Adorned.(sorted @@ program cls_adorned [ pr_qry ] [] [])
  ;;

  let negation () =
    Alcotest.(check output)
      "Negation generalized adornment example"
      (Ok prg_adorned)
      MonadCompile.(
        eval @@ map ~f:Program.Adorned.sorted @@ Adorn.adorn_program prg_raw)
  ;;
end

module Complement = struct
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

  let cls_raw =
    Clause.Raw.
      [ clause lit_t_head [ lit_g1 ]
      ; clause lit_t_head [ lit_g2; lit_t_body ]
      ; clause lit_ct Lit.Raw.[ neg lit_t_head; lit_n1; lit_n2 ]
      ]
  ;;

  let bb = Binding.(from_list [ Bound; Bound ])
  let ff = Binding.(from_list [ Free; Free ])
  let bf = Binding.(from_list [ Bound; Free ])
  let f = Binding.(from_list [ Free ])

  let cls_adorned =
    Clause.Adorned.
      [ clause
          Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
          Lit.Adorned.[ from_raw lit_g1 ~bpatt:bb ]
      ; clause
          Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
          Lit.Adorned.
            [ from_raw lit_g2 ~bpatt:bf; from_raw lit_t_body ~bpatt:bb ]
      ; clause
          Lit.Adorned.(from_raw lit_ct ~bpatt:ff)
          Lit.Adorned.
            [ from_raw lit_n1 ~bpatt:f
            ; from_raw lit_n2 ~bpatt:f
            ; neg @@ from_raw lit_t_head ~bpatt:bb
            ]
      ]
  ;;

  let prg_raw = Program.Raw.program cls_raw [ pr_ct ] [] []
  let prg_adrn = Program.Adorned.(sorted @@ program cls_adorned [ pr_ct ] [] [])

  let complement () =
    Alcotest.(check output)
      "Complement of transitive closure"
      (Ok prg_adrn)
      MonadCompile.(
        eval @@ map ~f:Program.Adorned.sorted @@ Adorn.adorn_program prg_raw)
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
