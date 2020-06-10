open Core
open Core_kernel
let testable_adorned_program = Adorned.Program.(Alcotest.testable pp equal)
let p_check_client = Pred.(logical ~arity:1 @@ Name.from_string "check_client")
let lit_check_client = Raw.Lit.(lit p_check_client Term.[ var "Pass" ])

let lit_check_client_sym =
  Raw.Lit.(lit p_check_client Term.[ sym @@ Symbol.from_string "PASSWORD123" ])
;;

let p_check_server = Pred.(logical ~arity:1 @@ Name.from_string "check_server")
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

(* -- Client / server  example -------------------------------------------------

----------------------------------------------------------------------------- *)

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
            [ from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Bound; Free ]) ]
      ; clause
          Lit.(
            from_raw lit_check_server ~bpatt:BindingPatt.(from_list [ Bound ]))
          Lit.
            [ from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Free; Bound ]) ]
      ; clause
          Lit.(from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Free; Bound ]))
          Lit.
            [ from_raw
                lit_rainbow
                ~bpatt:BindingPatt.(from_list [ Free; Bound ])
            ; from_raw lit_hash ~bpatt:BindingPatt.(from_list [ Bound; Bound ])
            ]
      ; clause
          Lit.(from_raw lit_weak ~bpatt:BindingPatt.(from_list [ Bound; Free ]))
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
    sorted @@ 
    program
      ~cstrs:cnstrs_client_server
      cls_client_server_adorned
      qrys_client_server)
;;

let client_server () =
  Alcotest.(check @@ option testable_adorned_program)
    "Client/server generalized adornment example"
    (Some prg_client_server_adorned)
    GenAdornment.(Option.map ~f:Adorned.Program.sorted @@ apply prg_client_server)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases =
  Alcotest.
    [ test_case
        "Client/server generalized adornment example"
        `Quick
        client_server
    ]
;;
