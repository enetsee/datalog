open Core_kernel

let partition = Partition.(Alcotest.testable pp equal)
let a = Partition.of_list Int.Set.[ of_list [ 0; 1 ] ]
let b = Partition.of_list Int.Set.[ singleton 0; singleton 1 ]
let one = Partition.of_list Int.Set.[ of_list [ 1; 2; 3; 4 ] ]
let two_a = Partition.of_list Int.Set.[ of_list [ 1; 2; 3 ]; of_list [ 4 ] ]
let two_b = Partition.of_list Int.Set.[ of_list [ 1; 3; 4 ]; of_list [ 2 ] ]
let two_c = Partition.of_list Int.Set.[ of_list [ 1; 3 ]; of_list [ 2; 4 ] ]
let two_d = Partition.of_list Int.Set.[ of_list [ 1; 4 ]; of_list [ 2; 3 ] ]
let two_e = Partition.of_list Int.Set.[ of_list [ 1; 2 ]; of_list [ 3; 4 ] ]
let two_f = Partition.of_list Int.Set.[ of_list [ 1 ]; of_list [ 2; 3; 4 ] ]
let two_g = Partition.of_list Int.Set.[ of_list [ 3 ]; of_list [ 1; 2; 4 ] ]

let three_a =
  Partition.of_list Int.Set.[ of_list [ 1; 3 ]; of_list [ 2 ]; of_list [ 4 ] ]
;;

let three_b =
  Partition.of_list Int.Set.[ of_list [ 1 ]; of_list [ 2; 3 ]; of_list [ 4 ] ]
;;

let three_c =
  Partition.of_list Int.Set.[ of_list [ 1 ]; of_list [ 2 ]; of_list [ 3; 4 ] ]
;;

let three_d =
  Partition.of_list Int.Set.[ of_list [ 1; 2 ]; of_list [ 3 ]; of_list [ 4 ] ]
;;

let three_e =
  Partition.of_list Int.Set.[ of_list [ 1; 4 ]; of_list [ 2 ]; of_list [ 3 ] ]
;;

let three_f =
  Partition.of_list Int.Set.[ of_list [ 1 ]; of_list [ 3 ]; of_list [ 2; 4 ] ]
;;

let four =
  Partition.of_list
    Int.Set.[ of_list [ 1 ]; of_list [ 2 ]; of_list [ 3 ]; of_list [ 4 ] ]
;;

let two = [ two_a; two_b; two_c; two_d; two_e; two_f; two_g ]
let three = [ three_a; three_b; three_c; three_d; three_e; three_f ]

let cross_neq xs =
  List.(
    xs >>= fun x -> xs >>= fun y -> if Partition.equal x y then [] else [ x, y ])
;;

(* -- All cases ------------------------------------------------------------- *)
let make_finer_than x ~than =
  let msg =
    Fmt.(
      to_to_string @@ hovbox @@ pair ~sep:(any " <| ") Partition.pp Partition.pp)
      (x, than)
  in
  let f () = Alcotest.(check bool) msg true Partition.(is_finer x ~than) in
  Alcotest.test_case msg `Quick f
;;

let make_not_finer_than x ~than =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any " /<| ") Partition.pp Partition.pp)
      (x, than)
  in
  let f () = Alcotest.(check bool) msg false Partition.(is_finer x ~than) in
  Alcotest.test_case msg `Quick f
;;

let mk_join expect x y =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any " === ") Partition.pp
      @@ pair ~sep:(any " \\/ ") Partition.pp Partition.pp)
      (expect, (x, y))
  in
  let f () = Alcotest.(check partition) msg expect Partition.(join x y) in
  Alcotest.test_case msg `Quick f
;;

let mk_meet expect x y =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any " === ") Partition.pp
      @@ pair ~sep:(any " /\\ ") Partition.pp Partition.pp)
      (expect, (x, y))
  in
  let f () = Alcotest.(check partition) msg expect Partition.(meet x y) in
  Alcotest.test_case msg `Quick f
;;

let mk_constrain expect cstrs x =
  let pp_cstr ppf (i, j) =
    Fmt.(prefix (any "sig") @@ braces @@ pair ~sep:(any " ~ ") int int)
      ppf
      (i, j)
  in
  let msg =
    Fmt.(
      to_to_string
      @@ pair ~sep:(any " === ") Partition.pp
      @@ pair (list pp_cstr)
      @@ parens Partition.pp)
      (expect, (cstrs, x))
  in
  let actual =
    List.fold_left cstrs ~init:x ~f:(fun accu cstr ->
        Partition.update_exn cstr accu)
  in
  let f () = Alcotest.(check partition) msg expect actual in
  Alcotest.test_case msg `Quick f
;;

(** Meet of two partitions is finer than both the operands *)
let meet_is_finer () =
  Alcotest.(check bool)
    "Meet of two partitions is finer than both the operands"
    true
    Partition.(
      let m = meet two_a two_b in
      is_finer m ~than:two_a && is_finer m ~than:two_b)
;;

let test_cases =
  List.concat
    [ [ mk_meet b a b; mk_join a a b ]
    ; List.map ~f:(make_finer_than ~than:one) two
    ; List.map ~f:(make_finer_than ~than:one) three
    ; List.map ~f:(fun than -> make_finer_than four ~than) three
    ; List.map ~f:(fun than -> make_finer_than four ~than) two
    ; [ make_finer_than four ~than:one ]
    ; List.map ~f:(fun itself -> make_finer_than itself ~than:itself) two
    ; List.map ~f:(fun itself -> make_finer_than itself ~than:itself) three
    ; List.map ~f:(fun (x, than) -> make_not_finer_than x ~than)
      @@ cross_neq two
    ; List.map ~f:(fun (x, than) -> make_not_finer_than x ~than)
      @@ cross_neq three
    ; List.map ~f:(fun (x, y) -> mk_join one x y) @@ cross_neq two
    ; List.map ~f:(fun (x, y) -> mk_meet four x y) @@ cross_neq three
    ; [ mk_join two_a three_a three_b
      ; mk_join two_b three_a three_c
      ; mk_join two_a three_a three_d
      ; mk_join two_b three_a three_e
      ; mk_join two_c three_a three_f
      ; mk_join two_f three_b three_c
      ; mk_join two_a three_b three_d
      ; mk_join two_d three_b three_e
      ; mk_join two_f three_b three_f
      ; mk_join two_e three_c three_d
      ; mk_join two_b three_c three_e
      ; mk_join two_f three_c three_f
      ; mk_join two_g three_d three_e
      ; mk_join two_g three_d three_f
      ; mk_join two_g three_e three_f
      ; mk_meet three_a two_a two_b
      ; mk_meet three_a two_a two_c
      ; mk_meet three_b two_a two_d
      ; mk_meet three_d two_a two_e
      ; mk_meet three_b two_a two_f
      ; mk_meet three_d two_a two_g
      ; mk_meet three_a two_b two_c
      ; mk_meet three_e two_b two_d
      ; mk_meet three_c two_b two_e
      ; mk_meet three_c two_b two_f
      ; mk_meet three_e two_b two_g
      ; mk_meet four two_c two_d
      ; mk_meet four two_c two_e
      ; mk_meet three_f two_c two_f
      ; mk_meet three_f two_c two_g
      ; mk_meet four two_d two_e
      ; mk_meet three_b two_d two_f
      ; mk_meet three_e two_d two_g
      ; mk_meet three_c two_e two_f
      ; mk_meet three_d two_e two_g
      ; mk_meet three_f two_f two_g
      ; mk_constrain three_a [ 1, 3 ] four
      ; mk_constrain three_b [ 2, 3 ] four
      ; mk_constrain three_c [ 3, 4 ] four
      ; mk_constrain three_d [ 1, 2 ] four
      ; mk_constrain three_e [ 1, 4 ] four
      ; mk_constrain three_f [ 2, 4 ] four
      ; mk_constrain one [ 1, 2; 2, 3; 3, 4 ] four
      ]
    ; Alcotest.
        [ test_case
            "Meet of two partitions is finer than both the operands"
            `Quick
            meet_is_finer
        ]
    ]
;;
