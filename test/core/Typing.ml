open Core_kernel
open Core
open Programs

(* -- MonadTyping ----------------------------------------------------------- *)

module Err = struct
  type t = unit
end

module Topic = struct
  type t = unit

  include Lib.Monoid.Make0 (struct
    type nonrec t = t

    let mempty = ()
    let append t _ = t
  end)
end

module M = struct
  include Effect.MonadRWSError.Make (Err) (Topic) (TypingEnv) (Ty.TRG)

  let subtypes_of ty =
    map ask ~f:(fun trg ->
        Option.value ~default:Ty.Set.empty @@ Ty.TRG.subtypes_of trg ~ty)
  ;;

  let get_typing_of name =
    map get ~f:(fun ty_env ->
        match TypingEnv.find_pred_typing ~name ty_env with
        | Some typing -> typing
        | _ ->
          Option.value_map ~default:Typing.bottom ~f:Typing.singleton
          @@ TypingEnv.find_data ~name ty_env)
  ;;

  let set_typing_of name typing =
    get
    >>= fun ty_env ->
    put @@ TypingEnv.update_pred_typing_exn ty_env ~name ~typing
  ;;
end

module TypingM = Typing.Make (M)

(* -- Test helpers ---------------------------------------------------------- *)

let mk_meet trg expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;/\\@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.(check @@ result Testable.typing unit)
      msg
      (Ok expect)
      M.(eval ~env:trg ~st:TypingEnv.empty @@ TypingM.meet lhs rhs)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_join expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;\\/@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () = Alcotest.check Testable.typing msg expect Typing.(join lhs rhs) in
  Alcotest.test_case msg `Quick f
;;

let mk_product expect lhs rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair ~sep:(any "@;*@;") Typing.pp Typing.pp)
      (expect, (lhs, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(product lhs rhs)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_project expect flds rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair (prefix (any "pi") @@ braces @@ list ~sep:comma int)
      @@ parens Typing.pp)
      (expect, (flds, rhs))
  in
  let f () =
    Alcotest.check Testable.typing msg expect Typing.(project rhs ~flds)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_restrict trg expect equiv rhs =
  let msg =
    Fmt.(
      to_to_string
      @@ hovbox
      @@ pair ~sep:(any "@;===@;") Typing.pp
      @@ hovbox
      @@ pair (prefix (any "sig") @@ parens @@ pair ~sep:(any "@;~@;") int int)
      @@ parens Typing.pp)
      (expect, (equiv, rhs))
  in
  let f () =
    Alcotest.(check @@ result Testable.typing unit)
      msg
      (Ok expect)
      (M.eval ~env:trg ~st:TypingEnv.empty @@ TypingM.restrict rhs ~equiv)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_stratum trg tyenv msg expect clauses =
  let f () =
    Alcotest.(check @@ result Testable.typing unit)
      msg
      (Ok expect)
      M.(
        eval ~env:trg ~st:tyenv
        @@ TypingM.interpret
        @@ Relation.of_clauses clauses)
  in
  Alcotest.test_case msg `Quick f
;;

let mk_program trg tyenv msg expect prog =
  let f () =
    let _, _, st = M.(run ~env:trg ~st:tyenv @@ TypingM.typing_of prog) in
    Alcotest.(check Testable.typing_env) msg expect st
  in
  Alcotest.test_case msg `Quick f
;;

module BikeShop = struct
  open Stratified.BikeShop

  let test_cases =
    [ mk_product typing_hasPart_2 typing_hasPart typing_hasPart
    ; mk_restrict closure typing_hasPart_2_eq_1_2 (1, 2) typing_hasPart_2
    ; mk_project
        typing_hasPart_2_eq_1_2_proj_0_4
        [ 0; 3 ]
        typing_hasPart_2_eq_1_2
    ; mk_stratum closure tyenv0 "Typing of stratum 1" typing_hasPart stratum1
    ; mk_stratum closure tyenv1 "Typing of stratum 2" typing_hasPartTC stratum2
    ; mk_stratum closure tyenv2 "Typing of stratum 3" typing_query stratum3
    ; mk_program closure tyenv0 "Typing of bike shop program" tyenv3 prog
    ]
  ;;
end

module SocialInsurance = struct
  open Stratified.SocialInsurance

  let test_cases =
    [ mk_stratum trg tyenv0 "Typing of stratum 1" typing_employee stratum1
    ; mk_stratum trg tyenv1 "Typing of stratum 2" typing_salary stratum2
    ; mk_stratum trg tyenv2 "Typing of stratum 3" typing_socins stratum3
    ; mk_stratum trg tyenv3 "Typing of stratum 4" typing_query stratum4
    ; mk_program trg tyenv0 "Typing of social insurance program" tyenv4 prog
    ]
  ;;
end

(* -- All cases ------------------------------------------------------------- *)

let test_cases = BikeShop.test_cases @ SocialInsurance.test_cases
