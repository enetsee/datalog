open Core
open Core_kernel

module Fail = struct
  include Effect.MonadFail.Make (Binding)

  let equal eq_a = Result.equal eq_a Binding.equal
  let pp pp_a = Fmt.(result ~ok:pp_a ~error:Binding.pp)
end

module M = struct
  include Effect.StateT.Make_with_state (TypingEnv) (Fail)

  let get_pred_constraint name =
    get
    >>= fun tyenv ->
    return
    @@ Option.value ~default:Constraint.trivial
    @@ TypingEnv.find_pred_constraint ~name tyenv
  ;;

  let set_pred_constraint name cstr =
    get
    >>= fun tyenv ->
    put @@ TypingEnv.update_pred_constraint_exn tyenv ~name ~cstr
  ;;

  let get_pred_effects name =
    get
    >>= fun tyenv ->
    return
    @@ Option.value ~default:Eff.Set.empty
    @@ TypingEnv.find_pred_effects ~name tyenv
  ;;

  let err_no_ordering binding _ = lift @@ Fail.fail binding
end

module AdornM = Adorn.Make (M)

let output = Alcotest.(result Testable.adorned_program Testable.binding)

let mk_ok msg expect tyenv prg_raw qry =
  let f () =
    Alcotest.(check output)
      msg
      (Ok expect)
      M.(
        eval ~init:tyenv
        @@ map ~f:Program.Adorned.sorted
        @@ AdornM.adorn_program prg_raw qry)
  in
  Alcotest.test_case msg `Quick f
;;

let client_server =
  Programs.ClientServer.(
    mk_ok
      "Client/server generalized adornment example"
      prog_adorned
      tyenv_client_server
      prog_raw
      queries)
;;

let negation =
  Programs.Negation.(
    mk_ok
      "Negation generalized adornment example"
      prog_adorned
      Core.TypingEnv.empty
      prog_raw
      queries)
;;

let complement =
  Programs.Negation.(
    mk_ok
      "Complement of transitive closure"
      prog_adorned
      Core.TypingEnv.empty
      prog_raw
      queries)
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases = [ client_server; negation; complement ]
