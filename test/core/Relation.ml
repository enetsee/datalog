module Env = struct
  type t = Core.Ty.t Core.Name.Map.t

  let empty = Core.Name.Map.empty
end

module Err = struct
  type t = Core.Name.t * Reporting.Region.t

  let equal (nm, _) (nm', _) = Core.Name.equal nm nm'
  let pp = Fmt.(pair ~sep:sp Core.Name.pp Reporting.Region.pp)
end

let testable_err = Err.(Alcotest.testable pp equal)

module Fail = struct
  include Effect.MonadFail.Make (Err)

  let err_unbound_param name region = fail (name, region)
end

module M = struct
  include Effect.ReaderT.Make_with_env (Env) (Fail)

  let err_unbound_param name region = lift @@ Fail.err_unbound_param name region

  let get_param_ty name =
    ask >>= fun env -> return @@ Core.Name.Map.find env name
  ;;
end

module RelM = Core.Relation.Make (M)

let mk_rel_ok name expected env stratum =
  let f () =
    Alcotest.(check @@ result Testable.relation testable_err) name (Ok expected)
    @@ M.run ~env
    @@ RelM.of_clauses stratum
  in
  Alcotest.test_case name `Quick f
;;

let relation_of_hasPartTC =
  mk_rel_ok
    "Relation for recursive clause"
    Programs.BikeShop.rel_hasPartTC
    Env.empty
    Programs.BikeShop.stratum2
;;

let relation_of_socins =
  mk_rel_ok
    "Relation with variable reordering"
    Programs.SocialInsurance.rel_socins
    Env.empty
    Programs.SocialInsurance.stratum3
;;

(* -- All cases ------------------------------------------------------------- *)

let test_cases = [ relation_of_hasPartTC; relation_of_socins ]
