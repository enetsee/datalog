open Core
open Core_kernel

module type Testable = sig
  type t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

let pp_set pp ppf t =
  Fmt.(braces @@ hovbox @@ list ~sep:comma pp) ppf @@ Set.elements t
;;

let pp_map ppk ppv ppf t =
  Fmt.(vbox @@ list ~sep:cut @@ pair ~sep:(any " => ") ppk ppv) ppf
  @@ Map.to_alist t
;;

let raw_program = Program.Raw.(Alcotest.testable pp equal)
let adorned_program = Program.Adorned.(Alcotest.testable pp equal)
let stratified_program = Program.Stratified.(Alcotest.testable pp equal)
let err = MonadCompile.Err.(Alcotest.testable pp equal)
let violation = Violation.(Alcotest.testable pp equal)
let kb = Knowledge.Base.(Alcotest.testable pp equal)
let partition = Partition.(Alcotest.testable pp equal)
let ty = Ty.(Alcotest.testable pp equal)
let tyset = Ty.Set.(Alcotest.testable (pp_set Ty.pp) equal)

let trg : Ty.Set.t Ty.Map.t Alcotest.testable =
  Ty.Map.(
    Alcotest.testable (pp_map Ty.pp @@ pp_set Ty.pp) @@ equal Ty.Set.equal)
;;

let ttc = TTC.(Alcotest.testable pp equal)
let typing = Typing.(Alcotest.testable pp equal)

let tuple2
    (type a b)
    (module Fst : Testable with type t = a)
    (module Snd : Testable with type t = b)
  =
  Alcotest.testable
    Fmt.(hovbox @@ pair ~sep:sp Fst.pp Snd.pp)
    Tuple2.(equal ~eq1:Fst.equal ~eq2:Snd.equal)
;;
