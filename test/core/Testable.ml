open Core
open Core_kernel

module type Testable = sig
  type t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

let raw_program = Program.Raw.(Alcotest.testable pp equal)
let adorned_program = Program.Adorned.(Alcotest.testable pp equal)
let stratified_program = Program.Stratified.(Alcotest.testable pp equal)
let err = MonadCompile.Err.(Alcotest.testable pp equal)
let violation = Violation.(Alcotest.testable pp equal)
let kb = Knowledge.Base.(Alcotest.testable pp equal)

let tuple2
    (type a b)
    (module Fst : Testable with type t = a)
    (module Snd : Testable with type t = b)
  =
  Alcotest.testable
    Fmt.(hovbox @@ pair ~sep:sp Fst.pp Snd.pp)
    Tuple2.(equal ~eq1:Fst.equal ~eq2:Snd.equal)
;;
