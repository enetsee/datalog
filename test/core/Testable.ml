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

let tuple2
    (type a b)
    (module Fst : Testable with type t = a)
    (module Snd : Testable with type t = b)
  =
  Alcotest.testable
    Fmt.(hovbox @@ pair ~sep:sp Fst.pp Snd.pp)
    Tuple2.(equal ~eq1:Fst.equal ~eq2:Snd.equal)
;;

let map
    (type a b)
    (module Key : Testable with type t = a)
    (module Val : Testable with type t = b)
  =
  let pp ppf m =
    Fmt.(
      vbox @@ list ~sep:cut @@ hvbox @@ pair ~sep:(any "@;=>@;") Key.pp Val.pp)
      ppf
    @@ Map.to_alist m
  and equal m1 m2 = Map.equal Val.equal m1 m2 in
  Alcotest.testable pp equal
;;

let raw_program = Core.Program.Raw.(Alcotest.testable pp equal)
let adorned_program = Core.Program.Adorned.(Alcotest.testable pp equal)
let stratified_program = Core.Program.Stratified.(Alcotest.testable pp equal)
let err = Core.MonadCompile.Err.(Alcotest.testable pp equal)
let violation = Core.Violation.(Alcotest.testable pp equal)
let kb = Core.Knowledge.Base.(Alcotest.testable pp equal)
let partition = Core.Partition.(Alcotest.testable pp equal)
let ty = Core.Ty.(Alcotest.testable pp equal)
let tyset = Core.Ty.Set.(Alcotest.testable (pp_set Core.Ty.pp) equal)
let cnstr = Core.Constraint.(Alcotest.testable pp equal)
let schedule = Core.Schedule.(Alcotest.testable pp equal)

let orderings =
  let pp =
    Fmt.(
      vbox
      @@ list ~sep:cut
      @@ hbox
      @@ brackets
      @@ list ~sep:comma Core.Lit.Raw.pp)
  (* sort the order of the orderings but not the orderings! *)
  and eq xxs yys =
    let xxs' = List.sort ~compare:(List.compare Core.Lit.Raw.compare) xxs
    and yys' = List.sort ~compare:(List.compare Core.Lit.Raw.compare) yys in
    List.equal (List.equal Core.Lit.Raw.equal) xxs' yys'
  in
  Alcotest.testable pp eq
;;

let trg : Core.Ty.Set.t Core.Ty.Map.t Alcotest.testable =
  Core.Ty.Map.(
    Alcotest.testable (pp_map Core.Ty.pp @@ pp_set Core.Ty.pp)
    @@ equal Core.Ty.Set.equal)
;;

let ttc = Core.TTC.(Alcotest.testable pp equal)
let typing = Core.Typing.(Alcotest.testable pp equal)
let relation = Core.Relation.(Alcotest.testable pp equal)
