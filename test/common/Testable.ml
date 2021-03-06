open Core_kernel

module type Testable = sig
  type t

  val equal : t -> t -> bool
  val pp : t Fmt.t
end

module MkList (T : Testable) : Testable = struct
  type t = T.t list

  let equal = List.equal T.equal
  let pp = Fmt.(hovbox @@ brackets @@ list ~sep:comma T.pp)
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

let binding = Core.Binding.(Alcotest.testable pp equal)
let raw_clause = Core.Clause.Raw.(Alcotest.testable pp equal)
let adorned_clause = Core.Clause.Adorned.(Alcotest.testable pp equal)

let strata =
  let pp = Fmt.(vbox @@ list ~sep:cut @@ list ~sep:cut Core.Clause.Adorned.pp)
  and eq xs ys =
    let xs' = List.map ~f:(List.sort ~compare:Core.Clause.Adorned.compare) xs
    and ys' = List.map ~f:(List.sort ~compare:Core.Clause.Adorned.compare) ys in
    List.equal (List.equal Core.Clause.Adorned.equal) xs' ys'
  in
  Alcotest.testable pp eq
;;

let cycles =
  let pp =
    Fmt.(
      hbox
      @@ list ~sep:comma
      @@ parens
      @@ pair ~sep:comma Core.Pred.pp Core.Pred.pp)
  and eq xs ys =
    let xs' =
      List.sort
        ~compare:
          Tuple2.(compare ~cmp1:Core.Pred.compare ~cmp2:Core.Pred.compare)
        xs
    and ys' =
      List.sort
        ~compare:
          Tuple2.(compare ~cmp1:Core.Pred.compare ~cmp2:Core.Pred.compare)
        ys
    in
    List.equal Tuple2.(equal ~eq1:Core.Pred.equal ~eq2:Core.Pred.equal) xs' ys'
  in
  Alcotest.testable pp eq
;;

let raw_program = Core.Program.Raw.(Alcotest.testable pp equal)
let adorned_program = Core.Program.Adorned.(Alcotest.testable pp equal)
let stratified_program = Core.Program.Stratified.(Alcotest.testable pp equal)
let violation = Dataflow.Violation.(Alcotest.testable pp equal)
let kb = Core.Knowledge.Base.(Alcotest.testable pp equal)
let partition = Partition.(Alcotest.testable pp equal)
let ty = Type.Ty.(Alcotest.testable pp equal)
let tyset = Type.Ty.Set.(Alcotest.testable (pp_set Type.Ty.pp) equal)
let cnstr = Constraint.(Alcotest.testable pp equal)
let schedule = Schedule.Graph.(Alcotest.testable pp equal)

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

let trg : Type.Ty.Set.t Type.Ty.Map.t Alcotest.testable =
  Type.Ty.Map.(
    Alcotest.testable (pp_map Type.Ty.pp @@ pp_set Type.Ty.pp)
    @@ equal Type.Ty.Set.equal)
;;

let ttc = Type.TTC.(Alcotest.testable pp equal)
let typing = Type.Typing.(Alcotest.testable pp equal)
let relation = Typecheck.Relation.Algebra.(Alcotest.testable pp equal)
let typing_env = Typecheck.TypingEnv.(Alcotest.testable pp equal)
