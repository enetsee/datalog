open Core_kernel
open Lib

type t = { stmts : Statement.t list }

let mapM ~f { stmts } =
  MonadCompile.(map ~f:(fun stmts -> { stmts }) @@ all @@ List.map ~f stmts)
;;

(* -- Pretty implementation ----------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end)
