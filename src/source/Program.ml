open Core_kernel
open Lib

type t = { stmts : Statement.t list }

(* -- Translate to `Core` representation ------------------------------------ *)
let normalize { stmts } =
  { stmts = List.concat_map ~f:Statement.normalize stmts }
;;

type elems =
  { clauses : Core.Clause.Raw.t list
  ; knowledge : Core.Knowledge.t list
  ; tydefns : (Core.Name.t * Core.Ty.t) list
  ; inputs : (Core.Name.t * Core.Typing.t) list
  ; params : (Core.Name.t * Core.Ty.t) list
  ; exports : Core.Name.t list
  }

let empty =
  { clauses = []
  ; knowledge = []
  ; tydefns = []
  ; inputs = []
  ; params = []
  ; exports = []
  }
;;

let collect_reprs { stmts } =
  let rec aux accu = function
    | next :: rest ->
      Result.(
        Statement.to_core next
        >>= (function
        | Statement.RCls cl ->
          aux { accu with clauses = cl :: accu.clauses } rest
        | RKnw fct -> aux { accu with knowledge = fct :: accu.knowledge } rest
        | RTy (nm, def) ->
          aux { accu with tydefns = (nm, def) :: accu.tydefns } rest
        | RExport pred -> aux { accu with exports = pred :: accu.exports } rest
        | RData (pred, typing) ->
          aux { accu with inputs = (pred, typing) :: accu.inputs } rest
        | RParam (param, ty) ->
          aux { accu with params = (param, ty) :: accu.params } rest))
    | [] -> Ok accu
  in
  aux empty stmts
;;

let mk_prog _ =
  (* { clauses ; exports; params; inputs ; knowledge; tydefns } = *)
  failwith ""
;;

let to_core prg = Result.map ~f:mk_prog @@ collect_reprs @@ normalize prg

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end)
