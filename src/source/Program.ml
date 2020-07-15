open Core_kernel
open Lib
open Reporting

type t = { stmts : Statement.t list }

(* -- Translate to `Core` representation ------------------------------------ *)
let normalize { stmts } =
  { stmts = List.concat_map ~f:Statement.normalize stmts }
;;

type elems =
  { clauses : Core.Clause.Raw.t list
  ; knowledge : Core.Knowledge.t list
  ; tydefs : (Core.Name.t * Core.Ty.t) list
  ; data : (Core.Name.t * (Core.Name.t * Core.Ty.t) list) list
  ; params : (Core.Name.t * Core.Ty.t) list
  ; exports : Core.Name.t Located.t list
  }

let empty =
  { clauses = []
  ; knowledge = []
  ; tydefs = []
  ; data = []
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
          aux { accu with tydefs = (nm, def) :: accu.tydefs } rest
        | RExport pred -> aux { accu with exports = pred :: accu.exports } rest
        | RData (pred, typing) ->
          aux { accu with data = (pred, typing) :: accu.data } rest
        | RParam (param, ty) ->
          aux { accu with params = (param, ty) :: accu.params } rest))
    | [] -> Ok accu
  in
  aux empty stmts
;;

let mk_module { clauses; exports; params; data; knowledge; tydefs } =
  Core.Module.
    { program = Core.Program.Raw.program clauses
    ; knowledge = Core.Knowledge.Base.of_list knowledge
    ; exports
    ; params
    ; data
    ; tydefs
    }
;;

let to_core prg = Result.map ~f:mk_module @@ collect_reprs @@ normalize prg

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end)
