open Core_kernel
open Lib
open Reporting

type t = { stmts : Statement.t list }

(* -- Pretty implementation ------------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end)

(* -- Translate to `Core` representation ------------------------------------ *)
let normalize { stmts } =
  { stmts = List.concat_map ~f:Statement.normalize stmts }
;;

type elems =
  { clauses : Core.Clause.Raw.t list
  ; knowledge : Core.Knowledge.t list
  ; tydefs : (Name.t * Type.Ty.t) list
  ; data : (Name.t * (Name.t * Type.Ty.t) list) list
  ; params : (Name.t * Type.Ty.t) list
  ; exports : Name.t Located.t list
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

module Make (M : SourceM.S) = struct
  module StmtM = Statement.Make (M)

  let collect_reprs { stmts } =
    let rec aux accu = function
      | next :: rest ->
        M.(
          StmtM.to_core next
          >>= (function
          | Statement.RCls cl ->
            aux { accu with clauses = cl :: accu.clauses } rest
          | RKnw fct -> aux { accu with knowledge = fct :: accu.knowledge } rest
          | RTy (nm, def) ->
            aux { accu with tydefs = (nm, def) :: accu.tydefs } rest
          | RExport pred ->
            aux { accu with exports = pred :: accu.exports } rest
          | RData (pred, typing) ->
            aux { accu with data = (pred, typing) :: accu.data } rest
          | RParam (param, ty) ->
            aux { accu with params = (param, ty) :: accu.params } rest))
      | [] -> M.return accu
    in
    aux empty stmts
  ;;

  let mk_module { clauses; exports; params; data; knowledge; tydefs } =
    M.return
      Core.Module.
        { program = Core.Program.Raw.program clauses
        ; knowledge = Core.Knowledge.Base.of_list knowledge
        ; exports
        ; params
        ; data
        ; tydefs
        }
  ;;

  let to_core prg = M.(normalize prg |> collect_reprs >>= mk_module)
end
