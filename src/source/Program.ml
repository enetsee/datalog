open Core_kernel
open Lib

type t = { stmts : Statement.t list }

let mapM ~f { stmts } =
  MonadCompile.(map ~f:(fun stmts -> { stmts }) @@ all @@ List.map ~f stmts)
;;

(* -- Compilation --------------------------------------------------------------

  Generate head for anonymous queries


*)
let generate_query_head t = mapM ~f:Statement.generate_query_head t
let set_nature t = mapM ~f:Statement.set_nature t

let normalize { stmts } =
  { stmts = List.concat_map ~f:Statement.normalize stmts }
;;

let collect_reprs rs =
  let rec aux cls qrys fcts = function
    | [] -> cls, qrys, fcts
    | Statement.RCls cl :: rest -> aux (cl :: cls) qrys fcts rest
    | RQry qry :: rest -> aux cls (qry :: qrys) fcts rest
    | RKnw fct :: rest -> aux cls qrys (fct :: fcts) rest
    | RDcl :: rest -> aux cls qrys fcts rest
  in
  aux [] [] [] rs
;;

let mk_prog reprs =
  MonadCompile.(
    constraints
    >>= fun cnstrts ->
    let cls, qrys, fcts = collect_reprs reprs in
    let queries =
      List.map ~f:(fun Core.Raw.Clause.{ head = { pred; _ }; _ } -> pred) qrys
    and clauses = cls @ qrys in
    let prog = Core.Raw.Program.{ clauses; queries; cnstrts }, fcts in
    if List.is_empty queries
    then warn Warn.NoQueries >>= fun _ -> return prog
    else return prog)
;;

let to_core t =
  MonadCompile.(
    generate_query_head t
    >>= set_nature
    >>= fun t ->
    let { stmts } = normalize t in
    List.map ~f:Statement.to_core stmts |> all >>= mk_prog)
;;

(* -- Pretty implementation ----------------------------------------------- *)

include Pretty.Make0 (struct
  type nonrec t = t

  let pp ppf { stmts } = Fmt.(vbox @@ list ~sep:cut Statement.pp) ppf stmts
  let pp = `NoPrec pp
end)
