open Core_kernel
open Core

type t =
  | GClause of Clause.Raw.t
  | GFact of Knowledge.t

let partition gs =
  let rec aux (cls, fcts) = function
    | [] -> List.rev cls, fcts
    | GClause cl :: rest -> aux (cl :: cls, fcts) rest
    | GFact fct :: rest -> aux (cls, Knowledge.Base.add fcts fct) rest
  in
  aux ([], Knowledge.Base.empty) gs
;;

let mk_body lit var idx =
  let pred = Lit.Raw.pred_of lit in
  let terms =
    List.init pred.arity ~f:(fun i ->
        if i = idx then Term.var' var else Term.wild None)
  in
  [ Lit.Raw.lit pred terms ]
;;

let from_src grdLit grdPred v src =
  match src with
  | Src.SLit (lit, idx) ->
    Some (GClause Clause.Raw.(clause grdLit @@ mk_body lit v idx))
  | SConst (Const.CSym sym) ->
    Some (GFact Knowledge.(knowledge grdPred [ KSymbol sym ]))
  | SConst (Const.CParam nm) ->
    Some (GFact Knowledge.(knowledge grdPred [ KParam nm ]))
  | SConst Const.(CWild _) -> None
;;
