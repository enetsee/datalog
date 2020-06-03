open Core_kernel
open Lib
open Reporting
module I = Parser.MenhirInterpreter
module IE = MenhirLib.IncrementalEngine

module Err = struct
  type t =
    | BadState
    | Msg of string option * Region.t
    | NotFound of string

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp ppf = function
      | Msg (Some msg, region) ->
        Fmt.(pair ~sep:sp (any "Parse error: " ++ string) (parens Region.pp))
          ppf
          (msg, region)
      | Msg (_, region) ->
        Fmt.(any "Parse error without message " ++ Region.pp) ppf region
      | BadState -> Fmt.string ppf "Parsing failed in a bad state"
      | NotFound path ->
        Fmt.(prefix (any "Path not found: ") @@ quote ~mark:"'" string) ppf path
    ;;

    let pp = `NoPrec pp
  end)
end

let succeed defn = Ok defn

let from_menhir_pos (pos : MenhirLib.IncrementalEngine.position) =
  let line = pos.Lexing.pos_lnum
  and col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Reporting.Position.{ line; col }
;;

let error_msg state =
  try
    let msg = Parsing_errors.message @@ I.number state in
    Some msg
  with
  | _ -> None
;;

let fail _ = function
  | I.HandlingError env ->
    (match I.stack env with
    | (lazy Nil) -> Result.Error Err.BadState
    | (lazy (Cons (I.Element (state, _, start_pos, end_pos), _))) ->
      let start_ = from_menhir_pos start_pos
      and end_ = from_menhir_pos end_pos in
      let msg_opt = error_msg state
      and region = Reporting.Region.{ start_; end_ } in
      Result.Error (Err.Msg (msg_opt, region)))
  | _ -> assert false
;;

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result
;;

let parse string =
  let lexbuf = Lexing.from_string string in
  loop lexbuf @@ Parser.Incremental.program lexbuf.lex_curr_p
;;

let parse_file path =
  try
    let chan = In_channel.create path in
    let lexbuf = Lexing.from_channel chan in
    loop lexbuf @@ Parser.Incremental.program lexbuf.lex_curr_p
  with
  | _ -> Error (Err.NotFound path)
;;
