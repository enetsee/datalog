open Core_kernel
module I = Parser.MenhirInterpreter
module IE = MenhirLib.IncrementalEngine

type error =
  | Empty
  | Message of string option * Reporting.Region.t

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
    let msg =
      match I.stack env with
      | (lazy Nil) -> Empty
      | (lazy (Cons (I.Element (state, _, start_pos, end_pos), _))) ->
        let start_ = from_menhir_pos start_pos
        and end_ = from_menhir_pos end_pos in
        let msg_opt = error_msg state in
        Message (msg_opt, { start_; end_ })
    in
    Error msg
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
  let chan = In_channel.create path in
  let lexbuf = Lexing.from_channel chan in
  loop lexbuf @@ Parser.Incremental.program lexbuf.lex_curr_p
;;
