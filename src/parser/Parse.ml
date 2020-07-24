open Core_kernel
open Lib
open Reporting
module I = Program.MenhirInterpreter
module IE = MenhirLib.IncrementalEngine

module Make (M : ParseM.S) = struct
  let from_menhir_pos (pos : MenhirLib.IncrementalEngine.position) =
    let line = pos.Lexing.pos_lnum
    and col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
    Reporting.Position.{ line; col }
  ;;

  let error_msg state =
    try
      let msg = Program_errors.message @@ I.number state in
      Some msg
    with
    | _ -> None
  ;;

  let fail _ = function
    | I.HandlingError env ->
      (match I.stack env with
      | (lazy Nil) -> M.err_bad_parse_state ()
      | (lazy (Cons (I.Element (state, _, start_pos, end_pos), _))) ->
        let start_ = from_menhir_pos start_pos
        and end_ = from_menhir_pos end_pos in
        let msg = error_msg state
        and region = Reporting.Region.{ start_; end_ } in
        M.err_parse_err ~msg ~region)
    | _ -> assert false
  ;;

  let loop lexbuf result =
    let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    I.loop_handle M.return (fail lexbuf) supplier result
  ;;

  let parse_string string =
    let lexbuf = Lexing.from_string string in
    loop lexbuf @@ Program.Incremental.program lexbuf.lex_curr_p
  ;;

  let parse_file path =
    try
      In_channel.with_file path ~f:(fun chan ->
          let lexbuf = Lexing.from_channel chan in
          loop lexbuf @@ Program.Incremental.program lexbuf.lex_curr_p)
    with
    | _ -> M.err_file_not_found path
  ;;
end
