open Core_kernel
module I = Parser.MenhirInterpreter
module IE = MenhirLib.IncrementalEngine
open Lib

(* -- Parsing --------------------------------------------------------------- *)

let succeed defn = Logger.return defn

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
    | (lazy Nil) -> Logger.(fail parse_bad_state)
    | (lazy (Cons (I.Element (state, _, start_pos, end_pos), _))) ->
      let start_ = from_menhir_pos start_pos
      and end_ = from_menhir_pos end_pos in
      let msg_opt = error_msg state
      and region = Reporting.Region.{ start_; end_ } in
      Logger.(fail @@ parse_error msg_opt region)
    | _ -> assert false)
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

(* -- Translate `Program` to `Core` language ----------------------------------- 

  The frontend is responsible for 
  1) Parsing input 
  2) Modifying any atoms with extra-logical predicate symbols 
  3) Naming queries 
  4) Normalizing sentences

  For step 4, we push negation towards atoms, pull disjunction outwards
  and then recursively split on disjunction so all sentences are just
  conjunctions.

  `Core` is then responsible for dataflow analysis, moding, stratification
  and evaluation.
*)

let ffns = Core.PredSymbol.Map.empty
let reserved = String.Set.empty

let raw path_or_str =
  match path_or_str with
  | `Path path -> parse_file path
  | `String str -> parse str
;;

let foreignFuncEmbed str = Logger.(raw str >>= Program.set_foreign_func ~ffns)
let namedQueries str = Logger.(foreignFuncEmbed str >>= Program.name_query)
let normalized str = Logger.(namedQueries str >>= Program.normalize)
let toCore str = Logger.(normalized str >>= Program.to_core)

let run t =
  Logger.run ~init:State.(init (Some "q") reserved) t
  |> Result.map ~f:(fun (r, State.{ warnings }) -> r, warnings)
;;

let print_result = function
  | Ok (prog, warnings) ->
    Fmt.(vbox @@ pair ~sep:cut Program.pp @@ list ~sep:cut Warning.pp)
      Format.std_formatter
      (prog, warnings)
  | Error err -> Err.pp Format.std_formatter err
;;

let print_core_result = function
  | Ok ((prog, edb), warnings) ->
    Fmt.(
      vbox
      @@ pair
           ~sep:cut
           (vbox
           @@ pair
                ~sep:cut
                (Core.Program.Unstratified.pp (fun _ _ -> ()))
                (list ~sep:cut @@ Core.Knowledge.pp @@ fun _ _ -> ()))
      @@ list ~sep:cut Warning.pp)
      Format.std_formatter
      ((prog, edb), warnings)
  | Error err -> Err.pp Format.std_formatter err
;;
