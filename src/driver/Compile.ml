open Parser
module P = Parse.Make (CompileM)
module S = Source.Program.Make (CompileM)
module C = Core.Compile.Make (CompileM)

let compile file =
  try
    let res, warn, _ =
      CompileM.(
        run ~env:Env.default ~st:State.default
        @@ (P.parse_file file >>= S.to_core >>= C.to_stratified))
    in
    match res with
    | Ok (prog, kb, env) -> Ok (prog, kb, env, Topic.to_string warn)
    | Error err -> Error (Err.to_string err)
  with
  | exn -> Error (Printexc.to_string exn)
;;
