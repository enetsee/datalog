open Core_kernel

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
let raw str = Parse.parse str
let foreignFuncEmbed str = Logger.(raw str >>= Program.set_foreign_func ~ffns)
let namedQueries str = Logger.(foreignFuncEmbed str >>= Program.name_query)
let normalized str = Logger.(namedQueries str >>= Program.normalize)
let to_core str = Logger.(normalized str >>= Program.to_core)
let run t = Logger.run ~prefix:(Some "q") reserved t

let print_result = function
  | Ok (prog, warnings) ->
    Fmt.(pair ~sep:cut Program.pp @@ list ~sep:cut Logger.Warning.pp)
      Format.std_formatter
      (prog, warnings)
  | Error err -> Logger.Err.pp Format.std_formatter err
;;

let test =
  "\n\
   edge(A,B).\n\
   edge(B,C).\n\
   edge(A,D).\n\
   edge(B,D).\n\n\
   connected(?X,?Y) \n\
  \  :- edge(?X,?Y) \n\
  \  ;  edge(?X,?Z) , connected(?Z,?Y). \n\n\
  \  complement(?X,?Y) :- !connected(?X,?Y).\n\
  \  ?- complement(A,?X)."
;;

let test_bad =
  "\n\
   edge(A,B)\n\
   connected(?X,?Y) := edge(?X,?Y) ; edge(?X,?Z), connected(?Z,?Y).\n"
;;
