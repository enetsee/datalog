{
  open Parser
  open Lexing
  exception UnexpectedChar of string
  exception UnterminatedComment  

  let current_loc = 
    let prev_pos = ref Reporting.Position.empty in 
    fun lexbuf ->
    let p = Lexing.lexeme_start_p lexbuf in
    let new_pos = 
      Reporting.Position.(
        { line = p.Lexing.pos_lnum
        ; col  = p.Lexing.pos_cnum - p.Lexing.pos_bol
        } 
      )
    in 
    let region = Reporting.Region.({ start_ = !prev_pos; end_ = new_pos}) in 
    prev_pos := new_pos;
    region
  ;;
  

  let wildcard_name str =      
    let tmp = Core_kernel.String.chop_prefix_exn ~prefix:"_" str in 
    if tmp = "" then None else Some tmp  
  ;;


}

(* -- Helpers --------------------------------------------------------------- *)
let idchar = ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']
let symbol_lit = ['A'-'Z'] idchar*   
let name = ['a'-'z'] idchar* 

let var = '?' name
let param = '#' name
let tyname = '@' name
let wild = '_' name?


let nonzero_digit = ['1'-'9']
let digit =  ['0'-'9']
let digits = digit+
let pos_int_lit = nonzero_digit digit* 
let int_lit  = pos_int_lit | '-' pos_int_lit | '0'
let exp_lit = ['e' 'E'] ['+' '-']? digits
let real_lit1 = digits '.' digits? exp_lit?
let real_lit2 = '.' digits exp_lit?
let real_lit3 = digits exp_lit
let real_lit = real_lit1 | real_lit2 | real_lit3
let whitespace = [' ' '\t'  '\011' '\012' '\r']


let newline = '\n'

(* -- Rules ----------------------------------------------------------------- *)

rule token = parse
  
  (* Whitespace *)
  | newline                 { new_line lexbuf; 
                              let _ : Reporting.Region.t = current_loc lexbuf in 
                              token lexbuf 
                            }
  | whitespace +            { let _ : Reporting.Region.t = current_loc lexbuf in
                              token lexbuf 
                            }
  | "/*"                    { multiline_comment lexbuf;
                              let _ : Reporting.Region.t = current_loc lexbuf in
                              token lexbuf 
                            }
  | "//"                    { singleline_comment lexbuf;
                              let _ : Reporting.Region.t = current_loc lexbuf in  
                              token lexbuf 
                            }
  
  (* Punctuation *)
  | '('                     { LPAREN (current_loc lexbuf) }
  | ')'                     { RPAREN (current_loc lexbuf) }
  | ','                     { COMMA (current_loc lexbuf) }    
  | ';'                     { SEMICOLON (current_loc lexbuf) }
  | ':'                     { COLON (current_loc lexbuf) }
  | "not"                   { BANG (current_loc lexbuf) }
  | '.'                     { DOT (current_loc lexbuf) }
  | ":-"                    { IMPL (current_loc lexbuf) }  
  

  (* Keywords *)

  | "extends"               { EXTENDS (current_loc lexbuf)}
  | "export"                { EXPORT (current_loc lexbuf)}
  | "type"                  { TYPE (current_loc lexbuf) }
  | "data"                  { DATA (current_loc lexbuf) }
  | "param"                 { PARAM (current_loc lexbuf) }


  (* Built-in type names *)
  | "symbol"                { TYSYMBOL (current_loc lexbuf) }
  | "real"                  { TYREAL (current_loc lexbuf) }
  | "int"                   { TYINT (current_loc lexbuf) }
  | "bool"                  { TYBOOL (current_loc lexbuf) }
  | "date"                  { TYDATE (current_loc lexbuf) }
  | "span"                  { TYDATE (current_loc lexbuf) }



  (* Identifiers and literals
    TODO: dates
  *)
  | "true"                  { BOOLLIT (true,current_loc lexbuf) }
  | "false"                 { BOOLLIT (false,current_loc lexbuf) }
  | int_lit                 { INTLIT (int_of_string @@ lexeme lexbuf, current_loc lexbuf) }
  | real_lit                { REALLIT (lexeme lexbuf, current_loc lexbuf) }  
  | symbol_lit              { SYMLIT (lexeme lexbuf,current_loc lexbuf) }

  | wild                    { WILDCARD (wildcard_name @@ lexeme lexbuf  , current_loc lexbuf) }
  | var                     { VAR (Core_kernel.String.chop_prefix_exn ~prefix:"?" @@ lexeme lexbuf , current_loc lexbuf) }  
  | param                   { PARAMNAME (Core_kernel.String.chop_prefix_exn ~prefix:"#" @@  lexeme lexbuf , current_loc lexbuf) }
  | tyname                  { TYNAME (Core_kernel.String.chop_prefix_exn ~prefix:"@" @@ lexeme lexbuf,current_loc lexbuf) }    

  | name                    { NAME (lexeme lexbuf,current_loc lexbuf) }    

  | eof                     { Parser.EOF }
  | _ 
    { raise (UnexpectedChar (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) 
    }

and multiline_comment = parse
  | "*/"                    { () }
  | eof                     { raise UnterminatedComment }
  | '\n'                    { new_line lexbuf; multiline_comment lexbuf }
  | _                       { multiline_comment lexbuf }

and singleline_comment = parse
  | '\n'                    { new_line lexbuf }
  | eof                     { () }
  | _                       { singleline_comment lexbuf }