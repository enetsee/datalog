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
    
}

(* -- Helpers --------------------------------------------------------------- *)
let idchar = ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']
let symbol_lit = ['A'-'Z'] idchar*  
let var = '?' symbol_lit
let wild = '?' '_' symbol_lit
let predym = ['a'-'z'] idchar* 


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
  | '!'                     { BANG (current_loc lexbuf) }
  | '.'                     { DOT (current_loc lexbuf) }
  | ":-"                    { IMPL (current_loc lexbuf) }  
  | "?-"                    { QRY (current_loc lexbuf) }
  
  

  (* Operators *)
  
  (* | '-'                     { MINUS (current_loc lexbuf) }  
  | '*'                     { STAR (current_loc lexbuf) }
  | '+'                     { PLUS (current_loc lexbuf) }
  | '/'                     { DIV (current_loc lexbuf) } 
  | '='                     { EQ (current_loc lexbuf) }   
  | "/="                    { NEQ (current_loc lexbuf) }  
  | "<"                     { LT (current_loc lexbuf) }  
  | ">"                     { GT (current_loc lexbuf) }
  | "<="                    { LTE (current_loc lexbuf) }  
  | ">="                    { GTE (current_loc lexbuf) }    
   *)

  (* Identifiers and literals *)
  | int_lit                 { INTLIT (int_of_string @@ lexeme lexbuf, current_loc lexbuf) }
  | real_lit                { REALLIT (lexeme lexbuf, current_loc lexbuf) }
  | "true"                  { BOOLLIT (true,current_loc lexbuf) }
  | "false"                 { BOOLLIT (false,current_loc lexbuf) }
  | symbol_lit              { SYMLIT (lexeme lexbuf,current_loc lexbuf) }
  | var                     { VAR (lexeme lexbuf,current_loc lexbuf) }
  | wild                    { WILDCARD (current_loc lexbuf) }
  | predym                  { PREDSYM(lexeme lexbuf,current_loc lexbuf) }
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