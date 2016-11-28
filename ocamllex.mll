{
open Lexing
open Token

let convert_position pos : int * int =
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule read = parse
  | [' ' '\t']+ { read lexbuf }
  (* single-line comment *)
  | '#' [^ '\n']+ { read lexbuf }
  | '\n' { new_line lexbuf; (NEWLINE, convert_position lexbuf.lex_start_p) }
  | id as s { (ID s, convert_position lexbuf.lex_start_p) }
  | '\'' (_ as c) '\'' { (CHAR c, convert_position lexbuf.lex_start_p) }
  | '"' { (read_string (Buffer.create 17) lexbuf, convert_position lexbuf.lex_start_p) }
  | "::=" { (DEF, convert_position lexbuf.lex_start_p) }
  | '|' { (OR, convert_position lexbuf.lex_start_p) }
  | '(' { (LPAREN, convert_position lexbuf.lex_start_p) }
  | ')' { (RPAREN, convert_position lexbuf.lex_start_p) }
  | '[' { (LBRACK, convert_position lexbuf.lex_start_p) }
  | ']' { (RBRACK, convert_position lexbuf.lex_start_p) }
  | '{' { (LBRACE, convert_position lexbuf.lex_start_p) }
  | '}' { (RBRACE, convert_position lexbuf.lex_start_p) }
  | _ { raise (SyntaxError (convert_position lexbuf.lex_start_p, "Unexpected character: " ^ lexeme lexbuf)) }
  | eof { (EOF, convert_position lexbuf.lex_start_p) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | [^ '"']+ { Buffer.add_string buf (lexeme lexbuf); read_string buf lexbuf }
  | eof { raise (SyntaxError (convert_position lexbuf.lex_start_p, "Unexpected EOF while reading string")) }
