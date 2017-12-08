type t =
  | EOF
  | IDENT of string
  | CHAR of char
  | STRING of string
  | DEF
  | OR
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | DOT

let to_string = function
  | EOF -> "<EOF>"
  | IDENT s -> s
  | CHAR c -> Printf.sprintf "'%c'" c
  | STRING s -> "\"" ^ s ^ "\""
  | DEF -> "="
  | OR -> "|"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACK -> "["
  | RBRACK -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | DOT -> "."

let of_char = function
  | '=' -> DEF
  | '|' -> OR
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '[' -> LBRACK
  | ']' -> RBRACK
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '.' -> DOT
  | _ -> assert false
