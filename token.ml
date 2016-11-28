type token =
  | EOF
  | NEWLINE
  | ID of string
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

type position = int * int

type token_with_pos = token * position

exception SyntaxError of position * string

let to_string token : string =
  match token with
  | EOF -> "<EOF>"
  | NEWLINE -> "<NEWLINE>"
  | ID s -> s
  | CHAR c -> Printf.sprintf "'%c'" c
  | STRING s -> "\"" ^ s ^ "\""
  | DEF -> "::="
  | OR -> "|"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACK -> "["
  | RBRACK -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"

let syntax_error_to_string (position, msg) : string =
  let line, col = position in
  Printf.sprintf "%d:%d: %s" line col msg

let check_newline token_p : unit =
  let token, pos = token_p in
  match token with
  | NEWLINE -> ()
  | _ -> raise (SyntaxError (pos, "new-line expected"))

let check_id token_p : string =
  let token, pos = token_p in
  match token with
  | ID s -> s
  | _ -> raise (SyntaxError (pos, "identifier expected"))

let check_def token_p : unit =
  let token, pos = token_p in
  match token with
  | DEF -> ()
  | _ -> raise (SyntaxError (pos, "‘::=’ expected"))

let check_or token_p : unit =
  let token, pos = token_p in
  match token with
  | OR -> ()
  | _ -> raise (SyntaxError (pos, "‘|’ expected"))

let check_or token_p : unit =
  let token, pos = token_p in
  match token with
  | OR -> ()
  | _ -> raise (SyntaxError (pos, "‘|’ expected"))

let check_rparen token_p : unit =
  let token, pos = token_p in
  match token with
  | RPAREN -> ()
  | _ -> raise (SyntaxError (pos, "‘)’ expected"))

let check_rbrack token_p : unit =
  let token, pos = token_p in
  match token with
  | RBRACK -> ()
  | _ -> raise (SyntaxError (pos, "‘]’ expected"))

let check_rbrace token_p : unit =
  let token, pos = token_p in
  match token with
  | RBRACE -> ()
  | _ -> raise (SyntaxError (pos, "‘}’ expected"))
