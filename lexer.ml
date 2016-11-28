open Lexing

type t = {
  lexbuf : lexbuf;
  mutable next_token_with_pos : Token.token_with_pos;
}

let create channel : t =
  let lexbuf = from_channel channel in
  { lexbuf; next_token_with_pos = Ocamllex.read lexbuf }

let peek lexer : Token.token_with_pos = lexer.next_token_with_pos

let lex lexer : unit =
  lexer.next_token_with_pos <- Ocamllex.read lexer.lexbuf

let next lexer : Token.token_with_pos =
  let token_with_pos = lexer.next_token_with_pos in
  lex lexer;
  token_with_pos
