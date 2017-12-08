open Lex
open Lexing

type t = {
  lexbuf : lexbuf;
  mutable curtok : Token.t * Lex.position;
}

let create channel =
  let lexbuf = from_channel channel in
  { lexbuf; curtok = Ocamllex.read lexbuf }

let peek lexer = lexer.curtok

let getsym lexer =
  lexer.curtok <- Ocamllex.read lexer.lexbuf
