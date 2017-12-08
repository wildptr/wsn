open Format
open Lex

let () =
  try
    let lexer = Lexer.create stdin in
    let grammar = Parser.parse lexer in
    fprintf std_formatter "%a@." Ast.pp_grammar grammar
  with Syntax_error err ->
    print_string (format_syntax_error err);
    print_newline ()
