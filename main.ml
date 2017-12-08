open Format
open Lex

let () =
  let grammar =
    try
      let lexer = Lexer.create stdin in
      Parser.parse lexer
    with Syntax_error err ->
      print_string (format_syntax_error err);
      print_newline ();
      exit 1
  in
  fprintf std_formatter "%a@." Ast.pp_grammar grammar;
  let analyzed_grammar = Algorithm.analyze grammar in
  grammar |> List.iteri (fun i (name, _) ->
    fprintf std_formatter "%s: %b\n" name analyzed_grammar.nullable.(i))
