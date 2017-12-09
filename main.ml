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
    fprintf std_formatter "%s: %b\n" name analyzed_grammar.nullable.(i));
  analyzed_grammar.first_set_array |>
  Array.iteri (fun nt_i first_set ->
    fprintf std_formatter "%s:" analyzed_grammar.nonterminal_array.(nt_i);
    first_set |>
    List.iter (fun t_i ->
      fprintf std_formatter " %s" analyzed_grammar.terminal_array.(t_i));
    fprintf std_formatter "\n")
